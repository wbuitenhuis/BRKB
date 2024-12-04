# https://www.sec.gov/data-research/sec-markets-data/form-n-port-data-sets
# quarterly sec filings with holdings from all S mutual funds
check_elementnames <- function(xbrl, fix = FALSE){
  
  sus_elements <- function(element_names){
    # sus element if:
    # 1.) last character is a digit
    # 2.) element without last character does not exists
    element_names <- element_names |> unique() 
    ind <- last_char_is_num(element_names)
    sus_elements <- element_names[ind]
    alt_elements <- stringr::str_sub(sus_elements, end = -2)
    ind <- alt_elements %in% element_names
    sus_elements <- sus_elements[!ind]
    return(sus_elements)
  }
  
  update_elements <- function(elements, mapping){
    ind <- match(elements, mapping$org)
    new <- mapping$new[ind]
    return(new)
  }
  
  sus_fact <- sus_elements(xbrl$fact$elementId)
  sus_element <- sus_elements(xbrl$element$elementId)
  sus_calc_from <- sus_elements(xbrl$calculation$fromElementId)
  sus_calc_to <- sus_elements(xbrl$calculation$toElementId)
  if (fix == FALSE){
    return(list(sus_facts = sus_fact, 
                sus_elements = sus_element,
                sus_calc_from = sus_calc_from,
                sus_calc_to = sus_calc_to))
  } else {
    print(paste("Found", length(sus_calc_from) + length(sus_calc_to), "suspected elements in calculations."))
    print(paste("Found", length(sus_fact), "suspected elements in facts."))
    rep_element <- stringr::str_sub(sus_element, end = -2)
    mapping <- data.frame(org = sus_element, new = rep_element)
    xbrl$element$elementId <- xbrl$element$elementId |> update_elements(mapping = mapping)
    xbrl$label$elementId <- xbrl$label$elementId |> update_elements(mapping = mapping)
    xbrl$presentation$fromElementId <- xbrl$presentation$fromElementId |> update_elements(mapping = mapping)
    xbrl$presentation$toElementId <- xbrl$presentation$toElementId |> update_elements(mapping = mapping)
    xbrl$definition$fromElementId <- xbrl$definition$fromElementId |> update_elements(mapping = mapping)
    xbrl$definition$toElementId <- xbrl$definition$toElementId |> update_elements(mapping = mapping)
    xbrl$calculation$fromElementId <- xbrl$calculation$fromElementId |> update_elements(mapping = mapping)
    xbrl$calculation$toElementId <- xbrl$calculation$toElementId |> update_elements(mapping = mapping)
    xbrl$fact$elementId <- xbrl$fact$elementId |> update_elements(mapping = mapping)
    return(xbrl)
  }
}

brkb_timeseries_10q <- function(){
  # 1.) obtain CIK
  # 2.) download 10-Q and 10-K links
  # 3.) For each 10)Q form, download xbrl instance and xbrl schema
  # 4.) parse tables from xbrl instance into csv file
  # 5.) extract individual tables (income statement and balance sheet) from csv file, maintain hierarchy
  # 6.) join the same table from from different filings over time to create a time series
  source("./scripts/finstr/finstr.R")
  source("./scripts/functions/10Q_10K_functions.R")
  cik <- "0001067983"
  type <- "10-Q"
  filing_urls <- edgar_link_to_filings(cik = cik, form = "10-Q")
  # filing_urls <- "/Archives/edgar/data/1067983/000095017024090305/0000950170-24-090305-index.htm"
  # filing_urls <- "/Archives/edgar/data/1067983/000095017023058993/0000950170-23-058993-index.htm"
  # filing_urls <- "/Archives/edgar/data/1067983/000095017023038705/0000950170-23-038705-index.htm"
  # filing_urls <- c("/Archives/edgar/data/1067983/000095017023038705/0000950170-23-038705-index.htm",
  #                 "/Archives/edgar/data/1067983/000156459022028282/0001564590-22-028282-index.htm")
  #filing_urls <- c("/Archives/edgar/data/1067983/000156459021055032/0001564590-21-055032-index.htm", 
  #              "/Archives/edgar/data/1067983/000156459020052144/0001564590-20-052144-index.htm")
  # want:
  # income statement for all business units separately for different periods
  # aggregate balance sheet for different periods

  for (i in 1:min(40, length(filing_urls))){

    if (sum(is.na(filing_urls) > 0)) browser()
    if (nchar(filing_urls[i]) < 5 | is.na(filing_urls[i])) browser()
    xml_filenames <- edgar_xbrl_URLs(paste0("https://www.sec.gov", filing_urls[i]),
                                     verbose = TRUE)
    print(paste("i = ", i, "XML file:", xml_filenames[1]))
    
    xbrl <- parse_xbrl(xml_filenames, cache_dir = "xbrl/cache_dir/")
    # xbrl <- check_elementnames(xbrl, fix = TRUE)
    xbrl$fact <- remove_duplicated_facts(xbrl$fact)
    st <- xbrl_get_statements_WB(xbrl_vars = xbrl, 
                                 lbase = "calculation",
                                 complete_first = FALSE, 
                                 end_of_quarter = TRUE,
                                 basic_contexts = FALSE,
                                 nonzero_only = TRUE,
                                 aggregate_over_period_and_entity = TRUE)
    if (i == 1){
      st_parent <- lapply(st, clean_BRKB_statement, parent_only = TRUE)
      st_all <- lapply(st, clean_BRKB_statement)
      n_parent <- length(st_parent)
      n_all <- length(st_all)
      # statements2excel(st_all, file = "statement1.xlsx")
      #statements2excel(st_pres, file = "statement_pres.xlsx")
    # browser()
    } else {
      st_parent_i <- lapply(st, clean_BRKB_statement, parent_only = TRUE)
      # statements2excel(st_parent_i, file = "statement2.xlsx")
      st_all_i <- lapply(st, clean_BRKB_statement)
      if (length(st_parent_i) > n_parent){
        if ("StatementConsolidatedStatementsOfEarnings2" %in% names(st_parent_i)){
          st_parent_i$StatementConsolidatedStatementsOfEarnings2 <- NULL
        } else {
          statements2excel(st_parent_i)
          browser()
        }
      }
      if (length(st_all_i) > n_all){
        if ("StatementConsolidatedStatementsOfEarnings2" %in% names(st_all_i)){
          st_all_i$StatementConsolidatedStatementsOfEarnings2 <- NULL
        } else {
          # statements2excel(st_all_i)
          browser()
        }
      }
      if (length(st_parent_i) < n_parent){
        browser()
        # statements2excel(st_parent_i)
      }
      
      if (isFALSE("statement" %in% class(st_parent_i[[1]]))) browser()
      if (isFALSE("statement" %in% class(st_parent[[1]]))) browser()
      # browser()
      st_parent <- merge.statements(st_parent, st_parent_i)
      # st_all <- merge.statements(st_all, st_all_i)
    }
    save(st_all, st_parent, file = "./data/BRKB_statements.RData")
  }
  print("Done")
}

clean_BRKB_statement <- function(st, parent_only = FALSE){
  # if missing for all BU, but not parent, should stay missing. 
  # need to track here which observations. Which variable meet these requirements?
  # browser()
  members <- c(NA, "brka:InsuranceAndOtherMember", "brka:RailroadUtilitiesAndEnergyMember")
  st <- st |> dplyr::filter(value1 %in% members)

  if (nrow(st) > 1){
    desc_vars <- colnames(st[1:which(colnames(st) == "value1")])
    value_cols <- colnames(st)
    value_cols <- value_cols[!(value_cols %in% desc_vars)]
    st_pa <- st |> dplyr::filter(is.na(value1))
    members <- members[!is.na(members)]
    st_bu <- st |> dplyr::filter(value1 %in% members)
    empty_cols_pa <- sapply(
      st_pa, function(x) length(stats::na.omit(x))==0 
    )
    empty_cols_pa[desc_vars] <- FALSE
    not_empty_cols_pa <- !empty_cols_pa |> as.vector()
    empty_cols_bu <- sapply(
      st_bu, function(x) length(stats::na.omit(x))==0 
    )
    st_bu[is.na(st_bu)] <- 0
    st_bu[,empty_cols_bu & not_empty_cols_pa] <- NA
    st_bu_total <- st_bu |>
        dplyr::summarise(dplyr::across(value_cols, \(x) sum(x, na.rm = TRUE)))
    ind <- which(empty_cols_pa) - length(desc_vars)
    st_pa[, empty_cols_pa] <- st_bu_total[, ind]
    if (parent_only){
      st <- st_pa  
    } else {
      st <- rbind(st_bu, st_parent = st_pa)
    }
  }
  return(st)
}

elements2excel <- function(el, file = "el.xlsx"){
  excel_filename <- file
  excel_filename <- paste0("./output/", excel_filename)
  options("openxlsx.numFmt" = "#,##0")
  xl.workbook <- openxlsx::createWorkbook()
  
  openxlsx::addWorksheet(xl.workbook, sheetName = paste0("el"), zoom = 130)
  openxlsx::writeData(xl.workbook, sheet = paste0("el"), x= el, startRow = 1, startCol = 1)
  openxlsx::saveWorkbook(xl.workbook, file = excel_filename, overwrite = TRUE)
  options("openxlsx.numFmt" = NULL)  
}

statements2excel <- function(st, file = "statements.xlsx"){
  excel_filename <- file
  excel_filename <- paste0("./output/", excel_filename)
  options("openxlsx.numFmt" = "#,##0")
  xl.workbook <- openxlsx::createWorkbook()
  for (i in 1:length(st)){
    openxlsx::addWorksheet(xl.workbook, sheetName = paste0("st_",i), zoom = 130)
    openxlsx::writeData(xl.workbook, sheet = paste0("st_",i), x= st[[i]], startRow = 1, startCol = 1)
  }
  openxlsx::saveWorkbook(xl.workbook, file = excel_filename, overwrite = TRUE)
  options("openxlsx.numFmt" = NULL)  
}


brkb_bs_statement <- function(xbrl.vars){
  # largely based on sample code from:
  # https://github.com/bergant/XBRLFiles 
  source("./scripts/finstr/finstr.R")
  source("./scripts/finstr/10Q_10K_functions.R")
  
  statement_ids <- xbrl_get_statement_ids_WB(xbrl.vars) 
  browser()
  is <- xbrl_get_statements_WB(xbrl_vars = xbrl.vars, role_ids = statement_ids$roleId[2],
                               complete_first = FALSE)
  is <- is[[1]]
  bs <-xbrl_get_statements_WB(xbrl_vars = xbrl.vars, role_ids = statement_ids$roleId[1],
                              complete_first = FALSE)
  bs <- bs[[1]]
  browser()
  # is <- statement(xbrl.vars, statement_ids$roleId[2])
  
  st <- xbrl_get_statements(xbrl.vars, complete_first = FALSE)
  
  statement_ids <- xbrl_get_statement_ids_WB(xbrl.vars) 
  
  
  st_w_context <- NULL
  for (i in 1:length(st)){
    context <- xbrl.vars$context |> select(contextId, value1)
    ind <- match(st[[i]]$contextId, context$contextId)
    st_w_context[[i]] <- cbind(context[ind, "value1"], st[[i]])
    names(st_w_context[[i]])[1] <- "entity"
    browser()
    st_w_context[[i]] <- st_w_context[[i]] |> dplyr::filter(entity %in% c(NA, "brka:InsuranceAndOtherMember", 
                                                                          "brka:RailroadUtilitiesAndEnergyMember"))
  }
} 