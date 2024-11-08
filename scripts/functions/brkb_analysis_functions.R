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
  # filing_urls <- edgar_link_to_filings(cik = cik, form = "10-Q")

  filing_urls <- "/Archives/edgar/data/1067983/000095017024090305/0000950170-24-090305-index.htm"
  #xml_filename <- download_edgar_xbrlfiles(paste0("https://www.sec.gov", filing_urls[1]), "xbrl/") # can create a for loop later for all filing urls
  xml_filename <- "xbrl/brka-20240630_htm.xml"
  #csv_filename <- save_xbrl_tables_with_arelle(xml_file = xml_filename)
  csv_filename <- "xbrl/brka-20240630_htm.csv"
  
  # parse xbrl files
  r_xbrl <- parse_xbrl(xml_filename)
  r_xbrl$fact <- remove_duplicated_facts(r_xbrl$fact)
  
  statement_ids <- xbrl_get_statement_ids_WB(r_xbrl)
  members <- c(NA, "brka:InsuranceAndOtherMember", "brka:RailroadUtilitiesAndEnergyMember")
  
  is <- xbrl_get_statements_WB(xbrl_vars = r_xbrl, role_ids = statement_ids$roleId[2],
                               complete_first = FALSE)
  is <- is[[1]]

  members <- c(NA, "brka:InsuranceAndOtherMember", "brka:RailroadUtilitiesAndEnergyMember")
  is <- st |> dplyr::filter(value1 %in% members)
  desc_vars <- colnames(st[1:which(colnames(st) == "value1")])
  members <- members[!is.na(members)]
  is_bu <- st |> dplyr::filter(value1 %in% members)
  # if missing for all BU, but not parent, should stay missing. 
  # need to track here which observations. Which variable meet these requirements?
  
  # st_bu[is.na(st_bu)] <- 0
    # is <- is |> dplyr::filter(value1 %in% members)
  bs <- xbrl_get_statements_WB(xbrl_vars = r_xbrl, role_ids = statement_ids$roleId[1],
                              complete_first = FALSE)
  bs <- bs[[1]]
  browser()
  debugonce(clean_BRKB_statement_parent)
  bs <- clean_BRKB_statement_parent(bs)
  is <- clean_BRKB_statement_parent(is)
  

  browser()
  # 
  
}

clean_BRKB_statement_parent <- function(st){
  members <- c(NA, "brka:InsuranceAndOtherMember", "brka:RailroadUtilitiesAndEnergyMember")
  st <- st |> dplyr::filter(value1 %in% members)
  desc_vars <- colnames(st[1:which(colnames(st) == "value1")])
  members <- members[!is.na(members)]
  st_bu <- st |> dplyr::filter(value1 %in% members)
  st_bu[is.na(st_bu)] <- 0
  st_bu_total <- st_bu |>
    dplyr::summarise(dplyr::across(dplyr::where(is.numeric), \(x) sum(x, na.rm = TRUE)))
  st_parent <- st |> dplyr::filter(is.na(value1))
  ind <- which(is.na(st_parent))
  ind <- ind[ind > length(desc_vars)]
  st_parent[1,ind] <- st_bu_total[1, ind - length(desc_vars)]
  return(st_parent)
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