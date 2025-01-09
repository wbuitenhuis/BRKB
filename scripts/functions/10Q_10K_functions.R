
download_edgar_file <- function(url){
    # download files from edgar SEC website
    # sets user agent
    # ensure not to violate download limits
    ua <- httr::user_agent("w.buitenhuis@gmail.com")
    Sys.sleep(0.12)
    webdata <- httr::GET(url, ua)
    httr::warn_for_status(webdata)
    return(webdata)
}

download_file <- function(url){
  # download files from edgar SEC website
  # sets user agent
  # ensure not to violate download limits
  # ua <- httr::user_agent("w.buitenhuis@gmail.com")
  # Sys.sleep(0.12)
  webdata <- httr::GET(url)
  httr::warn_for_status(webdata)
  return(webdata)
}

# edgar_timeseries_10q <- function(){
#   # 1.) obtain CIK
#   # 2.) download 10-Q and 10-K links
#   # 3.) For each 10)Q form, download xbrl instance and xbrl schema
#   # 4.) parse tables from xbrl instance into csv file
#   # 5.) extract individual tables (income statement and balance sheet) from csv file, maintain hierarchy
#   # 6.) join the same table from from different filings over time to create a time series
#   
#   cik <- "0001067983"
#   type <- "10-Q"
#   filing_urls <- edgar_link_to_filings(cik = cik, form = "10-Q")
#   browser()
#   filing_urls <- "/Archives/edgar/data/1067983/000095017024090305/0000950170-24-090305-index.htm"
#   xml_filenames <- edgar_xbrl_URLs(paste0("https://www.sec.gov", filing_urls[1], verbose = TRUE))
#   # xml_filename <- download_edgar_xbrlfiles(paste0("https://www.sec.gov", filing_urls[1]), "xbrl/") # can create a for loop later for all filing urls
#   # xml_filename <- "xbrl/brka-20240630_htm.xml"
#   #csv_filename <- save_xbrl_tables_with_arelle(xml_file = xml_filename)
#   csv_filename <- "xbrl/brka-20240630_htm.csv"
# 
#   # parse xbrl files
#   xbrl <- parse_xbrl_new(xml_filenames, cache_dir = "xbrl/cache/")
#   #a_xbrl <- xbrl_tables_with_arelle(xml_filename)
#   #browser()
#   xbrl$fact <- remove_duplicated_facts(xbrl$fact)
#   statement <- xbrl_statement(xbrl)
#   # 
# }

edgar_link_to_filings <- function(cik, form = "10-Q"){
  # Given a CIK, it gets links to the SEC filings of the specified form for all periods available,
  # as long as total number of links is not more than 100
  
  # error checks
  # browser()
  if (form %in% c("10-Q", "10-K")){
    # this is good
  } else {
    cat(paste("form has to be either 10-K or 10-Q, not", form))
    browser()
    # probably can use for other forms as well, but this is currently not in scope.
  }
  cik <- as.character(cik)
  # Define the URL for the EDGAR search
  items_on_page <- "100"
  
  url <- paste0("https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=", cik, 
                "&type=", form, "&dateb=&owner=exclude&count=", items_on_page)
  
  page <- download_edgar_file(url)
  page <- rvest::read_html(page)
  
  # Extract the links to the 10-Q filings
  index_links <- page |>
    rvest::html_elements("a") |>
    rvest::html_attr("href") 
  index_links_desc <- page |> rvest::html_elements("a") |> rvest::html_text()
  ind <- stringr::str_detect(index_links,"Archives/edgar/data")
  index_links <- index_links[ind]
  index_links_desc <- index_links_desc[ind]
  
  return(index_links)
}

edgar_xbrl_URLs <- function(url, verbose = FALSE){
  # given a url (obtained by edgar_link_to_filings()), it download the XBRL files 
  # of the specific filing
  
  page <- download_edgar_file(url)
  page <- rvest::read_html(page, encoding = "UTF-8")
  form_links <- page |>
    rvest::html_elements("a") |>
    rvest::html_attr("href") 
  form_links_desc <- page |> rvest::html_elements("a") |> rvest::html_text()
  # find the correct link with the html filing
  ind <- stringr::str_detect(form_links,"Archives/edgar/data")
  form_links <- form_links[ind]
  form_links_desc <- form_links_desc[ind]
  
  #form_links <- grep("Archives/edgar/data", form_links, value = TRUE)
  ind <- stringr::str_detect(form_links,"-")
  form_links <- form_links[ind]
  form_links_desc <- form_links_desc[ind]
  start_filename <- lapply(stringr::str_locate_all(form_links, "/"), xts::last) |> unlist()
  start_filename <- start_filename[seq(from = 1, by = 2, to = length(start_filename))] + 1
  filename <- stringr::str_sub(form_links, start_filename, stringr::str_length(form_links))
  dash_pos <- stringr::str_locate(filename,"-")[,1]
  left_str <- stringr::str_sub(filename, 1, dash_pos - 1)
  right_str <- stringr::str_sub(filename, dash_pos + 1)
  dot_pos <- stringr::str_locate(right_str,"\\.")[,1]
  date_str <- stringr::str_sub(right_str, 1, dot_pos - 1)
  extension <- stringr::str_sub(right_str, dot_pos + 1, stringr::str_length(right_str))
  is_date <- stringr::str_detect(date_str, "[:digit:]{6,8}")
  is_htm <- stringr::str_detect(extension, "htm")
  is_xml <- stringr::str_detect(extension, "xml")
  is_txt <- stringr::str_detect(extension, "txt")
  is_xsd <- stringr::str_detect(extension, "xsd")
  form_link_txt <- form_links[is_date & is_txt]
  form_link_html <- form_links[is_date & is_htm]
  form_link_xml <- form_links[is_date & is_xml]
  form_link_xsd <- form_links[is_date & is_xsd]
  filename_xsd<- filename[is_date & is_xsd]
  date_str <- date_str[is_date & is_htm]
  filename_xml <- filename[is_date & is_xml]
  filename_xsd <- filename[is_date & is_xsd]
  if (length(filename_xml) == 0){
    # there are no kml files
    print(there are no XML files.)
    return()
  }
  url_xml <- paste0("https://www.sec.gov", form_link_xml)
  url_xsd <- paste0("https://www.sec.gov", form_link_xsd)
  ret <- c(url_xml, url_xsd)
  if (verbose){
    print(paste("For", date_str, "found", length(ret), "files. First one is:", 
                ret[1]))
  }
  return(ret)
}


xbrl_tables_with_arelle <- function(xml_file){
  # Given an XBRL file in XLM format it saves the Arelle output in CSV files for
  # various tables Arelle can create.
  
  xml_file <- xml_file[1] # only looks at 1st xml file if multiple are provided
  ind <- stringr::str_locate_all(xml_file, "/")[[1]][,1] |> xts::last()
  dir_name <- stringr::str_sub(xml_file, 1, ind)
  filename <- stringr::str_sub(xml_file, start = ind + 1, end = -4)

  # create file names
  facttable_filename <- paste0("fact_table_", filename,"csv")
  facts_filename <- paste0("facts_", filename,"csv")
  linkbase_filename <- paste0("linkbase_", filename,"csv")
  pres_filename <- paste0("pres_", filename,"csv")
  dim_filename <- paste0("dim_", filename,"csv")
  cal_filename <- paste0("cal_", filename,"csv")
  concepts_filename  <- paste0("concepts_", filename,"csv")
  DTS_filename <- paste0("DTS_", filename,"csv")
  formulea_filename  <- paste0("formulea_", filename,"csv")
  anch_filename <- paste0("anch_", filename,"csv")
  table_filename <- paste0("table_", filename,"csv")
  view_filename <- paste0("view_", filename,"csv")
  arcrole_filename <- paste0("arcrole_", filename,"csv")
  roletypes_filename <- paste0("roletypes_", filename,"csv")
  arcroletypes_filename <- paste0("arcroletypes_", filename,"csv")
  
  arelle_arg <- c("--validate", paste0("--file ", xml_file),
                  "--factListCols=Label,Name,contextRef,unitRef,Dec,Prec,Lang,Value,Period,Dimensions,EntityScheme,EntityIdentifier",
                  "--factTableCols=Label,Name,contextRef,unitRef,Value,Period,Dimensions,EntityIdentifier",
                  "--relationshipCols Name,Documentation,References",
                  paste0("--facts=", dir_name, facts_filename),
                  paste0("--factTable=", dir_name, facttable_filename),
                  paste0("--DTS=", dir_name, DTS_filename),
                  paste0("--pre=",dir_name, pres_filename),
                  paste0("--formulae=", dir_name, formulea_filename),
                  paste0("--anch=", dir_name, anch_filename),
                  paste0("--dim=", dir_name, dim_filename),
                  paste0("--cal=", dir_name, cal_filename),
                  paste0("--concepts=", dir_name, concepts_filename),
                  paste0("--table=", dir_name, table_filename),
                  paste0("--viewFile=", dir_name, view_filename),
                  paste0("--viewarcrole=", dir_name, arcrole_filename),
                  paste0("--roleTypes=", dir_name, roletypes_filename),
                  paste0("--arcroleTypes=", dir_name, arcroletypes_filename),
                  paste0("--table=", dir_name, table_filename))
  # The following options need additional plugins:
  # "--save-loadable-excel",
  # "--package-DTS",
  # "--save-EBA-tablesets",
  # "--save-skos")
  system2("/Applications/Arelle.app/contents/MacOS/arelleCmdLine", 
          args = arelle_arg)
  
  # elements <- read.csv(file = paste0(dir_name, ))
  elements <- NULL
  roles <- read.csv(file = paste0(dir_name, roletypes_filename))
  # labels <- read.csv(file = paste0(dir_name, ))
  labels <- NULL
  presentation <- read.csv(file = paste0(dir_name, pres_filename))
  # definition <- read.csv(file = paste0(dir_name, ))
  definition <- NULL
  calculation <- read.csv(file = paste0(dir_name, cal_filename))
  contexts <- read.csv(file = paste0(dir_name, concepts_filename))
  # units <- read.csv(file = paste0(dir_name, ))
  units <- NULL
  facts <- read.csv(file = paste0(dir_name, facts_filename))
  # footnotes <- read.csv(file = paste0(dir_name, ))
  footnotes <- NULL
  output = list("element"=elements,
                "role"=roles,
                "label"=labels,
                "presentation"=presentation,
                "definition"=definition,
                "calculation"=calculation,
                "context"=contexts,
                "unit"=units,
                "fact"=facts,
                "footnote"=footnotes)
  return(output)
}


parse_xbrl <- function(xbrl_files, cache_dir = "xbrl_cache/"){
  # this uses the XBRL package to parse XBRL and create relevant tables
  # it checks if all xbrl schemas are downloaded, and if not will do so and store in cache dir
  # parse xml files as well as all schemas and joins them
  # uses XBRL package for R
  # browser()
  if (stringr::str_sub(cache_dir, start =-1) != "/"){
    cache_dir <- paste0(cache_dir, "/")
  }
  if (isFALSE(dir.exists(cache_dir))){
    dir.create(cache_dir)
  }
  
  xbrl_l <- NULL
  n <- length(xbrl_files)
  i <- 1
  while (i <= n){
    xbrl_name <- basename(xbrl_files[i])
    if (isFALSE(file.exists(paste0(cache_dir, xbrl_name)))){
      if (stringr::str_detect(xbrl_files[i], ".sec.gov/")){
        # dealing with a SEC download
        xbrl_file <- download_edgar_file(xbrl_files[i])
      } else {
        # dealing with another file
        xbrl_file <- download_file(xbrl_files[i])
      }
      xbrl_file <- xbrl_file |> httr::content("text", encoding = "UTF-8")
      write(xbrl_file, paste0(cache_dir, xbrl_name))
    }
    xbrl_l[[i]] <- try(XBRL::xbrlParse(paste0(cache_dir, xbrl_name)))
    if ("try-error" %in% class(xbrl_l[[i]])) browser()
    importnames <- XBRL::xbrlGetImportNames(xbrl_l[[i]])
    importnames <- importnames[!(importnames %in% xbrl_files)]
    keep <- stringr::str_sub(importnames,1,8) == "https://" | stringr::str_sub(importnames,1,7) == "http://"
    importnames <- importnames[keep]
    xbrl_files <- c(xbrl_files, importnames)
    n <- length(xbrl_files)
    i <- i + 1
  }

  el <- ro <- lab <- pres <- def <- calc <- NULL
  facts <- context <- units <- footnotes <- elements <- labels <-NULL
  roles <- presentation <- definition <- calculation <- NULL
  found_xbrl_instance <- FALSE
  for (i in 1:length(xbrl_l)){
    if (isFALSE(found_xbrl_instance)){
      facts <- XBRL::xbrlProcessFacts(xbrl_l[[i]])
      if (nrow(facts) != 0){
        found_xbrl_instance <- TRUE  
        contexts <- XBRL::xbrlProcessContexts(xbrl_l[[i]])
        units <- XBRL::xbrlProcessUnits(xbrl_l[[i]])
        footnotes <- XBRL::xbrlProcessFootnotes(xbrl_l[[i]])
      } 
    }
    el <- XBRL::xbrlProcessElements(xbrl_l[[i]])
    elements <- rbind(elements, el)
    lab <- XBRL::xbrlProcessLabels(xbrl_l[[i]])
    labels <- rbind(labels, lab)
    ro <- XBRL::xbrlProcessRoles(xbrl_l[[i]])
    roles <- rbind(roles, ro)
    pres <- XBRL::xbrlProcessArcs(xbrl_l[[i]], "presentation")
    if (!is.null(pres)) presentation <- rbind(presentation, pres)
    def <- XBRL::xbrlProcessArcs(xbrl_l[[i]], "definition")
    if (!is.null(def)) definition <- rbind(definition, def)
    calc <- XBRL::xbrlProcessArcs(xbrl_l[[i]], "calculation")
    if (!is.null(calc)) calculation <- rbind(calculation, calc)
    XBRL::xbrlFree(xbrl_l[[i]])
  }

  output = list("element"=elements,
                "role"=roles,
                "label"=labels,
                "presentation"=presentation,
                "definition"=definition,
                "calculation"=calculation,
                "context"=contexts,
                "unit"=units,
                "fact"=facts,
                "footnote"=footnotes)
  return(output)
}


remove_duplicated_facts <- function(facts, verbose = FALSE){
  # https://www.xbrl.org/WGN/xbrl-duplicates/WGN-2015-12-09/xbrl-duplicates-WGN-2015-12-09.html
  # It is possible to have duplicated facts, that are only unique in their FactID.
  # It is also possible to have duplicated facts, but they may differ in precision, 
  # declared language or unit. For instance different currencies.
  # This function looks for duplicated facts and removes them. 
  library(dplyr)
  full_dupes <- facts |> select(!factId) |> duplicated()
  # these are 100% duplicates
  # remove first full duplicates
  print(paste("Removing", sum(full_dupes), "duplicated facts."))
  facts <- facts[!full_dupes, ]
  
  # now recheck for other dupes
  dupes <- facts |> select(elementId, contextId) |> duplicated()
  if (verbose){
    print(paste(sum(dupes), "facts left with same element and context ID, 
                but either different values or units,"))
  }
  # these are the duplicated that can be full duplicates, or caused by:
  # currency differences
  # language differences
  # precision differences
  # look into cause of these
  ind <- which(dupes)
  remove_rows <- NULL
  if (length(ind) > 0){
    for(i in 1:length(ind)){
      element <- facts$elementId[ind[i]]
      context <- facts$contextId[ind[i]]
      dupe_fact <- facts |> filter(elementId == element & contextId == context)
      if (nrow(dupe_fact) == 1) next #should not happen honestly
      test_different_units <- sum(dupe_fact$unitId[1] == dupe_fact$unitId) != nrow(dupe_fact)
      if (test_different_units){
        not_usd <- dupe_fact$unitId != "U_USD"
        if (sum(not_usd) > 0){
          if (verbose){
            print(paste(sum(not_usd), "facts are not in USD. Removing those."))
          }
          # remove_rows <- c(remove_rows, which(facts$factId %in% dispose))
          dispose <- dupe_fact$factId[not_usd]
          remove_rows <- c(remove_rows, which(facts$factId %in% dispose))
        } else {
          browser()
          # in this case unit differences is unrelated to USD. Need to examine
        }
        next
      }
      test_different_precision <- sum(dupe_fact$decimals[1] == dupe_fact$decimals) != nrow(dupe_fact)
      if (test_different_precision){
        # look for max precision
        dupe_fact$decimals <- as.numeric(dupe_fact$decimals)
        dispose <- (dupe_fact$decimals < max(dupe_fact$decimals))
        dispose <- dupe_fact$factId[dispose]
        if (verbose){
          print(paste("Remove", length(dispose), "objects that have alternative facts with higher precision."))
        }
        remove_rows <- c(remove_rows, which(facts$factId %in% dispose))
        next
      }
      test_different_values <- sum(dupe_fact$fact[1] == dupe_fact$fact) != nrow(dupe_fact)
      if (test_different_values){
        browser()
        # in this case you have conflicting facts. Need to look into
        # remove_rows <- c(remove_rows, which(facts$factId %in% dispose))
      }
    }
    keep <- setdiff(1:nrow(facts), remove_rows)
    facts <- facts[keep,]
  }
  return(facts)
}

# makes sure statements y will have same names in same order as statements x
compare_statement_names <- function(x, y){

  if( !"statements" %in% class(x) || !"statements" %in% class(y) ) {
      stop("Not statements objects")
  }
  if (length(x) != length(y)){
    stop("statements objects are not of same length")
  }
  # if one statement is missing assign it the value fo the statement to be merged
  if ("WB error" %in% lapply(x, class) |> unlist() |> unique()){
    ind <- which(lapply(x, class) == "WB error")
    x[[ind]] <- y[[ind]]
  }
  if ("WB error" %in% lapply(y, class) |> unlist() |> unique()){
    ind <- which(lapply(y, class) == "WB error")
    y[[ind]] <- x[[ind]]
  }
  
    
  # Define a function to count common elements
  perc_common_elements <- function(df1, df2) {
    # Find common elements in the first variable
    df1 <- df1[,-(1:5)]
    df2 <- df2[,-(1:5)]
    common_elements <- intersect(names(df1), names(df2))
    ret <- length(common_elements) / min(ncol(df1),ncol(df2))
    return(ret)
  }

  same_name <- names(x) == names(y)
  # all names are the same, nothing to do
  if (sum(same_name) == length(x)) return(y)
  
  # check for duplicates in names
  dupes_x <- duplicated(names(x))
  dupes_y <- duplicated(names(y))
  if (sum(dupes_x) > 0 | sum(dupes_y) > 0){
    # have duplicated names
    browser()
  }
  
  ind <- names(y) %in% names(x)
  if (sum(ind) == length(x)){
    # x and y have the same names, but they are ordered differently.
    # just need to reorder y
    browser()
    y <- y[names(x)]
    return(y)
  } else if (sum(ind) == length(x) - 1){
    # only one name difference
    browser()
    names(y)[!ind] <- names(x)[!(names(x) %in% names(y))]
  } else {
    # multiple name differences
    # check which have different names
    missing_names_y <- names(y)[!ind]
    missing_names_x <- names(x)[!(names(x) %in% names(y))]
    y_missing <- y[missing_names_y]
    x_missing <- x[missing_names_x]
    common_elements <- matrix(NA, nrow = length(x_missing), ncol = length(y_missing))
    for (i in 1:length(x_missing)){
      for(j in 1:length(y_missing)){
        common_elements[i,j] <- perc_common_elements(x_missing[[i]], y_missing[[j]])
      }
    }
    ind2 <- apply(common_elements, 2, which.max)
  
      if (sum(duplicated(ind2)) > 1) browser() # have major issue
    if (sum(duplicated(ind2)) > 0){
      # This can happen if all variables in a statement are renamed in one period.
      # Next best matching method is either:
      # 2.) process of elimination: if one of duplicates has high match and other is a tie
      # assign highest match
      ind3 <- which(ind2 == ind2[duplicated(ind2)])
      common_elements3 <- common_elements[ind3, ind3]
      which.max(common_elements3)
      max3 <- apply(common_elements3,2,max)
      winner <- which.max(max3)
      loser <- which.min(max3)
      not_assigned <- (1:length(ind2))[!(1:length(ind2)) %in% ind2]
      ind2[ind3[loser]] <- not_assigned
      # 1.) assume statement order did not change
      # 3. ) go by closest statement name match
    } 
      
    names(y)[!ind] <- missing_names_x[ind2]
  }
  y <- y[names(x)]
  class(y) <- class(x)
  if( !"statements" %in% class(x) || !"statements" %in% class(y) ) browser()
  
  return(y)
  
}




# xbrl_statement <- function(xbrl.vars){
#   # largely based on sample code from:
#   # https://github.com/bergant/XBRLFiles 
#   source("./scripts/finstr/finstr.R")
# 
#   xbrl.vars$fact <-   remove_duplicated_facts(xbrl.vars$fact)
#   
# 
#   statement_ids <- xbrl_get_statement_ids_WB(xbrl.vars) 
#   browser()
#   is <- xbrl_get_statements_WB(xbrl_vars = xbrl.vars, role_ids = statement_ids$roleId[2],
#                             complete_first = FALSE)
#   is <- is[[1]]
#   bs <-xbrl_get_statements_WB(xbrl_vars = xbrl.vars, role_ids = statement_ids$roleId[1],
#                               complete_first = FALSE)
#   bs <- bs[[1]]
#   browser()
#   # is <- statement(xbrl.vars, statement_ids$roleId[2])
# 
#   st <- xbrl_get_statements(xbrl.vars, complete_first = FALSE)
#   
#   statement_ids <- xbrl_get_statement_ids_WB(xbrl.vars) 
#   
#   
#   st_w_context <- NULL
#   for (i in 1:length(st)){
#     context <- xbrl.vars$context |> select(contextId, value1)
#     ind <- match(st[[i]]$contextId, context$contextId)
#     st_w_context[[i]] <- cbind(context[ind, "value1"], st[[i]])
#     names(st_w_context[[i]])[1] <- "entity"
#     browser()
#     st_w_context[[i]] <- st_w_context[[i]] |> dplyr::filter(entity %in% c(NA, "brka:InsuranceAndOtherMember", 
#                                      "brka:RailroadUtilitiesAndEnergyMember"))
#   }
#   browser()
# 
#   check <- lapply(st, check_statement)
#   
#   excel_filename <- "statements.xlsx"
#   excel_filename <- paste0("./output/", excel_filename)
#   options("openxlsx.numFmt" = "#,##0")
#   xl.workbook <- openxlsx::createWorkbook()
#   for (i in 1:length(st)){
#     openxlsx::addWorksheet(xl.workbook, sheetName = paste0("st_",i), zoom = 130)
#     openxlsx::writeData(xl.workbook, sheet = paste0("st_",i), x= st[[i]], startRow = 1, startCol = 1)
#   }
#   openxlsx::saveWorkbook(xl.workbook, file = excel_filename, overwrite = TRUE)
#   options("openxlsx.numFmt" = NULL)  
# 
# 
#   
#   role_ids <- xbrl.vars$role$roleId[xbrl.vars$role$type == "Statement"]
#     
#   role_id <- "http://www.berkshirehathaway.com/20240630/taxonomy/role/Role_StatementConsolidatedBalanceSheets"
#   # browser()
#   statement_name <- basename(role_ids[3])
#   table <-statement(xbrl.vars, role_ids[3])
#   # 
#   browser()
#   table <- NULL
#   for (i in 1:length(role_ids)){
#     browser()
#     print(role_ids[i])
#     table[[i]] <- statement(xbrl.vars, role_ids[i])
#   }
#   browser()
# }   
#   



# statement <- function(xbrl.vars, role_id){
#   # based on: https://github.com/bergant/XBRLFiles
#   library(dplyr)
#   # browser()
#   # prepare presentation linkbase :
#   # filter by role_id and convert order to numeric
#   pres <-
#     xbrl.vars$presentation |>
#     filter(roleId %in% role_id) |>
#     mutate(order = as.numeric(order))
# 
#   # start with top element of the presentation tree
#   pres_df <-
#     pres |>
#     anti_join(pres, by = c("fromElementId" = "toElementId"))
#   pres_df <- pres_df |>
#     select(elementId = fromElementId)
# 
#   # breadth-first search
#   # add subsequent elements to presentation tree
#   while({
#     df1 <- pres_df |> na.omit()
#     df1 <- df1 |> left_join(pres, by = c("elementId" = "fromElementId"))
#     df1 <- df1 |> arrange(elementId, order)
#     df1 <- df1 |> select(elementId, child = toElementId);
#     nrow(df1) > 0
#   })
#   {
#     # add each new level to data frame
#     prev_pres_df <- pres_df
#     pres_df <- pres_df |> left_join(df1, by = "elementId")
#     names(pres_df) <-  c(sprintf("level%d", 1:(ncol(pres_df)-1)), "elementId")
#     # if (sum(is.na(pres_df$elementId)) > 0) browser()
#   }
# 
#   # add last level as special column (the hierarchy may not be uniformly deep)
#   # WB: this ensures there is an elementId for all rows
#   browser()
#   pres_df["elementId"] <-
#     apply( t(pres_df), 2, function(x){tail( x[!is.na(x)], 1)})
#   pres_df["elOrder"] <- 1:nrow(pres_df)
#   
#   
#   # min_level <- min(res$level, na.rm = TRUE)
#   # min_dec <- min(as.numeric(res$decimals), na.rm = TRUE)
#   # context_filter <- res |> dplyr::filter(level == min_level) |>
#   #    getElement("contextId") |> unique()
#   #  
#   # decimals_filter <- res |> dplyr::filter(level == min_level) |>
#   #    getElement("decimals") |>  unique()
#   # 
#   # join concepts with context, facts
#   pres_df_num <-
#     pres_df |>
#     inner_join(xbrl.vars$fact, by = "elementId")
#   pres_df_num <- pres_df_num |>
#     inner_join(xbrl.vars$context, by = "contextId")
#   #pres_df_num <- pres_df_num |> # this is an issue for BRKB if you want segment reporting
#   #  filter(is.na(dimension1)) # keeps only rows where there is no dimension1 value
#   pres_df_num <- pres_df_num |>
#     filter(!is.na(endDate))
#   pres_df_num <- pres_df_num|>
#     select(elOrder, contains("level"), elementId, fact, decimals, startDate, endDate, value1) |>
#     #  select(elOrder, contains("level"), elementId, fact, decimals, endDate, value1) |>
#     mutate(fact = as.numeric(fact) * 10^as.numeric(decimals)) # should also keep dimension1 for brkb income statements
#   pres_df_num_w <- pres_df_num |> tidyr::pivot_wider(
#     names_from = endDate,
#     values_from = fact,
#     values_fill = NA)
#   
#   # min_level <- min(res$level, na.rm = TRUE)
#   # 
#   # min_dec <- min(as.numeric(res$decimals), na.rm = TRUE)
#   # 
#   # context_filter <- res |> dplyr::filter(level == min_level) |>
#   #   getElement("contextId") |> unique()
#   # 
#   # decimals_filter <- res |> dplyr::filter(level == min_level) |>
#   #   getElement("decimals") |>  unique()
#   # res <-
#   #   elements |>
#   #   dplyr::inner_join(xbrl_vars$fact, by = "elementId")
#   # 
#   # res <-
#   #   res |>
#   #   dplyr::filter(contextId %in% context_filter) |>
#   #   dplyr::filter(decimals %in% decimals_filter) |> 
#   #   dplyr::mutate(fact = as.numeric(fact), decimals = min_dec ) |>
#   #   dplyr::inner_join(xbrl_vars$context, by = "contextId") |>
#   #   dplyr::select(contextId, startDate, endDate, elementId, fact, decimals) |>
#   #   tidyr::pivot_wider(names_from = "elementId", values_from = "fact") |>
#   #   dplyr::arrange(endDate)
#   
#   # pres_df_num_w <- pres_df_num |> tidyr::pivot_wider(
#   #               names_from = c(endDate, value1),
#   #               values_from = fact,
#   #               names_vary = "fastest")
# 
# 
#   pres_df_num_w  <-  pres_df_num_w |> arrange(elOrder)
# 
#   # replace NULL values with missing values (apparently does happen in certain situations)
#   # this is only the case if lists() re created by pivot_wider, which can happen if facts are not unique (or too much context has been lost)
#   # Function to replace NULL with NA
#   replace_nulls <- function(x) {
#     x[sapply(x, is.null)] <- NA
#     return(x)
#   }
#   # Apply the function to each column
#   pres_df_num_w[] <- lapply(pres_df_num_w, replace_nulls)
# 
# 
#   # next step would be to add labels, make calculations, join concepts with labels
#   # apparently it is possible to have two facts with the same elementId and contextId, but with different factIds otherwise completely identical
#   # https://www.xbrl.org/WGN/xbrl-duplicates/WGN-2015-12-09/xbrl-duplicates-WGN-2015-12-09.html
# 
#   # labels for our financial statement (role_id) in "en-US" language:
#   x_labels <-
#     xbrl.vars$presentation |>
#     filter(roleId == role_id) |>
#     select(elementId = toElementId, labelRole = preferredLabel) |>
#     semi_join(pres_df_num, by = "elementId") |>
#     left_join(xbrl.vars$label, by = c("elementId", "labelRole")) |>
#     filter(lang == "en-US") |>
#     select(elementId, labelString)
# 
#   # calculated elements in this statement component
#   x_calc <- xbrl.vars$calculation |>
#     filter(roleId == role_id) |>
#     select(elementId = fromElementId, calcRoleId = arcrole) |>
#     unique()
# 
#   # find relevant dates
#   # 1.) most recent date
#   # 2.) date with least missing observations
#   dates <- colnames(pres_df_num)[stringr::str_detect(colnames(pres_df_num), "[:digit:]{4}")]
#   most_recent_dt <- max(as.Date(dates)) |> as.character()
#   dates <- dates[dates != as.character(most_recent_dt)]
#   nr_missing_obs <- pres_df_num |> select(all_of(dates)) |> is.na() |> apply(2, sum)
#   least_missing_date <- dates[nr_missing_obs == min(nr_missing_obs)]
# 
#   statement_pretty <- pres_df_num |>
#     left_join(x_labels, by = "elementId") |>
#     left_join(x_calc, by = "elementId") |> select("labelString",
#                                                   matches(most_recent_dt),
#                                                   matches(least_missing_date))
# 
#   return(statement_pretty)
# }



################### can ignore everything below ###############

# oldwork <-function(){
# # this contains useful code, but started different approach
# # Not at all completed
#   
#   #### list Arelle vs R:
#   # element - NULL vs 201 x 8
#   # role - 792 x 3 vs 11 x 5
#   # label - NULL vs 1922 x 5
#   # presentation - 1559 x 14 vs 1211 x 11
#   # definition - NULL - 1094 x 11
#   # calculation - 220 x 12 vs 175 x 11
#   # context - 19044 x 13 vs 682 x 13
#   # unit - NULL vs 19 x 4
#   # fact - 3326 x 13 vs 1902 x 9
#   # R: Unique identifier: factId. 422 unique elementId, 682 contextID
#   # footnote - NULL vs 61 x 5
#   
#   # start with presentation file
#   pres <- r_xbrl$presentation
#   # find statements
#   ind <- stringr::str_detect(tolower(pres$roleId), "statement")
#   statements <- unique(pres$roleId[ind])
#   # analyze 1st statement. Hopefully this can be generalized for all statements
#   statement <- statements[1]
#   statement_pres <- pres[pres$roleId == statement, ]
#   write.csv(x = statement_pres, file = "./output/presentation.csv", col.names = TRUE)
#   strt <- match(statement_pres$fromElementId, r_xbrl$element$elementId)
#   end <- match(statement_pres$toElementId, r_xbrl$element$elementId)
#   browser()
#   # elementsIDs in elements table all start with "brka_", while statement table needs "us-gaap_"
#   # facts does have the right element IDs
#   
#   
#   
#   # old code trying to work with arelle table file
#   ind <- stringr::str_locate_all(filename, "/")[[1]][,1] |> xts::last()
#   dir_name <- stringr::str_sub(filename,1,ind)
#   filename <- stringr::str_sub(filename,start = ind + 1)
#   data <- read.csv(paste0(dir_name,"fact_table_", filename))
#   concepts <- read.csv(paste0(dir_name, "concepts_", filename))
#   pres <- read.csv(paste0(dir_name, "pres_", filename))
#   calc <- read.csv(paste0(dir_name, "cal_", filename))
#   dim <- read.csv(paste0(dir_name, "dim_", filename))
#   facts <- read.csv(paste0(dir_name, "facts_", filename))
# 
#   # read in presentation file
#   # select statements
#   # check if names are unique fact identifiers - they are not, a number of facts should mach them, all with a different contexts.
#   # obtain matching facts
#   # find matching concepts or contexts.
#   
#   # find tables and classify them- can make this one function
#   i_concept <- which(data[,1] != "")
#   concept_names <- data[i_concept,1]
#   concept_id <- stringr::str_sub(concept_names,end = 6)
#   ind <- stringr::str_locate_all(concept_names, " - ")
#   indf <- function(x){
#     output <- c(x[1,2] + 1, x[2,1] - 1)
#   }
#   v <- lapply(X=ind, FUN = indf)
#   v <- t(matrix(unlist(v), nrow = 2))
#   colnames(v) <- c("start", "end")
#   concept_classification <- stringr::str_sub(concept_names, v)
#   label <- stringr::str_sub(concept_names, start = v[,"end"] + 4)
#   end <- c(i_concept[-1]-1, nrow(data))
#   tables <- data.frame(start = i_concept, end = end, class = concept_classification,
#                        name = label)
#   # select only tables that contain statements
#   tables <- tables[tolower(tables$class) == "statement", ]
#   # this would be end of first function
#   table <- data[tables$start[1]:tables$end[1], ]
#   browser()
#   indf <- function(x){
#     which(x != "")[1]
#   }
#   hierarchy <- apply(table,1,indf)
#   names(hierarchy) <- 1:length(hierarchy)
#   label <- table[,1:max(hierarchy)]
#   keep <- label != ""
#   label <- label[keep]
#   data <- table[, (max(hierarchy)+1):ncol(table)]
#   data[data ==""] <- NA
#   no_obs <- is.na(data)
#   keep_col <- apply(no_obs,2,sum) != nrow(data)
#   data <- data[,keep]
#   date <- stringr::str_extract(colnames(data), "[:digit:]{2,}.[:digit:]{2,}.[:digit:]{2,}")
#   date <- lubridate::as_date(date, format = "...")
#   date <- stringr::str_replace_all(date, ".", "-")
#   
#   
#   # for each table extract information. For now will work with one table first
#   # Will take table 1
#   
#   
#   # check if table id is fully number
#   # find first and second " - " in between is table classification
#   # select concept_classification == tolower("statement")
#   # second till end - concept label
#   # subset data for 1st statement
#   
#   # check which columns have at least some data, erase all with only missing data
#   # review what is left - in particular look at column names
#   # extract period from column names
#   # extract entity from column names
#   # extract other items from column names?
#   # subset data based on column classifications (period, entity etc.)
#   
# }




# test.download.bkrb <- function(){
#   # Work in progress
# 
#   url <- "https://www.berkshirehathaway.com/reports.html"
#   
#   dir_name <- "./rawdata/financial_reports/"
#   # Load necessary libraries
# 
#   # Define the CIK for Berkshire Hathaway
#   cik <- "0001067983"
#   type <- "10-Q"
#   items_on_page <- "100"
#   # Define the URL for the EDGAR search
#   url <- paste0("https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=", cik, 
#                 "&type=", type, "&dateb=&owner=exclude&count=", items_on_page)
#   
#   # Get the webpage content
#   ua <- httr::user_agent("w.buitenhuis@gmail.com")
#   page <- httr::GET(url, ua) |> rvest::read_html()
#   
#   # Extract the links to the 10-Q filings
#   index_links <- page |>
#     rvest::html_elements("a") |>
#     rvest::html_attr("href") 
#   index_links_desc <- page |> rvest::html_elements("a") |> rvest::html_text()
#   # ind <- grepl("Archives/edgar/data", index_links, value = TRUE)
#   ind <- stringr::str_detect(index_links,"Archives/edgar/data")
#   index_links <- index_links[ind]
#   index_links_desc <- index_links_desc[ind]
# 
#   #alternative approach - nneds to be tested
#   # https://www.r-bloggers.com/2020/02/a-walk-though-of-accessing-financial-statements-with-xbrl-in-r-part-1/
#   # filings <- 
#   #   read_html(url) %>%
#   #   html_nodes(xpath='//*[@id="seriesDiv"]/table') %>%
#   #   html_table() %>%
#   #   as.data.frame() %>%
#   #   janitor::clean_names()
#   # 
#   
#   
# 
#   # Download the first 10-Q filing
#   if (length(index_links) > 0) {
#     # get links of this specific filing
#     Sys.sleep(0.12)
#     page <- httr::GET(paste0("https://www.sec.gov", index_links[1]), ua) |> rvest::read_html()
#     form_links <- page |>
#       rvest::html_elements("a") |>
#       rvest::html_attr("href") 
#     form_links_desc <- page |> rvest::html_elements("a") |> rvest::html_text()
#     # find the correct link with the html filing
#     ind <- stringr::str_detect(form_links,"Archives/edgar/data")
#     form_links <- form_links[ind]
#     form_links_desc <- form_links_desc[ind]
#     
#     #form_links <- grep("Archives/edgar/data", form_links, value = TRUE)
#     ind <- stringr::str_detect(form_links,"-")
#     form_links <- form_links[ind]
#     form_links_desc <- form_links_desc[ind]
#     start_filename <- lapply(stringr::str_locate_all(form_links, "/"), xts::last) |> unlist()
#     start_filename <- start_filename[seq(from = 1, by = 2, to = length(start_filename))] + 1
#     filename <- stringr::str_sub(form_links, start_filename, stringr::str_length(form_links))
#     dash_pos <- stringr::str_locate(filename,"-")[,1]
#     left_str <- stringr::str_sub(filename, 1, dash_pos - 1)
#     right_str <- stringr::str_sub(filename, dash_pos + 1)
#     dot_pos <- stringr::str_locate(right_str,"\\.")[,1]
#     date_str <- stringr::str_sub(right_str, 1, dot_pos - 1)
#     extension <- stringr::str_sub(right_str, dot_pos + 1, stringr::str_length(right_str))
#     is_date <- stringr::str_detect(date_str, "[:digit:]{6,8}")
#     is_htm <- stringr::str_detect(extension, "htm")
#     is_xml <- stringr::str_detect(extension, "xml")
#     is_txt <- stringr::str_detect(extension, "txt")
#     is_xsd <- stringr::str_detect(extension, "xsd")
#     form_link_txt <- form_links[is_date & is_txt]
#     form_link_html <- form_links[is_date & is_htm]
#     form_link_xml <- form_links[is_date & is_xml]
#     form_link_xsd <- form_links[is_date & is_xsd]
#     filename_xsd<- filename[is_date & is_xsd]
#     if (length(form_link_txt) != 1) browser()
#     if (length(form_link_html) != 1) browser()
#     browser()
#     date_str <- date_str[is_date & is_htm]
#     
#     # https://xbrl.us/join-us/membership/individual/
#     # https://www.sec.gov/search-filings/edgar-application-programming-interfaces
#     # https://www.lexjansen.com/pharmasug-cn/2021/SR/Pharmasug-China-2021-SR031.pdf
#     # https://www.r-bloggers.com/2020/02/a-walk-though-of-accessing-financial-statements-with-xbrl-in-r-part-1/
#     
#     Sys.sleep(0.2)
#     url_txt <- paste0("https://www.sec.gov",form_link_txt)
#     url_html <- paste0("https://www.sec.gov",form_link_html)
#     url_xml <- paste0("https://www.sec.gov",form_link_xml)
#     url_xsd <- paste0("https://www.sec.gov",form_link_xsd)
#     webdata_txt <- httr::GET(url_txt, httr::user_agent("w.buitenhuis@gmail.com")) 
#     Sys.sleep(0.2)
#     webdata_html <- httr::GET(url_html, httr::user_agent("w.buitenhuis@gmail.com")) 
#     html <- httr::content(webdata_html, "text")
#     html <- xml2::read_html(webdata_html) # use java script error
#     
#     Sys.sleep(0.2)
#     webdata_xml <- httr::GET(url_xml, httr::user_agent("w.buitenhuis@gmail.com")) 
#     Sys.sleep(0.2)
#     webdata_xsd <- httr::GET(url_xsd, httr::user_agent("w.buitenhuis@gmail.com")) 
#     # setwd("./xbrl/")
#     xml <- webdata_xml |> httr::content("text")
#     write(xml, "./xbrl/test.xml")
#     xsd <- webdata_xsd |> httr::content("text")
#     write(xsd, filename_xsd)
# #   test <- XBRL::xbrlDoAll("test.xml") # does not work
#     
#     browser()
#     # parse data
#     xbrl_doc <- XBRL::xbrlParse("./xbrl/test.xml")
#     schema_name <- XBRL::xbrlGetSchemaName(xbrl_doc)
#     xbrl_xsd <- XBRL::xbrlParse(schema_name) # xsd file (xbrl schema)
#     
#     # read in xbrl doc
#     facts <- XBRL::xbrlProcessFacts(xbrl_doc)
#     contexts <- XBRL::xbrlProcessContexts(xbrl_doc)
#     units <- XBRL::xbrlProcessUnits(xbrl_doc)
#     footnotes <- XBRL::xbrlProcessFootnotes(xbrl_doc)
#     linkbase <- XBRL::xbrlGetLinkbaseNames(xbrl_doc) # empty
#     
#     # read in schema file
#     labels <- XBRL::xbrlProcessLabels(xbrl_xsd)
#     elements <- XBRL::xbrlProcessElements(xbrl_xsd)
#     roles <- XBRL::xbrlProcessRoles(xbrl_xsd)
#     importnames <- XBRL::xbrlGetImportNames(xbrl_xsd)
#     
#     for (i in 1:length(importnames)){
#       cat(importnames[i])
#       ind <- stringr::str_locate_all(importnames[i], "/")[[1]] |> xts::last()
#       ind <- ind[1,1] + 1
#       filename <- stringr::str_sub(importnames[i], start = ind)
#       webdata <- httr::GET(importnames[i])
#       httr::warn_for_status(webdata)
#       import_xsd <- httr::content(webdata, "text")
#       write(import_xsd, file = paste0("./xbrl/xbrl.Cache/", filename))
#     }
#     browser()
#     # arcs <- XBRL::xbrlProcessArcs(xbrl_doc) breaks argument arcType is missing
#     
#     # dir <- getwd()
#     # setwd("./xbrl/")
#     # XBRL::xbrlDoAll("test.xml", cache.dir = "xbrl.Cache", verbose = TRUE, 
#     #                 delete.cached.inst = FALSE)
#     # # breaks when trying. to download a https:// file which is already in the cache dir
#     # setwd(dir)
#     # # XBRL::xbrlFree(xbrl_doc)
#     
#     system2("/Applications/Arelle.app/contents/MacOS/arelleCmdLine", 
#             args = c("--about","--save-loadable-excel"))
#     
#     arelle_arg <- c("--validate", "--file xbrl/test.xml", "--facts=xbrl/facttable.csv")
#     arelle_arg <- c("--validate", "--file xbrl/test.xml", "--facts=xbrl/output.json", 
#                     "--factListCols=Label,Name,contextRef,unitRef,Dec,Prec,Lang,Value",
#                     "--DTS=xbrl/dtsfile.csv",
#                     "--factTable=xbrl/facttable.csv",
#                     "--table=xbrl/table_linkbase.csv",
#                     "--pre=xbrl/presentation_file.csv")
#     system2("/Applications/Arelle.app/contents/MacOS/arelleCmdLine", 
#             args = arelle_arg)
#     
#     # python arelleCmdLine.py --validate --file="yourfile.xbrl" --output="output.json" --tables
#     
#     
#     # arelleCmdLine -f c:\temp\test.rss -v --disclosureSystem efm-pragmatic-all-years --store-to-XBRL-DB "localhost\SQLEXPRESS,,sqlLogin,sqlPassword,,90,mssqlSemantic"
#     # python arelle.py --validate --file="yourfile.xbrl" --output="output.csv"
# 
#     ind <- which(facts$elementId == "us-gaap_WeightedAverageNumberOfSharesOutstandingBasic") 
#     ind <- stringr::str_detect(facts$elementId, "NumberOfSharesOutstanding")
#     unique(facts$elementId[ind])
#     
#     # b <- chromote::ChromoteSession$new()
#     # b$Network$setUserAgentOverride(userAgent = "w.buitenhuis@gmail.com")
#     # webdataLIVE_html <- rvest::read_html_live(url_html) # enable javascript
#     # # the html version seems to only show the java script, not the html being populated.
#     # the $view() method does show the javascript generated page though.
#     # Sys.sleep(6.5)
#     # webdataLIVE_xml <- rvest::read_html_live(url_xml) 
#     # x <- webdataLIVE_xml |> rvest::html_text()
#     
#     Sys.sleep(0.2)
#     webdata <- httr::GET(url_xml, httr::user_agent("w.buitenhuis@gmail.com"))
#     useragent <- webdata$request$options$useragent
#     httr::warn_for_status(webdata)
#     # bin <- httr::content(webdata, "raw")
#     text <- httr::content(webdata_txt, "text")
#     
#     webdataLIVE_txt <- rvest::read_html_live(url_txt) 
#     # need to use read_html_live (using chromite, otherwise get 403 error
#     text <- webdataLIVE_txt$html_elements((css = "pre")) |> rvest::html_text()
#     # all relevant text is part of one big pre tag. this effectively removes the tag
#     writeLines(text, "./output/test.txt")
#     # we now get an html doc
#     # html <- rvest::read_html( "./output/test.txt")
#     
#     tables <- html |> rvest::html_element("table")
#     x <- tables |> rvest::html_table()
#     # look for correct file to download
#     
#     # download_url <- paste0("https://www.sec.gov", links[1])
#     # debugonce(download_edgar_file)
#     # download_edgar_file(download_url, destfile = paste0(dir_name, "Berkshire_Hathaway_10Q.pdf"))
#     # cat("Downloaded:", download_url, "\n")
#   } else {
#     cat("No 10-Q filings found.\n")
#   }
#   
#   browser()
#   
#   
#   file.list <- list.files(dir_name)
#   save_dt <- file.info(paste0(dir_name, file.list))$atime
#   last_file <- file.list[which(save_dt == max(save_dt))[1]]
# 
# }


