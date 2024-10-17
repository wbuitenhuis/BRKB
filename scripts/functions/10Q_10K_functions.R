
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

edgar_timeseries_10q <- function(){
  # 1.) obtain CIK
  # 2.) download 10-Q and 10-K links
  # 3.) For each 10)Q form, download xbrl instance and xbrl schema
  # 4.) parse tables from xbrl instance into csv file
  # 5.) extract individual tables (income statement and balance sheet) from csv file, maintain hierarchy
  # 6.) join the same table from from different filings over time to create a time series
  
  cik <- "0001067983"
  type <- "10-Q"
  
  # filing_urls <- edgar_link_to_filings(cik = cik, form = "10-Q")
  filing_urls <- "/Archives/edgar/data/1067983/000095017024090305/0000950170-24-090305-index.htm"
  # xml_filename <- download_edgar_xbrlfiles(paste0("https://www.sec.gov", filing_urls[1]), "xbrl/") # can create a for loop later for all filing urls
  xml_filename <- "xbrl/brka-20240630_htm.xml"
  #csv_filename <- save_xbrl_tables(xml_file = xml_filename)
  csv_filename <- "xbrl/brka-20240630_htm.csv"
  browser()
  read_arelle_tables(csv_filename)
  # 
  
}

edgar_link_to_filings <- function(cik, form = "10-Q"){
  
  # error checks
  if (form %in% c("10-Q", "10-K")){
    # this is good
  } else {
    cat(paste("form has to be either 10-K or 10-Q, not", form))
    browser()
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

download_edgar_xbrlfiles <- function(url, destination_dir){
    # get links of this specific filing
    page <- download_edgar_file(url)
    page <- rvest::read_html(page)
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
    url_xml <- paste0("https://www.sec.gov", form_link_xml)
    url_xsd <- paste0("https://www.sec.gov", form_link_xsd)
    webdata_xml <- download_edgar_file(url_xml)
    webdata_xsd <- download_edgar_file(url_xsd)
    xml <- webdata_xml |> httr::content("text")
    write(xml, paste0(destination_dir, filename_xml)) #breaks here
    xsd <- webdata_xsd |> httr::content("text")
    write(xsd, paste0(destination_dir, filename_xsd))
    cat(paste(filename_xml, "and", filename_xsd, "downloaded."))
    return(paste0(destination_dir, filename_xml))
}

save_xbrl_tables <- function(xml_file){
  
  filename <- stringr::str_sub(xml_file,start = 1, end = -4)
  filename <- paste0(filename,"csv")
  # arelle_arg <- c("--validate", "--file xbrl/test.xml", "--facts=xbrl/output.json", 
  #                 "--factListCols=Label,Name,contextRef,unitRef,Dec,Prec,Lang,Value",
  #                 "--DTS=xbrl/dtsfile.csv",
  #                 "--factTable=xbrl/facttable.csv",
  #                 "--table=xbrl/table_linkbase.csv",
  #                 "--pre=xbrl/presentation_file.csv")
  arelle_arg <- c("--validate", paste0("--file ", xml_file),
                  "--factListCols=Label,Name,contextRef,unitRef,Dec,Prec,Lang,Value",
                  "--DTS=xbrl/dtsfile.csv",
                  paste0("--factTable=", filename),
                  "--table=xbrl/table_linkbase.csv",
                  "--pre=xbrl/presentation_file.csv")
  system2("/Applications/Arelle.app/contents/MacOS/arelleCmdLine", 
          args = arelle_arg)
  return(filename)
}

read_arelle_tables <- function(filename){

  data <- read.csv(filename)
  
  # find tables and classify them- can make this one function
  i_concept <- which(data[,1] != "")
  concept_names <- data[i_concept,1]
  concept_id <- stringr::str_sub(concept_names,end = 6)
  ind <- stringr::str_locate_all(concept_names, " - ")
  indf <- function(x){
    output <- c(x[1,2] + 1, x[2,1] - 1)
  }
  v <- lapply(X=ind, FUN = indf)
  v <- t(matrix(unlist(v), nrow = 2))
  colnames(v) <- c("start", "end")
  concept_classification <- stringr::str_sub(concept_names, v)
  label <- stringr::str_sub(concept_names, start = v[,"end"] + 4)
  end <- c(i_concept[-1]-1, nrow(data))
  tables <- data.frame(start = i_concept, end = end, class = concept_classification,
                       name = label)
  # select only tables that contain statements
  tables <- tables[tolower(tables$class) == "statement", ]
  # this would be end of first function
  table <- data[tables$start[1]:tables$end[1], ]
  browser()
  indf <- function(x){
    which(x != "")[1]
  }
  hierarchy <- apply(table,1,indf)
  names(hierarchy) <- 1:length(hierarchy)
  label <- table[,1:max(hierarchy)]
  keep <- label != ""
  label <- label[keep]
  data <- table[, (max(hierarchy)+1):ncol(table)]
  data[data ==""] <- NA
  no_obs <- is.na(data)
  keep_col <- apply(no_obs,2,sum) != nrow(data)
  data <- data[,keep]
  date <- stringr::str_extract(colnames(data), "[:digit:]{2,}.[:digit:]{2,}.[:digit:]{2,}")
  date <- lubridate::as_date(date, format = "...")
  date <- stringr::str_replace_all(date, ".", "-")
  # for each table extract information. For now will work with one table first
  # Will take table 1
  
  
  # check if table id is fully number
  # find first and second " - " in between is table classification
  # select concept_classification == tolower("statement")
  # second till end - concept label
  # subset data for 1st statement
  
  # check which columns have at least some data, erase all with only missing data
  # review what is left - in particular look at column names
  # extract period from column names
  # extract entity from column names
  # extract other items from column names?
  # subset data based on column classifications (period, entity etc.)
  
}




################### can ignore everything below ###############
test.download.bkrb <- function(){
  # Work in progress

  url <- "https://www.berkshirehathaway.com/reports.html"
  
  dir_name <- "./rawdata/financial_reports/"
  # Load necessary libraries

  # Define the CIK for Berkshire Hathaway
  cik <- "0001067983"
  type <- "10-Q"
  items_on_page <- "100"
  # Define the URL for the EDGAR search
  url <- paste0("https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=", cik, 
                "&type=", type, "&dateb=&owner=exclude&count=", items_on_page)
  
  # Get the webpage content
  ua <- httr::user_agent("w.buitenhuis@gmail.com")
  page <- httr::GET(url, ua) |> rvest::read_html()
  
  # Extract the links to the 10-Q filings
  index_links <- page |>
    rvest::html_elements("a") |>
    rvest::html_attr("href") 
  index_links_desc <- page |> rvest::html_elements("a") |> rvest::html_text()
  # ind <- grepl("Archives/edgar/data", index_links, value = TRUE)
  ind <- stringr::str_detect(index_links,"Archives/edgar/data")
  index_links <- index_links[ind]
  index_links_desc <- index_links_desc[ind]

  #alternative approach - nneds to be tested
  # https://www.r-bloggers.com/2020/02/a-walk-though-of-accessing-financial-statements-with-xbrl-in-r-part-1/
  # filings <- 
  #   read_html(url) %>%
  #   html_nodes(xpath='//*[@id="seriesDiv"]/table') %>%
  #   html_table() %>%
  #   as.data.frame() %>%
  #   janitor::clean_names()
  # 
  
  

  # Download the first 10-Q filing
  if (length(index_links) > 0) {
    # get links of this specific filing
    Sys.sleep(0.12)
    page <- httr::GET(paste0("https://www.sec.gov", index_links[1]), ua) |> rvest::read_html()
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
    if (length(form_link_txt) != 1) browser()
    if (length(form_link_html) != 1) browser()
    browser()
    date_str <- date_str[is_date & is_htm]
    
    # https://xbrl.us/join-us/membership/individual/
    # https://www.sec.gov/search-filings/edgar-application-programming-interfaces
    # https://www.lexjansen.com/pharmasug-cn/2021/SR/Pharmasug-China-2021-SR031.pdf
    # https://www.r-bloggers.com/2020/02/a-walk-though-of-accessing-financial-statements-with-xbrl-in-r-part-1/
    
    Sys.sleep(0.2)
    url_txt <- paste0("https://www.sec.gov",form_link_txt)
    url_html <- paste0("https://www.sec.gov",form_link_html)
    url_xml <- paste0("https://www.sec.gov",form_link_xml)
    url_xsd <- paste0("https://www.sec.gov",form_link_xsd)
    webdata_txt <- httr::GET(url_txt, httr::user_agent("w.buitenhuis@gmail.com")) 
    Sys.sleep(0.2)
    webdata_html <- httr::GET(url_html, httr::user_agent("w.buitenhuis@gmail.com")) 
    html <- httr::content(webdata_html, "text")
    html <- xml2::read_html(webdata_html) # use java script error
    
    Sys.sleep(0.2)
    webdata_xml <- httr::GET(url_xml, httr::user_agent("w.buitenhuis@gmail.com")) 
    Sys.sleep(0.2)
    webdata_xsd <- httr::GET(url_xsd, httr::user_agent("w.buitenhuis@gmail.com")) 
    # setwd("./xbrl/")
    xml <- webdata_xml |> httr::content("text")
    write(xml, "./xbrl/test.xml")
    xsd <- webdata_xsd |> httr::content("text")
    write(xsd, filename_xsd)
#   test <- XBRL::xbrlDoAll("test.xml") # does not work
    
    browser()
    # parse data
    xbrl_doc <- XBRL::xbrlParse("./xbrl/test.xml")
    schema_name <- XBRL::xbrlGetSchemaName(xbrl_doc)
    xbrl_xsd <- XBRL::xbrlParse(schema_name) # xsd file (xbrl schema)
    
    # read in xbrl doc
    facts <- XBRL::xbrlProcessFacts(xbrl_doc)
    contexts <- XBRL::xbrlProcessContexts(xbrl_doc)
    units <- XBRL::xbrlProcessUnits(xbrl_doc)
    footnotes <- XBRL::xbrlProcessFootnotes(xbrl_doc)
    linkbase <- XBRL::xbrlGetLinkbaseNames(xbrl_doc) # empty
    
    # read in schema file
    labels <- XBRL::xbrlProcessLabels(xbrl_xsd)
    elements <- XBRL::xbrlProcessElements(xbrl_xsd)
    roles <- XBRL::xbrlProcessRoles(xbrl_xsd)
    importnames <- XBRL::xbrlGetImportNames(xbrl_xsd)
    
    for (i in 1:length(importnames)){
      cat(importnames[i])
      ind <- stringr::str_locate_all(importnames[i], "/")[[1]] |> xts::last()
      ind <- ind[1,1] + 1
      filename <- stringr::str_sub(importnames[i], start = ind)
      webdata <- httr::GET(importnames[i])
      httr::warn_for_status(webdata)
      import_xsd <- httr::content(webdata, "text")
      write(import_xsd, file = paste0("./xbrl/xbrl.Cache/", filename))
    }
    browser()
    # arcs <- XBRL::xbrlProcessArcs(xbrl_doc) breaks argument arcType is missing
    
    # dir <- getwd()
    # setwd("./xbrl/")
    # XBRL::xbrlDoAll("test.xml", cache.dir = "xbrl.Cache", verbose = TRUE, 
    #                 delete.cached.inst = FALSE)
    # # breaks when trying. to download a https:// file which is already in the cache dir
    # setwd(dir)
    # # XBRL::xbrlFree(xbrl_doc)
    
    system2("/Applications/Arelle.app/contents/MacOS/arelleCmdLine", 
            args = c("--about","--save-loadable-excel"))
    
    arelle_arg <- c("--validate", "--file xbrl/test.xml", "--facts=xbrl/facttable.csv")
    arelle_arg <- c("--validate", "--file xbrl/test.xml", "--facts=xbrl/output.json", 
                    "--factListCols=Label,Name,contextRef,unitRef,Dec,Prec,Lang,Value",
                    "--DTS=xbrl/dtsfile.csv",
                    "--factTable=xbrl/facttable.csv",
                    "--table=xbrl/table_linkbase.csv",
                    "--pre=xbrl/presentation_file.csv")
    system2("/Applications/Arelle.app/contents/MacOS/arelleCmdLine", 
            args = arelle_arg)
    
    # python arelleCmdLine.py --validate --file="yourfile.xbrl" --output="output.json" --tables
    
    
    # arelleCmdLine -f c:\temp\test.rss -v --disclosureSystem efm-pragmatic-all-years --store-to-XBRL-DB "localhost\SQLEXPRESS,,sqlLogin,sqlPassword,,90,mssqlSemantic"
    # python arelle.py --validate --file="yourfile.xbrl" --output="output.csv"

    ind <- which(facts$elementId == "us-gaap_WeightedAverageNumberOfSharesOutstandingBasic") 
    ind <- stringr::str_detect(facts$elementId, "NumberOfSharesOutstanding")
    unique(facts$elementId[ind])
    
    # b <- chromote::ChromoteSession$new()
    # b$Network$setUserAgentOverride(userAgent = "w.buitenhuis@gmail.com")
    # webdataLIVE_html <- rvest::read_html_live(url_html) # enable javascript
    # # the html version seems to only show the java script, not the html being populated.
    # the $view() method does show the javascript generated page though.
    # Sys.sleep(6.5)
    # webdataLIVE_xml <- rvest::read_html_live(url_xml) 
    # x <- webdataLIVE_xml |> rvest::html_text()
    
    Sys.sleep(0.2)
    webdata <- httr::GET(url_xml, httr::user_agent("w.buitenhuis@gmail.com"))
    useragent <- webdata$request$options$useragent
    httr::warn_for_status(webdata)
    # bin <- httr::content(webdata, "raw")
    text <- httr::content(webdata_txt, "text")
    
    webdataLIVE_txt <- rvest::read_html_live(url_txt) 
    # need to use read_html_live (using chromite, otherwise get 403 error
    text <- webdataLIVE_txt$html_elements((css = "pre")) |> rvest::html_text()
    # all relevant text is part of one big pre tag. this effectively removes the tag
    writeLines(text, "./output/test.txt")
    # we now get an html doc
    # html <- rvest::read_html( "./output/test.txt")
    
    tables <- html |> rvest::html_element("table")
    x <- tables |> rvest::html_table()
    # look for correct file to download
    
    # download_url <- paste0("https://www.sec.gov", links[1])
    # debugonce(download_edgar_file)
    # download_edgar_file(download_url, destfile = paste0(dir_name, "Berkshire_Hathaway_10Q.pdf"))
    # cat("Downloaded:", download_url, "\n")
  } else {
    cat("No 10-Q filings found.\n")
  }
  
  browser()
  
  
  file.list <- list.files(dir_name)
  save_dt <- file.info(paste0(dir_name, file.list))$atime
  last_file <- file.list[which(save_dt == max(save_dt))[1]]

}


