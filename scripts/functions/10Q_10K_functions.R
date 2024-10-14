
download.file <-function(URL, origination.file, destination, destination.file)
{
  if (missing(destination.file)) destination.file <- origination.file
  if (grepl(".csv", destination.file, fixed = TRUE))
  {
    # in some cases need to download file with extension .csv-dl, while want to save .csv
    # this is case with CBOE volume data
    n <- stringr::str_locate(destination.file, ".csv")[, "end"]
    destination.file <- substr(destination.file, 1, n)
  }
  destination.file <- paste0(destination, destination.file)
  URL <- paste0(URL, origination.file)
  webdata <- httr::GET(URL)
  useragent <- webdata$request$options$useragent
  httr::warn_for_status(webdata)
  bin <- httr::content(webdata, "raw")
  if (httr::http_status(webdata)$category == "Success") 
  {
    writeBin(bin, destination.file)
    n <- nchar(origination.file)
    if (tolower(substr(origination.file, n - 3, n)) == ".zip")
    {
      utils::unzip(destination.file, exdir = destination, setTimes = TRUE)
    }  
  }
  else
  {
    warning(paste(URL, "Did not download."))
  }
  return(httr::http_status(webdata))
}

download_edgar_file <- function(url, destfile){
    # download files from edgar SEC website
    # sets user agent
    # ensure not to violate download limits
    ua <- httr::user_agent("w.buitenhuis@gmail.com")
    Sys.sleep(0.12)
    webdata <- httr::GET(url, ua)
    httr::warn_for_status(webdata)
    bin <- httr::content(webdata, "raw")
    # if (httr::http_status(webdata)$category == "Success") writeBin(bin, destfile)
    return(bin)
}

download.bkrb.quarterly.earnings <- function(){
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
    write(xml, "test.xml")
    xsd <- webdata_xsd |> httr::content("text")
    write(xsd, filename_xsd)
#   test <- XBRL::xbrlDoAll("test.xml") # does not work
    
    browser()
    xbrl_doc <- XBRL::xbrlParse("test.xml")
    facts <- XBRL::xbrlProcessFacts(xbrl_doc)
    contexts <- XBRL::xbrlProcessContexts(xbrl_doc)
    units <- XBRL::xbrlProcessUnits(xbrl_doc)
    labels <- XBRL::xbrlProcessLabels(xbrl_doc)# nothing
    elements <- XBRL::xbrlProcessElements(xbrl_doc)# nothing
    roles <- XBRL::xbrlProcessRoles(xbrl_doc) # nothing
    footnotes <- XBRL::xbrlProcessFootnotes(xbrl_doc)
    schema_name <- XBRL::xbrlGetSchemaName(xbrl_doc)
    linkbase <- XBRL::xbrlGetSchemaName(xbrl_doc)
    importnames <- XBRL::xbrlGetImportNames(xbrl_doc) # empty
    # arcs <- XBRL::xbrlProcessArcs(xbrl_doc) breaks argument arcType is missing
    
    # XBRL::xbrlFree(xbrl_doc)
    
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


