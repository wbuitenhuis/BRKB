
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
  # Download monthly statistics from Interactive Brokers investor relations website
  # Convert pdf document in R readible form
  library(xts)
  extract_lineitem_data <- function(data, line_item, dates)
  {
    slct <- grepl(line_item, data)
    if (sum(slct) == 0) {
      browser()
      #Interactive Brokers may have changed name of line items or deleted line item.
    }
    data1 <- data[slct]
    data1 <- stringr::str_replace(data1, line_item, "#")
    if (line_item == "Cleared Avg. DART per Account")
    {
      # browser()
      slct <- grepl("(Annualized)", data)
      data1 <- data[slct]
      data1 <- stringr::str_replace(data1, "(Annualized)", "#")
    }
    # data1 <- stringr::str_remove_all(data1, line_item)
    data1 <- stringr::str_replace_all(data1, "\\s{1,}", ";") #replace multiple spaces by semi colon
    data1 <- stringr::str_replace_all(data1, ",", "") #remove comma' s
    data1 <- stringr::str_replace_all(data1, "\\$", "") #remove $ signs
    data1 <- stringr::str_split(data1, ";")
    ind.not.numbers <- stringr::str_detect(data1[[1]], "%") # breaks here
    data1[[1]][ind.not.numbers] <- NA
    n.spaces <- unlist(lapply(lapply(data1, stringr::str_detect, " "), sum, na.rm = TRUE))
    if (max(n.spaces) > 0)
    {
      # have a year with spaces inside observations (typically last year)
      ind <- which(n.spaces > 0)
      for (i in 1:length(ind))
      {
        x <- str_split(data1[[ind[i]]], " ")
        n.obs.before <- length(data1[[ind[i]]])
        ind2 <- which(unlist(lapply(x, length)) > 1)
        for (j in 1:length(ind2))
        {
          add.obs <- x[[ind2[j]]]
          n.add.obs <- length(add.obs)
          data1[[ind[i]]][ind2[j]:(ind2[j] + n.add.obs - 1)] <- add.obs
          # one known instance this solves a problem.
          # can imagine situations where it create other problems
        }
      }
    }
    n.obs <- unlist(lapply(data1, length))
    if (max(n.obs) > 13)
    {
      # message("More than 12 observations in a year")
      ind <- which(n.obs > 13)
      for (i in 1:length(ind))
      {
        data1[[ind[i]]] <- data1[[ind[i]]][1:13]
      }
      # browser()
    }
    if (min(n.obs) < 13)
    {
      ind <- which(n.obs < 13)
      for (i in 1:length(ind))
      {
        n.NA <- 13 - n.obs[ind[i]]
        data1[[ind[i]]] <- c(data1[[ind[i]]], rep(NA, n.NA))
      }
    }
    #Should have 12 obsservations per year now.
    # order old to new
    n.years <- length(data1)
    data1 <- data1[n.years:1]
    data1 <- unlist(data1)
    keep <- !grepl("#", data1)
    data1 <- data1[keep]
    data1 <- as.numeric(data1)
    # data1 <- t(matrix(as.numeric(unlist(data1)), nrow = 12, ncol = n.years))
    data1 <- xts(as.vector(t(data1)), order.by = dates)
    names(data1) <- line_item
    return(data1)
  }
  
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
    #rvest::html_nodes("a") |>
    rvest::html_elements("a") |>
    rvest::html_attr("href") 
  index_links <- grep("Archives/edgar/data", index_links, value = TRUE)
  
  # Download the first 10-Q filing
  if (length(index_links) > 0) {
    # get links of this specific filing
    Sys.sleep(0.12)
    page <- httr::GET(paste0("https://www.sec.gov", index_links[1]), ua) |> rvest::read_html()
    form_links <- page |>
      rvest::html_nodes("a") |>
      rvest::html_attr("href") 
    # find the correct link with the html filing 
    form_links <- grep("Archives/edgar/data", form_links, value = TRUE)
    form_links <- form_links[stringr::str_detect(form_links,"-")]
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
    form_link <- form_links[is_date & is_htm]
    if (length(form_link) != 1) browser()
    browser()
    date_str <- date_str[is_date & is_htm]
    
    # 00000webdata <- download_edgar_file(paste0("https://www.sec.gov",form_link))
    
    ua <- httr::user_agent("w.buitenhuis@gmail.com")
    Sys.sleep(0.12)
    webdata <- httr::GET(paste0("https://www.sec.gov",form_link), ua)
    html <- rvest::read_html(webdata)
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

mktx_stats <- function(){
  
  library(xts)
  url <- "https://investor.marketaxess.com/trading-data/default.aspx"
  # domain <- "https://www.marketaxess.com/"
  dir_name <- "./rawdata/marketaxess/"
  href <- rvest::read_html(url)|> rvest::html_elements("a")|> rvest::html_attr("href")
  excel_links <- stringr::str_detect(href, ".xlsx")
  href <- href[excel_links]
  href <- href[!is.na(href)]
  url <- href[stringr::str_detect(tolower(href), "monthly-volume")]
  ind <- stringr::str_locate_all(url, "/")[[1]][, 1] |> xts::last()
  file_name <- stringr::str_sub(url, start = ind + 1)
  url <- stringr::str_sub(url, end = ind)
  url <- paste0("https:", url)
  download.file(URL = url, origination.file = file_name, destination = dir_name, 
                destination.file = file_name)
  
  file.list <- list.files(dir_name)
  save_dt <- file.info(paste0(dir_name, file.list))$atime
  last_file <- file.list[which(save_dt == max(save_dt))[1]]
  
  excel_data <- readxl::read_excel(path = paste0(dir_name, last_file),
                                   sheet = "Market Volumes - Monthly")
  
  dates <-  excel_data[1, ]
  dates <- dates[,-1] |> as.numeric()
  dates <- lubridate::as_date(dates, origin = "1899-12-30")
  excel_data <- excel_data[-1, ]
  varnames <- excel_data[,1][[1]]
  trace_MKTX <- excel_data[, -1]
  trace_MKTX[trace_MKTX == "n/a"] <- NA
  trace_MKTX <- t(trace_MKTX)
  trace_MKTX <- matrix(as.numeric(trace_MKTX), ncol = length(varnames))
  colnames(trace_MKTX) <- varnames
  keep <- which(varnames %in% c("U.S. High-Grade TRACE",
                                "U.S. High-Yield TRACE"))
  keep <- keep[3:4]
  keep <- c(keep, which(varnames %in% "Eurobonds")[2] + 1)
  trace_MKTX <- trace_MKTX[, keep]
  colnames(trace_MKTX) <- c("HG_volume", "HY_volume", "EURO_credit")
  trace_MKTX <- xts(x=trace_MKTX, order.by = as.yearmon(dates))
  
  excel_data <- readxl::read_excel(path = paste0(dir_name, last_file),
                                   sheet = "MKTX - Monthly")
  
  dates <-  excel_data[3, ]
  dates <- dates[,-1] |> as.numeric()
  dates <- lubridate::as_date(dates, origin = "1899-12-30")
  excel_data <- excel_data[c(-1,-2,-3), ]
  varnames <- excel_data[,1][[1]]
  MKTX_data <- excel_data[, -1]
  MKTX_data[MKTX_data == "n/a"] <- NA
  MKTX_data <- t(MKTX_data)
  MKTX_data <- matrix(as.numeric(MKTX_data), ncol = length(varnames))
  colnames(MKTX_data) <- varnames
  keep <- which(varnames %in% c("U.S. High-Grade",
                                "U.S. High-Yield"))
  keep <- keep[3:4]
  keep <- c(keep, which(varnames %in% "Eurobonds")[2] + 1)
  MKTX_data <- MKTX_data[, keep]
  colnames(MKTX_data) <- c("HG_volume", "HY_volume", "EURO_credit")
  MKTX_data <- xts(x=MKTX_data, order.by = as.yearmon(dates))
  
  save(trace_MKTX, MKTX_data, file = "./RData/MKTX_stats.RData")
}
