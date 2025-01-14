
#' @name xbrl_data_aapl2014
#' @title XBRL data from SEC archive
#' @description parsed XBRL files from 
#'    http://edgar.sec.gov/Archives/edgar/data/320193/000119312514383437/aapl-20140927.xml
#'    using XBRL::xbrlDoAll
#'    and truncating to only necessary data
#' @usage data(xbrl_data_aapl2014) 
#' @docType data
NULL

#' @name xbrl_data_aapl2013
#' @title XBRL data from SEC archive
#' @description parsed XBRL files from 
#'    http://edgar.sec.gov/Archives/edgar/data/320193/000119312513416534/aapl-20130928.xml
#'    using XBRL::xbrlDoAll
#'    and truncating to only necessary data
#' @usage data(xbrl_data_aapl2013) 
#' @docType data
NULL

#' Function to create package data included in the package
#' 
#' @details xbrlDoAll creates > 5Mb list - we need only 2Mb
#' @keywords internal
xbrl_create_data <-function() {

  file1 <- "http://edgar.sec.gov/Archives/edgar/data/320193/000119312513416534/aapl-20130928.xml"
  file2 <- "http://edgar.sec.gov/Archives/edgar/data/320193/000119312514383437/aapl-20140927.xml" 

  xbrl_data_aapl2013 <- XBRL::xbrlDoAll(file1)
  xbrl_data_aapl2014 <- XBRL::xbrlDoAll(file2)
  
  
  xbrl_data_aapl2013$unit <- NULL
  xbrl_data_aapl2013$footnote <- NULL
  xbrl_data_aapl2013$definition <- NULL
  xbrl_data_aapl2013$presentation <- NULL
  xbrl_data_aapl2013$element <- 
    xbrl_data_aapl2013$element %>%
    dplyr::semi_join(xbrl_data_aapl2013$fact, by="elementId" )
  
  xbrl_data_aapl2014$unit <- NULL
  xbrl_data_aapl2014$footnote <- NULL
  xbrl_data_aapl2014$definition <- NULL
  xbrl_data_aapl2014$presentation <- NULL
  xbrl_data_aapl2014$element <- 
    xbrl_data_aapl2014$element %>%
    dplyr::semi_join(xbrl_data_aapl2014$fact, by="elementId" )
  
  # devtools::use_data(xbrl_data_aapl2013, overwrite = T)
  # devtools::use_data(xbrl_data_aapl2014, overwrite = T)
  
}


finstr_cols <- function(x = NULL, inverse = FALSE) {
  #cols <- c("contextId", "startDate", "endDate", "decimals")
  cols <- c("contextId", "startDate", "endDate", "decimals", "value1")
  if(!missing(x)) {
    if(inverse){
      cols <- names(x)[!names(x) %in% cols]
    } else {
      cols <- names(x)[names(x) %in% cols]
    }
  }
  return(cols)
}

finstr_ncol <- function() length (finstr_cols())
finstr_values <- function(x) (x[(finstr_ncol()+1):ncol(x)])

  
#' Get a vector of statement IDs
#' @param xbrl_vars XBRL data
#' @importFrom magrittr "%>%"
#' @keywords internal
#' @export
xbrl_get_statement_ids <- function(xbrl_vars) {
  # finds roleIds for statements
  # example:
  #   xbrl_get_statements(xbrl_vars)
  
  if( !all( c("role", "calculation") %in% names(xbrl_vars)))
    stop(substitute(xbrl_vars), " does not include role and calculation data.")
  # browser()
  xbrl_vars$role |>
    dplyr::filter(type == "Statement") |>
    dplyr::semi_join(xbrl_vars$calculation, by = "roleId") |>
    dplyr::select(roleId) |>
    simplify2array() |>
    unname()
}
xbrl_get_statement_ids_WB <- function(xbrl_vars) {
  # finds roleIds for statements
  # example:
  #   xbrl_get_statements(xbrl_vars)
  
  if( !all( c("role", "calculation") %in% names(xbrl_vars)))
    stop(substitute(xbrl_vars), " does not include role and calculation data.")
  xbrl_vars$role |>
    dplyr::filter(type == "Statement") |>
    dplyr::semi_join(xbrl_vars$calculation, by = "roleId") |>
    dplyr::select(roleId, description, definition)
}


#' Get a statement from data (data for specified elements)
#' @param elements elements object
#' @param xbrl_vars XBRL data
#' @param complete_only just the rows without NA
#' @param complete_first just the rows without NA in first column
#' @param basic_contexts in case of duplicated periods, get only basic contexts
#' @keywords internal
#' @export
xbrl_get_data <- function(elements, xbrl_vars, 
                          complete_only = FALSE, complete_first = TRUE, 
                          basic_contexts = TRUE) {
  # gets data in normal format (with variables as columns and 
  # time periods as rows)

  if( !("data.frame" %in% class(elements)) )
    elements <- data.frame(elementId = elements, stringsAsFactors = FALSE)
  #browser()
  res <-
    elements |>
    dplyr::inner_join(xbrl_vars$fact, by = "elementId")

  min_level <- min(res$level, na.rm = TRUE)

  min_dec <- min(as.numeric(res$decimals), na.rm = TRUE)
  
  # context_filter <- res |> dplyr::filter(level == min_level) |>
  #   getElement("contextId") |> unique()
  context_filter <- res |> 
    getElement("contextId") |> unique()
  # decimals_filter <- res |> dplyr::filter(level == min_level) |>
  #   getElement("decimals") |>  unique()
  decimals_filter <- res |> 
    getElement("decimals") |>  unique()
  
  res <-
    res |>
    dplyr::filter(contextId %in% context_filter)
    res <- res |>
      dplyr::filter(decimals %in% decimals_filter)
    res <- res |>
      dplyr::mutate(fact = as.numeric(fact), decimals = min_dec )
    res <- res |>
      dplyr::inner_join(xbrl_vars$context, by = "contextId")
    res <- res |>
      dplyr::select(contextId, startDate, endDate, elementId, fact, decimals, value1)
    res <- res |> tidyr::pivot_wider(names_from = "elementId", values_from = "fact") 
    res <- res |> dplyr::arrange(endDate)
  

  vec1 <- elements$elementId[! elements$elementId %in% names(res)] 
  # what if this is empty? all elementIds are in names(res)
  df1 <- stats::setNames( data.frame(rbind(rep(0, length(vec1)))), vec1)
  res <- cbind(res, df1)
  
  value_cols <- finstr_cols(res, inverse = TRUE)
  
  #res <- res[, c(names(res)[1:4], elements$elementId)]
  res <- res[, c(finstr_cols(res), elements$elementId)]

  # Handling strange NAs - if some columns are total NA:
  empty_cols <- sapply(
    res[elements$elementId], function(x) length(stats::na.omit(x))==0 
  )
  res[, names(empty_cols)[ empty_cols]] <- 0
  
  # keep only complete rows
  if(complete_only)
    res <- res[stats::complete.cases( res[ value_cols ] ), ]
  # only basic_contexts
  if(basic_contexts) {
    context_filter2 <-
      res |>
      dplyr::group_by(startDate, endDate) |>
      # dplyr::summarise(min_context = contextId[nchar(contextId) == min(nchar(contextId))]) |>
      dplyr::reframe(min_context = contextId[nchar(contextId) == min(nchar(contextId))]) |>
      getElement("min_context")
    
    res <- res |> dplyr::filter(contextId %in% context_filter2)
  }
  
  if(complete_first) # this creates problem in BRKB balance sheet if TRUE
    res <- res[!is.na(res[, value_cols[1]]), ]
  
  if(any(duplicated(res$endDate))) {
    #warning("Rows with duplicated endDate")
    rownames(res) <- res$contextId
  } else {
    rownames(res) <- res$endDate
  }
  
  class(res) <- c("statement", "data.frame")
  return(res)
}

# returns the end of quaerter dates in a vector of dates
quarters <- function(dates){
  
  range <- c(min(dates), max(dates))
  years <- as.integer(stringr::str_sub(range, 1, 4))
  years[2] <- years[2] + 1
  quarters <- try(seq.Date(from = as.Date(paste(years[1], "-01-01", sep = "")), 
                           to = as.Date(paste(years[2], "-12-31", sep = "")), 
                           by = "quarter"))
  quarters <- quarters - lubridate::days(1)
  return(quarters)
}

# Need to know if it is an annual or quarterly report
# Need to know which quarter, if quarterly report
# From these two facts determine if relevant periods are:
# 1.) full year, or 2.) Last quarter and year to date. Calculate year to date as well.
# Check number of observations for 1.) or 2.). Select 1.) or 2.)
# current approach does not work if reporting year is not a calendar year
# also may have issues with new companies after ipo
relevant_periods <- function(startDt, endDt){
  # for balance sheet, no starting dates, for 10K current period and previous year
  # for 10Q current quarter, start of year
  # for income statement and comprehensive income, 10K two periods, current year and previous year
  # 10 Q current quarter, current quarter last year, ytd, and ytd previous year.
  # for cash flow statements: 10K 10K two periods, current year and previous year
  # for 10Q also two periods: YTD and YTD previous year
  # Approach is to find this quarter, last quarter, ytd, this quarter previous year, ytd last year,
  # next check which of those are in startDt and endDt, and sort by frequency, pick n
  startDt <- as.Date(startDt)
  endDt <- as.Date(endDt)
  dates <- data.frame(startDt = startDt, endDt = endDt)
  gs <- dates |> dplyr::group_by(startDt, endDt) |> summarise(n = dplyr::n()) |> 
    dplyr::arrange(dplyr::desc(n), dplyr::desc(endDt), dplyr::desc(startDt))
  if (sum(is.na(startDt)) == length(startDt)){
    # no start dates - has to be balance sheet
    statement <- "BS"
  } else {
    statement <- "other"
  }
  gs$year <- lubridate::year(as.Date(gs$endDt))
  gs$month <- lubridate::month(as.Date(gs$endDt))
 
  i.year <- which.max(gs$year[1:min(nrow(gs), 4)])
  year <- gs$year[i.year[1]]
  month <- max(gs$month[i.year])
    
  endofquarter <- 
    lubridate::ceiling_date(as.Date(paste0(year, "-", month, "-01")), 
                            "month") - lubridate::days(1)
  startofquarter <- lubridate::floor_date(endofquarter, "quarter")
  
  periods <- data.frame(startDt = as.Date("1900-01-01"), 
                        endDt = as.Date("1900-01-01"))
  periods <- rbind(periods, c(NA, endofquarter))
  periods <- rbind(periods, c(NA, as.Date(paste0(year-1,"-12-31"))))
  periods <- rbind(periods, c(startofquarter, endofquarter))
  periods <- rbind(periods, c(as.Date(paste0(year, "-01-01")), endofquarter)) #YTD
  oneyear <- data.frame(startDt = rep(lubridate::years(1), 2), 
                        endDt = rep(lubridate::years(1), 2))
  periods <- rbind(periods, periods[4:5, ] - oneyear)
  
  periods <- periods[-1, ]
  if (statement != "BS"){
    periods <- periods[-2:-1, ]
  }
  # check if date pairs are in original date
  res <- dplyr::inner_join(periods, gs, by = colnames(periods))
  #check <- dplyr::inner_join(res, gs[1:nrow(res), ], by = colnames(res))
  #if (nrow(check) != nrow(res)) browser()
    # did not take most frequent date combo - this is unexpected. Need to verify
  res <- res |> dplyr::arrange(dplyr::desc(endDt), desc(startDt))
  res <- res[,c("startDt", "endDt")]
  res <- apply(res, 2, as.character)
  if (class(try(res[, "startDt"])) == "try-error") browser()
  #browser()
  res <- data.frame(res)
  return(res)
}

last_char_is_num <- function(string){
  # checks is last char of string is a digit and 2nd to last is not a digit
  last_char <- stringr::str_sub(string, start = -1)
  second_to_last <- stringr::str_sub(string, start = -2, end = -2)
  is_last_digit <- last_char |>
    stringr::str_detect("[:digit:]")
  is_2nd_to_last_digit <-  second_to_last  |>
    stringr::str_detect("[:digit:]{4}-[:digit:]{2}-[:digit:]{2}")
  return(is_last_digit & !is_2nd_to_last_digit)
}

#' Get a statement from data (data for specified elements)
#' @param elements elements object
#' @param xbrl_vars XBRL data
#' @param complete_only just the rows without NA
#' @param complete_first just the rows without NA in first column
#' @param basic_contexts in case of duplicated periods, get only basic contexts
#' @keywords internal
#' @export
xbrl_get_data_WB <- function(elements, xbrl_vars, 
                          complete_only = FALSE, complete_first = TRUE, 
                          basic_contexts = TRUE, nr_periods = 1, 
                          end_of_quarter = FALSE, 
                          regular_sec_reporting_periods = TRUE,
                          nonzero_only = FALSE,
                          aggregate_over_period_and_entity = TRUE,
                          filter_members = FALSE) {
  # gets data in normal format (with variables as columns and 
  # time periods as rows)
  remove_all_zeros_nas <- function(df) { df[rowSums(df != 0 & !is.na(df)) > 0, ] }
  # browser()
  if( !("data.frame" %in% class(elements)) )
    elements <- data.frame(elementId = elements, stringsAsFactors = FALSE)
  # browser()
  res <-
    elements |>
    dplyr::inner_join(xbrl_vars$fact, by = "elementId")
  if (nrow(res) == 0){
    ret <- "no matching facts in elements"
    class(ret) = "try-error"
    return(ret)
  }  
  # this filter does not work for Berkshire income statement
  min_level <- min(res$level, na.rm = TRUE)
  min_dec <- min(as.numeric(res$decimals), na.rm = TRUE)
  # context_filter <- res |> dplyr::filter(level == min_level) |>
  #   getElement("contextId") |> unique()
  context_filter <- res |>
    getElement("contextId") |> unique() 
  decimals_filter <- res |> dplyr::filter(level == min_level) |>
    getElement("decimals") |>  unique()
  res <-
    res |>
    dplyr::filter(contextId %in% context_filter)
  res <- res |>
    dplyr::filter(decimals %in% decimals_filter)
  res <- res |>
    dplyr::mutate(fact = as.numeric(fact), decimals = min_dec )
  res <- res |>
    dplyr::inner_join(xbrl_vars$context, by = "contextId")
  
  # work to filter non relevant reporting periods
  # Need to know if it is an annual or quarterly report
  # Need to know which quarter, if quarterly report
  # From these two facts determine if relevant periods are:
  # 1.) full year, or 2.) Last quarter and year to date. Calculate year to date as well.
  # Check number of observations for 1.) or 2.). Select 1.) or 2.)
  
  # browser()
  if (regular_sec_reporting_periods){
    periods <- relevant_periods(res$startDate, res$endDate)
    periods_filter <- paste(periods[, "startDt"], periods[, "endDt"])
    nr_periods <- min(nr_periods, length(periods_filter))
    periods_filter <- periods_filter[1:nr_periods]
    res$filter_by <- paste(res$startDate, res$endDate)
    res <- res |> dplyr::filter(filter_by %in% periods_filter)
  } else {
    gs <- res |> dplyr::group_by(startDate, endDate) |> summarise(n = dplyr::n())
    gs$dates <- paste(gs$startDate, gs$endDate)
    res$dates <- paste(res$startDate, res$endDate)
    ind <- match(res$dates, gs$dates)
    res$n <- gs$n[ind]
    res$date <- as.Date(res$endDate)
    if (end_of_quarter){
      filter_quarters <- quarters(res$date)
      filter_quarters <- filter_quarters[filter_quarters %in% c(startDates, endDates)]
    } else {
      filter_quarters <- unique(res$date)
    }
    res$periodLength <- as.numeric(res$date - as.Date(res$startDate))
    res$periodLength <- sprintf("%03d", as.numeric(res$periodLength)) # add leading zeros
    res <- res |> dplyr::arrange(dplyr::desc(endDate), periodLength, dplyr::desc(n))
    #res$filter_by <- paste(res$endDate, res$PeriodLength, res$n)
    res <- res |> dplyr::filter(date %in% filter_quarters)
    res$filter_by <- paste(res$endDate, res$periodLength)
    nr_periods <- min(nr_periods, length(unique(res$filter_by)))
    dates_filter <- unique(res$filter_by)[1:nr_periods]
    res <- res |> dplyr::filter(filter_by %in% dates_filter)
  }
  res <- res |>
    dplyr::select(contextId, startDate, endDate, elementId, fact, decimals, value1)
  res <- res |> tidyr::pivot_wider(names_from = "elementId", values_from = "fact") 
  res <- res |> dplyr::arrange(endDate)
  if (filter_members){
    member_filter <- !(stringr::str_detect(res$value1, "us-gaap:") | 
                         stringr::str_detect(res$value1, "srt:")) | 
      is.na(res$value1)
    res <- res[member_filter, ]
  }
  
  vec1 <- elements$elementId[! elements$elementId %in% names(res)] 
  # what if this is empty? all elementIds are in names(res)
  df1 <- stats::setNames( data.frame(rbind(rep(0, length(vec1)))), vec1)
  res <- cbind(res, df1)
  
  value_cols <- finstr_cols(res, inverse = TRUE)
  res <- res |> dplyr::mutate(dplyr::across(dplyr::any_of(value_cols), as.numeric))
  #res <- res |> dplyr::mutate(dplyr::any_of(value_cols), as.numeric)

  res <- res[, c(finstr_cols(res), elements$elementId)]
  
  # Handling strange NAs - if some columns are total NA:
  empty_cols <- sapply(
    res[elements$elementId], function(x) length(stats::na.omit(x))==0 
  )
  res[, names(empty_cols)[ empty_cols]] <- 0
  
  # keep only complete rows
  if(complete_only)
    res <- res[stats::complete.cases( res[ value_cols ] ), ]
  
  # remove rows with only zero's or NA's
  if(nonzero_only){
    res <- remove_all_zeros_nas(res)
  }
  # only basic_contexts
  if(basic_contexts) {
    # This only select context where the context ID has the length of the smallest context
    # It gives issues for 2022-06-30 statement of BRKB if this is TRUE
    context_filter2 <-
      res |>
      dplyr::group_by(startDate, endDate) |>
      # dplyr::summarise(min_context = contextId[nchar(contextId) == min(nchar(contextId))]) |>
      dplyr::reframe(min_context = contextId[nchar(contextId) == min(nchar(contextId))]) |>
      getElement("min_context")
    res <- res |> dplyr::filter(contextId %in% context_filter2)
  }
  
  if(complete_first) # this creates problem in BRKB balance sheet if TRUE
    res <- res[!is.na(res[, value_cols[1]]), ]
  
  if(any(duplicated(res$endDate))) {
    #warning("Rows with duplicated endDate")
    rownames(res) <- res$contextId
  } else {
    rownames(res) <- res$endDate
  }
  
  class(res) <- c("statement", "data.frame")
  return(res)
}



xbrl_get_elements <- function(xbrl_vars, relations) {
  if(nrow(relations) == 0) {
    return(NULL)
  }
  # get elements & parent relations & labels
  elements <-
    data.frame( 
      elementId = with(relations, unique(c(fromElementId, toElementId))),
      stringsAsFactors = FALSE
      )  |>
    dplyr::left_join(xbrl_vars$element, by = c("elementId")) |>
    #dplyr::filter_(~type == "xbrli:monetaryItemType") %>% 
    dplyr::left_join(relations, by = c("elementId" = "toElementId")) |>
    dplyr::left_join(xbrl_vars$label, by = c("elementId")) |>
    dplyr::filter(labelRole == "http://www.xbrl.org/2003/role/label") |> # why filter with this labelRole?
    dplyr::transmute(elementId, parentId = fromElementId, order, balance, labelString)
  
  # debugonce(get_elements_h)
  elements <- get_elements_h(elements)
  
  class(elements) <- c("elements", "data.frame")
  return(elements)
}


#' Get elements hierarchy 
#' 
#' @details Add level and hierarchical id columns
#' @param elements data.frame with elementId, parentId and order columns 
#' @export
#' @keywords internal
get_elements_h <- function(elements) {
  # reorder and classify elements by hierarchy
  # adds level, hierarchical id and terminal column
  # browser()
  # if (elements$elementId |> unique() |> length() < nrow(elements)) browser()
  # temp0 <- elements
  level <- 1
  df1 <- elements |> dplyr::filter(is.na(parentId)) # What if all observations do have a parent ID?
  df1 <- df1 |> dplyr::mutate(id = "") # add id variable
  df1 <- df1 |> dplyr::arrange(dplyr::desc(balance)) # sort by decreasing balance value

  if (nrow(df1) == 0) browser()
  # all observations have a parent ID
  # this means there is no element on level 1
  # code breaks
  
  while({
    level_str <- 
      unname(unlist(lapply(split(df1$id, df1$id), function(x) {
        sprintf("%s%02d", x, 1:length(x))
      }))) # creates new id like "0103" - I think this is later used to order rows in statement in correct hierarchy
    if (is.null(level_str)) browser()
    elements[elements$elementId %in% df1$elementId, "level"] <- level
    to_update <- elements[elements$elementId %in% df1$elementId, "elementId"]
    # if (nrow(df1) > nrow(elements)) browser()
    
      # find position of for elements$elementID in df1$elementId, sort them in 
      # ascending order. Any missing values will be put last. It takes only
      # the number of elements equal to length of level_str or number of rows in df1, 
      # so you have
      # one match for each level_str value. Issues may arise if the number of non missing
      # values in the match function exceeds the length of level_str. Now we
      # have only missing values to work with.
      # This can happen if the length of level_str is longer then the number of 
      # rows in elements. However, how can this happen? Seems wrong?
    elements[ 
        #order(match(elements$elementId, to_update))[1:length(level_str)], 
        order(match(elements$elementId, df1$elementId))[1:length(level_str)], 
        "id"] <- level_str
    
    # temp1 <- df1
    # take all elements whose parent ID is an element of df1
    # The new df1 are all children of the old df1
    df1 <- elements |>
      dplyr::filter(parentId %in% df1$elementId) |>
      dplyr::arrange(order)

    # take elementId from df1 (the new child), match with parentId from elements. 
    # - get parent info of the child.
    # if multiple elements share the same parentID in elements each will create a new
    # row in the joint. If parentId is not in df1, it will show missing values 
    # for the variables that have been joined.
    # temp2 <- df1
    df1 <- df1 |>
      dplyr::select(elementId, parentId) |>
      dplyr::left_join(elements, by=c("parentId"="elementId")) |> 
      dplyr::arrange(id)
    nrow(df1) > 0})
  {
    level <- level + 1
  }

  elements <- 
    elements |>
    dplyr::arrange(id) |>
    dplyr::mutate( terminal = !elementId %in% parentId )
  #browser()
}

#' Get relations from XBRL calculation link base
#' @param xbrl_vars XBRL data (list of dataframes)
#' @param role_id id of role (usually of type statement)
#' @param lbase link base (default is calculation)
#' @keywords internal
#' @export
xbrl_get_relations <- function(xbrl_vars, role_id, lbase = "calculation") {
  
  res <- 
    xbrl_vars[[lbase]] |>
    dplyr::filter(roleId == role_id) |>
    dplyr::select(fromElementId, toElementId, order) |>
    dplyr::mutate(order = as.numeric(order)) |>
    dplyr::arrange(order) |>
    unique() 

  # check the hierarchy: children should not be duplicated
  duplicated_children <- duplicated(res$toElementId)
  if(any(duplicated_children)) {
    res <- res[!duplicated_children, ]
    warning("Found and removed duplicated children in ", lbase, "/",role_id, call. = FALSE)
  }
  class(res) <- c("xbrl_relations", "data.frame")
  return(res)
}

#' Get a financial statements object from XBRL
#' 
#' @param xbrl_vars a XBRL parsed object
#' @param rm_prefix a prefix to remove from element names
#' @param complete_only Get only context with complete facts
#' @param complete_first Get only context with non-NA first fact
#' @param role_ids specify statements (all statements are returned by default)
#' @param lbase link base ("calculation" is default, can also do "presentation" or "definition")
#' @param basic_contexts when duplicated periods, only basic contexts are returned
#' @return A statements object
#' @examples
#' \dontrun{
#' 
#' # parse XBRL to XBRL data and get statements:
#' xbrl_url <- "http://edgar.sec.gov/Archives/edgar/data/320193/000119312514383437/aapl-20140927.xml"
#' xbrl_data <- xbrl_parse_min(xbrl_url)
#' st1 <- xbrl_get_statements(xbrl_data)
#' 
#' # get statements directly from a url:
#' xbrl_url <- "http://edgar.sec.gov/Archives/edgar/data/320193/000119312514383437/aapl-20140927.xml"
#' st1 <- xbrl_get_statements(xbrl_url)
#' }
#' 
#' @seealso \link{finstr}
#' @export
xbrl_get_statements <- function(xbrl_vars, rm_prefix = "us-gaap_", 
                                complete_only = FALSE,
                                complete_first = TRUE, 
                                nonzero_only = FALSE,
                                role_ids = NULL,
                                lbase = "calculation",
                                basic_contexts = TRUE)  {

  # xbrl is parsed xbrl
  if( !all( c("role", "calculation", "fact", "context", "element") %in% names(xbrl_vars))) {
    stop("Input does not include all data needed from XBRL.")
  }
  
  
  # get all statement types from XBRL
  if(missing(role_ids)) {
    role_ids <- xbrl_get_statement_ids_WB(xbrl_vars)
    statement_names <- role_ids # can use statement names to present output later on in better way
    # can add as attributes to final output
    role_ids <- role_ids$roleId
    # role_ids <- xbrl_get_statement_ids(xbrl_vars)
  }
  
  #get calculation link base relations
  relations <- lapply(role_ids, function(role_id){
    xbrl_get_relations(xbrl_vars = xbrl_vars, role_id = role_id, lbase = lbase)
  })
  with_content <- vapply(relations, function(x) nrow(x) > 0, logical(1))
  if(!any(with_content)) {
    return(NULL)
  }
  relations <- relations[with_content]
  role_ids <- role_ids[with_content]
    
  names(relations) <- basename(role_ids)
  elements_list <- lapply(relations, function(x){
    xbrl_get_elements(xbrl_vars, x)
  })
  names(elements_list) <- basename(role_ids)
  
  taxonomy_prefix <- sprintf("^%s", rm_prefix)

  # store xbrl data in statement data structure
  statements <- 
    stats::setNames(
      lapply(
        role_ids,
        function(role_id) {
          # browser()
          stat_name <- basename(role_id)
          links <- relations[[stat_name]]
          elements <- elements_list[[stat_name]]
          #label <- xbrl_get_labels(xbrl_vars, elements)
          res <- xbrl_get_data(elements, xbrl_vars, 
                               complete_only, complete_first,
                               basic_contexts)
          # delete taxonomy prefix
          names(res) <- gsub(taxonomy_prefix, "", names(res))          
          links$fromElementId <- gsub(taxonomy_prefix, "", links$fromElementId)          
          links$toElementId <- gsub(taxonomy_prefix, "", links$toElementId)          
          elements$elementId <- gsub(taxonomy_prefix, "", elements$elementId)          
          elements$parentId <- gsub(taxonomy_prefix, "", elements$parentId)          
          # set attributes
          attr(res, "role_id") <- stat_name
          attr(res, "relations") <- links
          attr(res, "elements") <- elements
          res
        } 
      ),
      basename(role_ids)
    )
  class(statements) <- c("statements", "list")
  return(statements)
}

xbrl_get_statements_WB <- function(xbrl_vars, rm_prefix = "us-gaap_", 
                                complete_only = FALSE,
                                complete_first = TRUE, 
                                nonzero_only = FALSE,
                                role_ids = NULL,
                                lbase = "calculation",
                                basic_contexts = TRUE,
                                filter_members = FALSE,
                                regular_sec_reporting_periods = TRUE,
                                end_of_quarter = FALSE,
                                nr_periods = 1)  {
  
  # xbrl is parsed xbrl
  if( !all( c("role", "calculation", "fact", "context", "element") %in% names(xbrl_vars))) {
    stop("Input does not include all data needed from XBRL.")
  }
  
  # get all statement types from XBRL
  if(missing(role_ids)) {
    role_ids <- xbrl_get_statement_ids_WB(xbrl_vars)
    statement_names <- role_ids # can use statement names to present output later on in better way
    # can add as attributes to final output
    role_ids <- role_ids$roleId
  }
  
  #get calculation link base relations
  relations <- lapply(role_ids, function(role_id){
    xbrl_get_relations(xbrl_vars = xbrl_vars, role_id = role_id, lbase = lbase)
  })
  with_content <- vapply(relations, function(x) nrow(x) > 0, logical(1))
  if(!any(with_content)) {
    return(NULL)
  }
  relations <- relations[with_content]
  role_ids <- role_ids[with_content]
  
  names(relations) <- basename(role_ids)
  elements_list <- lapply(relations, function(x){
    xbrl_get_elements(xbrl_vars, x)
  })
  names(elements_list) <- basename(role_ids)
  
  taxonomy_prefix <- sprintf("^%s", rm_prefix)
  
  # store xbrl data in statement data structure
  statements <- 
    stats::setNames(
      lapply(
        role_ids,
        function(role_id) {
          stat_name <- basename(role_id)
          links <- relations[[stat_name]]
          elements <- elements_list[[stat_name]]
          #label <- xbrl_get_labels(xbrl_vars, elements)
          res <- xbrl_get_data_WB(elements, xbrl_vars, 
                               complete_only = complete_only, complete_first = complete_first,
                               basic_contexts = basic_contexts, end_of_quarter = end_of_quarter, 
                               filter_members = filter_members, nr_periods = nr_periods,
                               regular_sec_reporting_periods = regular_sec_reporting_periods)
          if (class(res)[1] == "try-error"){
            res <- "error"
            class(res) <- "WB error"
            return(res)
          }
          # delete taxonomy prefix
          names(res) <- gsub(taxonomy_prefix, "", names(res))          
          links$fromElementId <- gsub(taxonomy_prefix, "", links$fromElementId)          
          links$toElementId <- gsub(taxonomy_prefix, "", links$toElementId)          
          elements$elementId <- gsub(taxonomy_prefix, "", elements$elementId)          
          elements$parentId <- gsub(taxonomy_prefix, "", elements$parentId)          
          # set attributes
          attr(res, "role_id") <- stat_name
          attr(res, "relations") <- links
          attr(res, "elements") <- elements
          res
        } 
      ),
      basename(role_ids)
    )
  class(statements) <- c("statements", "list")
  # statements <- Filter(Negate(is.null), statements)
  return(statements)
}



#' Get statement elements and the calculation hierarchy
#' 
#' @param x A statement object
#' @param parent_id used as filter if defined
#' @param all if FALSE only terminal elements from the hierarchy will be returned
#' @seealso \code{\link{calculate}}
#' @export
get_elements <- function(x, parent_id = NULL, all = TRUE) {
  # returns all terminating elements 
  # if parent_id provided, only descendands from this elements are returned

  if( is.null(x)  ) {
    stop("No statement")
  }
  if( !"statement" %in% class(x)  ) {
    stop("Not a statement class")
  }

  elements <- attr(x, "elements")
  # if not explicitly given a parentId, this statement is FALSE, even though
  # parentId is set to NULL by default.
  if(!missing(parent_id)) {
    children <- elements[["elementId"]] == parent_id
    if(!any(children)) {
      stop("No children with parent ", parent_id, " found", call. = FALSE)
    }
    id_parent <- elements[["id"]][children]
    elements <- elements %>%
      dplyr::filter_(~substring(id, 1, nchar(id_parent)) == id_parent) %>%
      as.elements()

  }
  # if all = FALSE, only take terminal elements and set their level to 1.
  if(!all) {
    elements <- elements %>%
      dplyr::filter_(~terminal) %>%
      dplyr::mutate(level = 1) %>%
      as.elements()
  }

  return(elements)

}


#' Get descendands
#' @description Gets all descendand terminal elements from calculation tree
#' @param x a statement object
#' @param element_id element (or vector of elements) from statement hierarchy
#' @export
#' @keywords internal
get_descendants <- function(x, element_id, all = FALSE) {
  
  unique(do.call(c, lapply(
    element_id, function(e) {
      get_elements(x, parent_id = e, all)[["elementId"]]
    }
  )))
  
}

#' Get parent
#' @description Gets a parent element id
#' @param x a statement object
#' @param element_id element id
#' @export
#' @keywords internal
get_parent <- function(x, element_id) {
  elements <- get_elements(x)
  elements[elements$elementId %in% element_id,"parentId"]
}


#' Get ascendant
#' @description Gets common ascendant from elements
#' @param x a statement object
#' @param element_id elements
#' @export
#' @keywords internal
get_ascendant <- function(x, element_id) {
  elements <- get_elements(x)

  if( !all(element_id %in% elements[,"elementId"] )) 
    return(NULL)
  
  sel_elements <- elements[elements$elementId %in% element_id,]
  asc1 <- sapply(elements$id, function(x) { all(x == substring(sel_elements$id, 1, nchar(x)))})
  return(elements[asc1,"elementId"])
}


as.elements <- function(x) {
  if( !all(c("elementId", "parentId") %in% names(x))) {
    stop("Can't convert to elements")
  }
  class(x) <- c("elements", "data.frame")
  return(x)
}

#' Check statement
#' @description Checks statement calculation consistency
#' @param statement statement object
#' @param element_id element from hierarchy where to perform calculation (if specified)
#' @return check object with calculated and original values
#' @export
check_statement <- function(statement, element_id = NULL) {
  
  if(! "statement" %in% class(statement)) {
    stop("Not a statement object")
  }

  els <- get_elements(statement)
  if(is.null(els)) {
    stop("No calculation hierarchy found")
  }
  if(missing(element_id)) {
    element_id <- els$elementId
  }
  err1 <- do.call(
    rbind,
    lapply(element_id, function(x) {
      
      xb <- els$balance[els$elementId == x]
      xc <- stats::na.omit(els$elementId[els$parentId == x])
      xcb <- els$balance[els$element %in% xc]
      xcs <- ifelse( xb == xcb, 1, -1)
      xcv <- rowSums(  crossprod (t(statement[, xc]),  xcs) )
      xv <- statement[ , x]
      if(length(xc) == 0) {
        return(NULL)
      }
      err <- data.frame(
        date = statement$endDate, 
        elementId = x, 
        expression = paste(ifelse(xcs == 1, "+", "-"), xc, collapse = " "),
        original = xv,
        calculated = xcv,
        error = xv - xcv, 
        stringsAsFactors = FALSE)
      row.names(err) <- 1:nrow(err)
      return(err)
    })
  )
  class(err1) <- c("check", "data.frame")
  return(err1)
}



#' Merge statement elements object
#' 
#' @param x elements object
#' @param y elements object
#' @param ... ignored
#' @keywords internal
#' @export
merge.elements <- function(x, y, ...) {
  z <- NULL
  #browser()
  col_names <- names(x)[!names(x) %in% c("level", "id", "terminal")]
  z <- rbind(x[,col_names], y[,col_names])
  z <- z[!duplicated(z[,c("elementId", "parentId")]), ]
  z <- get_elements_h(z)
  z <- as.elements(z)
  return(z)
}

# Check if elements of two financial statements have the same parentIds
# If they are different give preference to statement 1
# created by Wouter
check_parents_of_elements <- function(x, y){
  # Load the dplyr package
  # Perform the join and filter based on the conditions
  joined <- x |> dplyr::select(elementId, parentId) |>
    dplyr::inner_join(y |> dplyr::select(elementId, parentId), by = "elementId") 
  joined[is.na(joined)] <- "@NA@"
  to_change <- joined |>
    dplyr::filter(parentId.x != parentId.y)
  if (nrow(to_change) > 0){
    print("Found elements with conflicitng parents")
    print(to_change)
    # now create a fix
    y[is.na(y)] <- "@NA@"
    y_updated <- y |> dplyr::left_join(to_change, by = dplyr::join_by(elementId)) |> 
      dplyr::mutate(parentId = ifelse(!is.na(parentId.x), parentId.x, parentId)) |> 
      dplyr::select(-parentId.x, -parentId.y) # Optionally, remove the D column if not needed
    y_updated[y_updated == "@NA@"] <- NA
    y_updated$order <- as.numeric(y_updated$order)
    y_updated$level <- as.numeric(y_updated$level)
    # need elementId, parentId and order as input for element_h()
    # order follows from get_relations()
    return(y_updated)
  }
}
  
check_parents_of_elements1 <- function(x, y, update = FALSE){
  
  joined <- x |> dplyr::select(elementId, parentId) |>
    dplyr::inner_join(y |> dplyr::select(elementId, parentId), by = "elementId") 
  joined[is.na(joined)] <- "@NA@"
  to_change <- joined |>
    dplyr::filter(parentId.x != parentId.y)
  if (update){
    if (nrow(to_change) > 0){
      print("Found elements with conflicitng parents")
      print(to_change)
      # now create a fix
      y[is.na(y)] <- "@NA@"
      y_updated <- y |> dplyr::left_join(to_change, by = dplyr::join_by(elementId)) |> 
        dplyr::mutate(parentId = ifelse(!is.na(parentId.x), parentId.x, parentId)) |> 
        dplyr::select(-parentId.x, -parentId.y) # Optionally, remove the D column if not needed
      y_updated[y_updated == "@NA@"] <- NA
      y_updated$order <- as.numeric(y_updated$order)
      y_updated$level <- as.numeric(y_updated$level)
      # need elementId, parentId and order as input for element_h()
      # order follows from get_relations()
      return(y_updated)
    }
  } else {
    return(nrow(to_change))
  }
}

compare_element_names <- function(x, y){
  
  num_x <- x[last_char_is_num(x)] |> sort()
  num_y <- y[last_char_is_num(y)] |> sort()
  test_passed = TRUE
  if (length(num_x) == length(num_y)){
    test <- num_x == num_y
    if (sum(!test) > 0) {
      test_passed <- FALSE
      browser()
    }
  } else if (length(num_x) > length(num_y)){
    alt_elements <- stringr::str_sub(x, end = -2)
    if (sum(num_y %in% alt_elements) > 0){
      test_passed <- FALSE
      browser()
    } else {
      test_passed <- TRUE
    }
  } else if (length(num_y) > length(num_x)){
    alt_elements <- stringr::str_sub(y, end = -2)
    if (sum(num_y %in% alt_elements) > 0){
      test_passed <- FALSE
      browser()
    } else {
      test_passed <- TRUE
    }
  } else {
    browser()
  }
 return(test_passed)
}


#' Merge two financial statements
#' 
#' @description Merge two statements from different time periods.
#' @details
#' Since statements are basically data.frames the functions are similar. Except:
#' \itemize{
#' \item new statement elements are allways union of input elements,
#'   if taxonomy changes new and old elements are visible in all periods 
#' \item missing values are set to 0 not to NA as merge would normally do 
#' \item hierarchy is merged from both statements hierarchies
#' \item rows are treated as duplicated based on endDate and the row of x is 
#' allways considered as duplicate (so y should allways be the later statement)
#' }
#' 
#' @param x statement object
#' @param y statement object
#' @param replace_na (boolean) replace NAs with zeros  
#' @param ... further arguments passed to or from other methods
#' @return statement object
#' @export
merge.statement <- function(x, y, replace_na = FALSE, remove_dupes = FALSE, 
                            keep_first = TRUE,...) {
  if( !"statement" %in% class(x) || !"statement" %in% class(y) ) {
    stop(paste("Not statement objects. Dealing with object classes", class(x), "and", class(y)))
  }
  if (isFALSE(compare_element_names(names(x)[-5], names(y)[-5]))) browser()
  # merge elements
  # without any additional arguments, this just takes the attribute elements from x
  el_x <- get_elements(x)
  el_y <- get_elements(y)
  # before merging elements, check if elementId and parentId combinations in x
  # matches elementId and parentId combinations in y - if there are inconsistencies here
  # we will run into issues down the road
  # browser()
  parents_check <- check_parents_of_elements1(el_x, el_y, update = FALSE)
  if (parents_check > 0){
    print(paste(parents_check, "number of mismatches in parent- and element Ids."))
    print("Will fix now")
    el_y_org <- el_y
    el_y <- check_parents_of_elements1(el_x, el_y, update = TRUE)
  }
  el_z <- try(merge.elements(el_x, el_y), silent = TRUE) 
  
  z_error <- "try-error" %in% class(el_z)
  if (z_error){
    browser()
  }
  
  if(!any(names(x)[-(1:5)] %in% names(y)[-(1:5)])  ) {
    # if same period and different statements
    col_pos <- which(names(y) %in% c("contextId", "startDate", "decimals"))
    z <- 
      x |>
      dplyr::left_join(y[,-col_pos], by = "endDate")
    
  } else {
    # if same statement type and different periods
    z <- suppressMessages(dplyr::full_join(x, y))
    # dplyr::full_join prevents issues with merging 10Q and 10K merges.
    # z <- merge.data.frame(x, y, by = intersect(names(x), names(y)), all = TRUE, ...)
    # # if (sum(last_char_is_num(names(z)[-5])) > 0 ) browser()
    # replace NAs in values by zeros
    if(replace_na) {
      z[,5:ncol(z)][is.na(z[,5:ncol(z)])] <- 0
    }
    if (remove_dupes){
      # remove duplicated rows (based on periods)
      # if fromLast = FALSE, will drop last duplicated observation, will keep first
      # if fromLast = TRUE, will drop first duplicated observation, will keep last
      z <- z[!duplicated(z[c("startDate", "endDate", "value1")], fromLast = !keep_first), ]
    }
    # order rows by endDate
    z <- z[order(z$endDate), ]
    # order columns based on original taxonomy
      if ("value1" %in% names(z)) desc_col <- 5
      else desc_col <- 4
      z <- z[ ,c(names(z)[1:desc_col], el_z[["elementId"]])] 
    
  }
  # if (sum(last_char_is_num(names(z)[-5])) > 0 ) browser()
  # add attributes
  class(z) <- class(x)
  if (z_error){
    attr(z, "elements") <- el_x
  } else {
    attr(z, "elements") <- el_z
  }
  attr(z, "role_id") <- attr(x, "role_id")
  if (isFALSE("statement" %in% class(x))) browser()
  return(z)  
}



#' Merge two lists of statements
#' 
#' @details Merges all statements in x with all statements in y
#' @param x statements object
#' @param y statements object
#' @param replace_na (boolean) replace NAs with zeros  
#' @param ... further arguments passed to or from other methods
#' @return statements object
#' @seealso \link{merge.statement} for merging two statements
#' @export
merge.statements <- function(x, y, replace_na = TRUE, remove_dupes = FALSE, 
                             keep_first = TRUE,...) {
  if (class(x)[1] == "WB error") return(y)
  if (class(y)[1] == "WB error") return(x)
  
  if( !"statements" %in% class(x) || !"statements" %in% class(y) ) {
      stop("Not statements objects")
  }

  if (length(x) != length(y)){
    stop("Different number of statements to merge")
  }
  
  z <-
      lapply(names(x), function(statement){
        merge.statement(x[[statement]], y[[statement]], replace_na = replace_na, 
                        remove_dupes = remove_dupes, keep_first = keep_first, ...)
      })
    names(z) <- names(y)
  class(z) <- "statements"
  return(z)
}


#' Calculate formulas 
#' 
#' @param x a statement object
#' @param ... list of formulas
#' @param calculations optional: calculations generated by calculation function
#' @param digits if specified the result will be rounded according to number of digits
#' @param decimals if specified the result will be multiplied by 10^decimals
#' @return data frame with date and specified columns
#' @examples
#' \dontrun{
#' 
#' balance_sheet %>% calculate(
#'   
#'   current_ratio = AssetsCurrent / LiabilitiesCurrent,
#'   
#'   quick_ratio =  
#'     ( CashAndCashEquivalentsAtCarryingValue + 
#'         AvailableForSaleSecuritiesCurrent +
#'         AccountsReceivableNetCurrent
#'       ) / LiabilitiesCurrent
#' )
#' }
#' @seealso \code{\link{calculation}}
#' @export
calculate <- function(x, ..., digits = NULL, decimals = NULL, calculations = NULL) {
  # calculate
  res <- dplyr::transmute_(x, date = ~endDate, .dots = c(lazyeval::lazy_dots(... ),calculations))
  # remove hidden columns (leading dots)
  res <- res[grep("^[^\\.]", names(res))]
  # rounding results
  if(!missing(digits)) {
    res[,2:ncol(res)] <- round(res[,2:ncol(res)], digits) 
  }
  if(missing(decimals)) {
    if(!is.null(x[["decimals"]])) {
      decimals <- min(x[["decimals"]], na.rm = TRUE)
    }
    if(!is.null(x[["decimals.x"]])) {
      decimals <- min(x[["decimals.x"]], na.rm = TRUE)
    }
  }
  if(!is.null(decimals) && !is.na(decimals) && 
       max(abs(res[1:5,2:ncol(res)]), na.rm = TRUE)>10^(-decimals)) {
    res[,2:ncol(res)] <- res[,2:ncol(res)] * 10 ^ decimals 
  }
  return(res)
}

#' Define calculation
#' @param ... formulas
#' @seealso \code{\link{calculate}}
#' @examples
#' \dontrun{
#' 
#' profit_margins <- calculation(
#'  Gross_Margin = (SalesRevenueNet - CostOfGoodsAndServicesSold) / SalesRevenueNet,
#'  Operating_Margin = OperatingIncomeLoss / SalesRevenueNet,
#'  Net_Margin = NetIncomeLoss / SalesRevenueNet
#' )
#' 
#' income_statement %>% calculate(calculations = profit_margins)
#' }
#' @export
calculation <- function(...) {
  lazyeval::lazy_dots(...) 
}




#' Statement lagged differences
#' 
#' @param x a statement to be differenced
#' @param lag an integer indicating which lag to use
#' @param ... further arguments passed to or from other methods
#' @return a statement object equal to successive  differences (x and lagged x)
#' @export
diff.statement <- function(x, lag = 1L, ...) {
  y <- x[-((nrow(x)-lag+1):nrow(x)),]
  y$endDate <- x$endDate[-(1:lag)]
  y[,5:ncol(y)] <- x[-(1:lag),5:ncol(x)] - y[,5:ncol(y)]
  return(y)
}

#'Without operator
#'@param a element id from element hierarchy
#'@param b element id from element hierarchy
#'@description Used inside fold function to select elements in a without elements in b
#'@return Lazy object with function to select statement elements in a witout elements in b
#'@export
#'@keywords internal
`%without%` <- function (a, b) {
  # declare x and y just to let the check now there is no problem...
  # x will be a statement object when the expression evaluates
  # y will be all used elements
  x <- NULL
  y <- NULL

  lazyeval::lazy(
    setdiff( setdiff( get_descendants(x, a), get_descendants(x, b)), y)
  )
}

#' Proportional values
#' 
#' Every value in the financial statement is divided by topmost parent's value
#' @param x a statement object
#' @param digits if specified number of digits to round the result
#' @export
proportional <- function(x, digits = NULL) {
  y <- x
  for(col_name in get_elements(x)[["elementId"]]) {
    parent_id <- get_ascendant(x, col_name)[[1]]
    x[[col_name]] <- y[[col_name]] / y[[parent_id]]
    if(!missing(digits)) {
      x[[col_name]] <- round(x[[col_name]], digits )
    }
  }
  x[["decimals"]] <- 0
  return(x)
}

#'Other elements
#'@param ... element IDs from element hierarchy
#'@description Used inside expose function to select all other elements
#'@return Lazy object with function to select elements from statement x
#'@export
#'@keywords internal
other <- function (...) {
  # declare x and y just to let the check now there is no problem...
  # x will be a statement object when the expression evaluates
  # y will be all used elements
  x <- NULL
  y <- NULL
  
  lazyeval::lazy(
    setdiff(get_descendants(x, c(...) ), y)
  ) 
}



#' Reshape to "long" format
#' @description Reshapes statement object to a data frame with one value column
#'  and dimension columns (endDate, elementId and parentId). 
#' @param x a statement object
#' @param levels if defined only elements from specified levels will be included
#' @export
reshape_long <- function(x, levels = NULL) {
  elements <- get_elements(x)
  if(missing(levels)) levels <- unique(elements[["level"]])

  x %>%
    tidyr::gather_("elementId", "value", elements[["elementId"]], convert = FALSE) %>%
    dplyr::mutate_("elementId" = ~as.character(elementId)) %>%
    dplyr::inner_join(elements, by = "elementId") %>%
    dplyr::filter_(~!is.na(parentId) & level %in% levels ) %>%
    dplyr::select_(date = ~endDate, element = ~elementId, parent = ~parentId, ~value, 
            label = ~labelString, ~decimals, element_id = ~id) %>%
    dplyr::left_join(elements, by = c("parent" = "elementId")) %>%
    dplyr::select_(~date, ~element, ~parent, ~value, ~label, 
            parent_label = ~labelString, ~decimals, 
            ~element_id,  parent_id = ~id)

}

#' Reshape statement data to table format
#' 
#' Transposes data to "print-out" format
#' @param x a statement object
#' @param decimals return values with decimals
#' @export
#' @keywords internal
reshape_table <- function(x, decimals = TRUE, simple = FALSE) {

  e <- get_elements(x)
  values <- finstr_values(x) 

  if(decimals) {
    decimals_no <- min(x[["decimals"]])
    if(is.na(decimals_no)) decimals_no <- 0
    values <- values * 10 ^ decimals_no
  }
  
  parent_pos <- match(e[["parentId"]], e[["elementId"]])
  is_negative <- e[,"balance"] != e[parent_pos, "balance"]
  is_negative[is.na(is_negative)] <- FALSE
  
  ret <- cbind(e[,c("elementId", "level", "id", "terminal", "labelString")], is_negative, t(values))
  if(!any(duplicated(x$endDate))) {
    names(ret)[7:ncol(ret)] <- x$endDate
  } else {
    names(ret)[7:ncol(ret)] <- x$contextId
  }
  row.names(ret) <- 1:nrow(ret)
  if(simple) {
    ret <- ret[, -c(1, 2, 3, 4, 6)]
  }
  return(ret)
}


expose_prepare <- function(x, e_list) {
  used_elements <- c()
  ret_list <- list()
  
  for(exp_name in names(e_list) ) { 
    exp_els <- e_list[[exp_name]]
    
    # Elements can be defined as a function (lazy object)
    # ... or as descendands of specified elements
    if("lazy" %in% class(exp_els)) {
      els <- lazyeval::lazy_eval( exp_els, 
                                  data = list(x = x, y = used_elements))
    } else {
      els <- unname(get_descendants(x, exp_els))
    }
    if( any(els %in% used_elements) ) {
      warning("The duplicate elements will be removed from ", 
              exp_name, ": ",
              els[ els %in% used_elements],
              call. = FALSE)
      els <- setdiff( els, used_elements)
    }
    # track used elements
    used_elements <- c(used_elements, els)
    # result
    ret_list[[exp_name]] <- els    
  }
  
  # pick all leftovers
  elements <- get_elements(x, all = FALSE)[["elementId"]]
  the_rest <- setdiff(elements, used_elements)
  if(length(the_rest) > 0) {
    the_rest <- split(the_rest, sapply(the_rest, function(s) get_ascendant(x, s)[1]))
    for(tr in names(the_rest)) {
      ret_list[[paste0("Other",tr,"_")]] <- the_rest[[tr]]
    }
  }
  
  return(ret_list)
}


#' Expose financial sheet values
#' 
#' Simplifies statement to 2-level hierarchy.
#' Elements are defined by list of element vectors.
#' 
#' @param x a statement object
#' @param ... expressions to expose values
#' @param e_list expressions to expose values
#' @examples
#' 
#' \dontrun{
#' expose(balance_sheet,
#'                      
#'   # Assets
#'   `Current Assets` = "AssetsCurrent",
#'   `Noncurrent Assets` = other("Assets"),
#'   # Liabilites and equity
#'   `Current Liabilities` = "LiabilitiesCurrent",
#'   `Noncurrent Liabilities` = other(c("Liabilities", "CommitmentsAndContingencies")),
#'   `Stockholders Equity` = "StockholdersEquity"
#' )
#' }
#' @export
expose <- function(x, ..., e_list = NULL) {
  
  # concatenate dots and list arguments
  e_list <- c(e_list, list(...))
  # prepare list of elements
  if(length(e_list) > 0)
    e_list <- expose_prepare(x, e_list)
  
  descriptions <- names(e_list)
  names(e_list) <- make.names(names(e_list))
  
  # initial y = top level elements from x
  x_els <- get_elements(x)
  y_els <- x_els[ x_els$level == 1, ]
  y_els$terminal <- TRUE
  y <- x[, c(names(x)[1:4], y_els$elementId)]
  attr(y, "elements") <- y_els

  for(exp_name in names(e_list) ) { 
    els <- e_list[[exp_name]]
    description <- descriptions[which(exp_name == names(e_list))]
    # add a row in elements object
    parent_id <- get_ascendant(x, els)[1]
    if(is.na(parent_id) || length(parent_id) == 0)
      stop("A group of elements defined too broadly")
    balance <- x_els[ x_els$elementId == parent_id, "balance" ]
    labelString <- description
    
    y_els <-
      dplyr::bind_rows(y_els, dplyr::data_frame(
          elementId = exp_name,
          parentId = parent_id,
          order = 1,
          balance = balance,
          labelString = labelString))
    
    # calculate value
    xb <- x_els$balance[x_els$elementId == parent_id]
    xc <- stats::na.omit(x_els[x_els$elementId %in% els, "elementId"])
    xcb <- x_els[x_els$elementId %in% xc, "balance"]
    xcs <- ifelse(xb == xcb, 1, -1)
    xcv <- rowSums(crossprod(t(x[, xc]), xcs), na.rm = TRUE )
    y[[exp_name]] <- xcv
    
    # rearrange hierarchy
    y_els <- get_elements_h(y_els[1:5])
    y_els <- as.elements(y_els)
    y <- y[,c(names(x)[1:4], y_els$elementId)]
    attr(y, "elements") <- y_els
  }
  
  return(y)
}

