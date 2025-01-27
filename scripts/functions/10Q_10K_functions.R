
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
    print("There are no XML files.")
    ret <- "There are no XML files."
    class(ret) <- "WB error"
    return(ret)
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

statements_in_same_order <- function(x, y){
  # this function achieves same as compare statement names,
  # but does not use statement names, but the compares similarities in elements 
  # of statements to decide in which order the statements of x and y
  # need to be in order to match, or if any statements are missing.
  # output is y in such a form that it matches x as much as possible for merging purposes.
  
  if( !"statements" %in% class(x) || !"statements" %in% class(y) ) {
    stop("Not statements objects")
  }
  
  # below is not the right approach
  # should keep track which items ar erors or missing, than do analysis
  # if one statement is missing assign it the value of the statement to be merged
  ind_x <- ind_y <- NA
  error_x <- error_y <- FALSE
  if ("WB error" %in% lapply(x, class) |> unlist() |> unique()){
    browser()
    error_x <- TRUE
    ind_x <- lapply(x, class) == "WB error" |> unlist()
    x <- x[!ind_x]
    # x[ind_x] <- y[ind_x]
  }
  if ("WB error" %in% lapply(y, class) |> unlist() |> unique()){
    error_y <- TRUE
    ind_y <- lapply(y, class) == "WB error" |> unlist()
    error <- y[ind_y]
    y <- y[!ind_y]
  }
  variables_x <- lapply(x ,names)
  variables_y <- lapply(y, names)
  variables_x <- lapply(variables_x, function(x) x[-(1:5)])
  variables_y <- lapply(variables_y, function(x) x[-(1:5)])
  shared_perc <- lapply(variables_x, function(vec1) {
    sapply(variables_y, function(vec2) {
      length(intersect(vec1, vec2)) / length(vec1)
    })
  })
  shared_perc2 <- lapply(variables_x, function(vec1) {
    sapply(variables_y, function(vec2) {
      length(intersect(vec1, vec2)) / length(vec2)
    })
  })
  shared <- lapply(variables_x, function(vec1) {
    sapply(variables_y, function(vec2) {
      length(intersect(vec1, vec2))
    })
  })
  shared_perc <- do.call(rbind, shared_perc)
  shared_perc2 <- do.call(rbind, shared_perc2) # put results n matrix form
  # put results n matrix form
  shared <- do.call(rbind, shared)
  best_match <- apply(shared, 2, which.max)
  if (sum(duplicated(best_match)) > 0){
    browser() # have an issue 
  }
  if (length(x) == length(y)){
    y <- y[best_match]
    names(y) <- names(x)
  }
  if (length(x) > length(y)){
    # y_old <- y # for debugging
    # y <- y[best_match]
    # browser()
    names(y) <- names(x[best_match])
    ind_y <- !1:length(x) %in% best_match
    if (!error_y){
      # need to put an error in missing element
      # browser()
      error <- "statement missing"
      class(error) <- "WB error"
      ind_error <- which(!names(x) %in% names(y))
      for (i in 1:length(ind_error)){
        y <- append(y, error, after = ind_error[i] - 1)
        class(y[[ind_error[i]]]) <- class(error)
      }
    }
  } 
  if (length(x) < length(y)){
    browser()
    # which y is not in x
    # put y in best order
    # give y elements names of x
  } 
  if (error_y){
    # put errors bag in y if there were any
    ind_y <- which(ind_y)
    for (i in 1:length(ind_y)){
      y <- append(y, error[[i]], after = ind_y[i] - 1)
      class(y[[ind_y[i]]]) <- class(error[[i]])
    }
  }
  
  class(y) <- "statements"
  return(y)
}


find_specific_fact <- function(url, pattern, clean = TRUE, field = fact){
  #field = element, fact, label
  # look for a specific fact (as string)
  # this is used for debuging final reports that seem to have missing values
  xml_filenames <- edgar_xbrl_URLs(paste0(url), verbose = TRUE)
  xbrl <- parse_xbrl(xml_filenames, cache_dir = "xbrl/cache_dir/")
  #all_missing_rows <- apply(xbrl$fact, 1, function(row) all(is.na(row)))
  #print (paste(sum(all_missing_rows), "rows with only missing values"))
  if (clean){
    xbrl$fact <- remove_duplicated_facts(xbrl$fact)
  }
  fact <- xbrl$fact
  browser()
  if (field == "fact"){
    ind <- stringr::str_detect(tolower(fact$fact), pattern = tolower(pattern))
    ind[is.na(ind)] <- FALSE
    ret <- fact[ind, ]
    ret <- ret |> dplyr::left_join(xbrl$label, by = dplyr::join_by(elementId))
  } else {
    fact <- fact |> dplyr::left_join(xbrl$label, by = dplyr::join_by(elementId))
  }
  if (field == "label"){
    ind <- stringr::str_detect(fact$labelString, pattern = pattern)
    ind[is.na(ind)] <- FALSE
    ret <- fact[ind, ]
  }
  if (field == "element"){
    ind <- stringr::str_detect(fact$elementId, pattern = pattern)
    ind[is.na(ind)] <- FALSE
    ret <- fact[ind, ]
  }
  ret <- ret |> dplyr::left_join(xbrl$context, by = dplyr::join_by(contextId))
  return(ret)
}

  




