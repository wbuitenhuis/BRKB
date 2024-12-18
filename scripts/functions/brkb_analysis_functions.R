# https://www.sec.gov/data-research/sec-markets-data/form-n-port-data-sets
# quarterly sec filings with holdings from all S mutual funds



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
  # filing_urls <- c("/Archives/edgar/data/1067983/000156459021055032/0001564590-21-055032-index.htm")
  #              "/Archives/edgar/data/1067983/000156459020052144/0001564590-20-052144-index.htm")
  # want:
  # income statement for all business units separately for different periods
  # aggregate balance sheet for different periods

  # IS2_parent <- IS2_all <- NULL
  add_is2 <- FALSE
  for (i in 1:min(40, length(filing_urls))){

    if (sum(is.na(filing_urls) > 0)) browser()
    if (nchar(filing_urls[i]) < 5 | is.na(filing_urls[i])) browser()
    xml_filenames <- edgar_xbrl_URLs(paste0("https://www.sec.gov", filing_urls[i]),
                                     verbose = TRUE)
    print(paste("i = ", i, "XML file:", xml_filenames[1]))
    
    xbrl <- parse_xbrl(xml_filenames, cache_dir = "xbrl/cache_dir/")
    # xbrl <- check_elementnames(xbrl, fix = TRUE)
    xbrl$fact <- remove_duplicated_facts(xbrl$fact)
    # browser()
    # ind <- stringr::str_which(xbrl$fact$fact, "3497")
    # ind <- stringr::str_which(xbrl$fact$fact, "3527")
    # contex_id1 <- xbrl$fact$contextId[ind[1]]
    # contex_id2 <- xbrl$fact$contextId[ind[2]]
    # ind <- stringr::str_which(xbrl$context$contextId, contex_id1)
    # context1 <- xbrl$context[ind, ]
    # ind <- stringr::str_which(xbrl$context$contextId, contex_id2)
    # context2 <- xbrl$context[ind, ]
    st <- xbrl_get_statements_WB(xbrl_vars = xbrl, 
                                 lbase = "presentation",
                                 complete_first = FALSE, 
                                 end_of_quarter = TRUE,
                                 basic_contexts = FALSE,
                                 nonzero_only = TRUE)
                                 
    if (i == 1){
      st_parent <- lapply(st, clean_BRKB_statement, parent_only = TRUE)
      st_all <- lapply(st, clean_BRKB_statement)
      class(st_parent) <- class(st_all) <- "statements"
      n_parent <- length(st_parent)
      n_all <- length(st_all)
      # statements2excel(st_all, file = "statement1.xlsx")
      #statements2excel(st_pres, file = "statement_pres.xlsx")
    # browser()
    } else {
      #if (i == 18) browser()
      st_parent_i <- lapply(st, clean_BRKB_statement, parent_only = TRUE)
      # statements2excel(st_parent_i, file = "statement2.xlsx")
      st_all_i <- lapply(st, clean_BRKB_statement)
      class(st_parent_i) <- class(st_all_i) <- "statements"
      if (length(st_parent_i) > n_parent){
         if ("StatementConsolidatedStatementsOfEarnings2" %in% names(st_parent_i)){
           add_is2 <- TRUE
           IS2_parent <- st_parent_i[["StatementConsolidatedStatementsOfEarnings2"]]
           st_parent_i$StatementConsolidatedStatementsOfEarnings2 <- NULL
         } else {
      #     statements2excel(st_parent_i)
           browser()
         }
      }
      if (length(st_all_i) > n_all){
      #   browser()
         if ("StatementConsolidatedStatementsOfEarnings2" %in% names(st_all_i)){
           add_is2 <- TRUE
           IS2_all <- st_all_i[["StatementConsolidatedStatementsOfEarnings2"]]
           st_all_i$StatementConsolidatedStatementsOfEarnings2 <- NULL
         } else {
      #     # statements2excel(st_all_i)
           browser()
         }
      }
      if (length(st_parent_i) < n_parent){
        # browser()
        if ("StatementConsolidatedStatementsOfEarnings2" %in% names(st_parent)){
          add_is2 <- TRUE
          IS2_parent <- st_parent[["StatementConsolidatedStatementsOfEarnings2"]]
          st_parent[["StatementConsolidatedStatementsOfEarnings2"]] <- NULL
        } else {
          browser()
        }
        # statements2excel(st_parent_i)
      }
      if (length(st_all_i) < n_parent){
        # browser()
        if ("StatementConsolidatedStatementsOfEarnings2" %in% names(st_all)){
          add_is2 <- TRUE
          IS2_all <- st_all[["StatementConsolidatedStatementsOfEarnings2"]]
          st_all[["StatementConsolidatedStatementsOfEarnings2"]] <- NULL
        } else {
          browser()
        }
        # statements2excel(st_parent_i)
      }
      if (isFALSE("statement" %in% class(st_parent_i[[1]]))) browser()
      if (isFALSE("statement" %in% class(st_parent[[1]]))) browser()
      different_names <- names(st_parent) != names(st_parent_i)
      if (sum(different_names) > 0){
        st_parent_i <- compare_statement_names(st_parent, st_parent_i)
      }
      different_names <- names(st_all) != names(st_all_i)
      if (sum(different_names) > 0){
        st_all_i <- compare_statement_names(st_all, st_all_i)
      }
      st_parent <- merge.statements(st_parent, st_parent_i, replace_na = TRUE)
      st_all <- merge.statements(st_all, st_all_i, replace_na = FALSE)
      if (add_is2){
        st_parent[["StatementConsolidatedStatementsOfEarnings2"]] <- IS2_parent
        st_all[["StatementConsolidatedStatementsOfEarnings2"]] <- IS2_all
      }
    }
    add_is2 <- FALSE
    n_parent <- length(st_parent)
    n_all <- length(st_all)
    save(st_all, st_parent, file = "./data/BRKB_statements.RData")
  }
  print("Done")
}

clean_BRKB_statement <- function(st, parent_only = FALSE){
  # if missing for all BU, but not parent, should stay missing. 
  # need to track here which observations. Which variable meet these requirements?
  #browser()
  members <- c(NA, "brka:InsuranceAndOtherMember", "brka:RailroadUtilitiesAndEnergyMember", 
               "brka:CargoAndFreightMember", "brka:UtilitiesAndEnergyMember", "us-gaap:CargoAndFreightMember")
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

# present time series for different business units:
# revenue / operating expenses
# insurance
# energy
# rail roads
# investment gains
run_brkb_bu_analysis <- function(st){
  library(dplyr)
  is <- st[[2]]
  is <- is[,-(1:2)]
  browser()
  is <- is |> group_by(endDate, value1) |> summarise(across(everything(), sum))
  browser()
  ins <- is |> filter(value1 %in% c("brka:InsuranceAndOtherMember"))
  ins <- ins |> select_if(~ any(!is.na(.)) & any(. != 0))
  
  rail <- is |> filter(value1 %in% c("brka:CargoAndFreightMember", 
                                     "us-gaap:CargoAndFreightMember"))
  rail <- rail |> select_if(~ any(!is.na(.)) & any(. != 0))
  energy <- is |> filter(value1 %in% c("brka:UtilitiesAndEnergyMember"))
  energy <- energy |> select_if(~ any(!is.na(.)) & any(. != 0))
  
  infra <-  is |> filter(value1 %in% c("brka:RailroadUtilitiesAndEnergyMember"))
  infra <- infra |> select_if(~ any(!is.na(.)) & any(. != 0))
  
  # in "brka:InsuranceAndOtherMember"
  insurance_prem <- c("PremiumsEarnedNet")
  service_rev <- c("RevenueFromContractWithCustomerExcludingAssessedTax", 
                   "brka_SalesAndServiceRevenue",
                   "RevenueOtherFinancialServices", 
                   "brka_ServiceRevenuesAndOtherIncome")
  
  # in "brka:CargoAndFreightMember", "brka:UtilitiesAndEnergyMember", "us-gaap:CargoAndFreightMember"
  freight_rev <- c("Revenues", "OperatingExpenses", "brka_FreightRailTransportationRevenues") 
  energy_rev <- c("RevenueFromContractWithCustomerExcludingAssessedTax", 
    "brka_UtilityAndEnergyOperatingRevenues","brka_EnergyOperatingRevenues")
  
  freight <- rail[,c("endDate", "value1", freight_rev[freight_rev %in% names(rail)])]
  freight <- infra[, c("endDate", "value1")]
    
  leasing_rev <- "OperatingLeaseLeaseIncome"
 
  leasing_cost <- "brka_CostOfLeasing"
  
  insurance_loss <- c("LiabilityForUnpaidClaimsAndClaimsAdjustmentExpenseIncurredClaims1",
    "PolicyholderBenefitsAndClaimsIncurredNet", "IncurredClaimsPropertyCasualtyAndLiability"
  )
  life_ins_ben <-c("PolicyholderBenefitsAndClaimsIncurredLifeAnnuityAndHealth", 
      "PolicyholderBenefitsAndClaimsIncurredLifeAndAnnuity",            "brka_PolicyholderBenefitsAndClaimsIncurredLifeAnnuityAndHealth")
  ins_underw_expenses <- c("ExpenseRelatedToDistributionOrServicingAndUnderwritingFees",
  "brka_InsuranceUnderwritingExpenses", )
  service_cost <- "OtherFinancialServicesCosts"
 
  
  # under insurance
  interest_div_inc <- c("InvestmentIncomeInterestAndDividend", 
      "brka_InvestmentIncomeInterestDividendAndOther")
  interest_exp_ins <- "InterestExpense"
  c("GainLossOnInvestmentsExcludingOtherThanTemporaryImpairments", 
    "OtherThanTemporaryImpairmentLossesInvestmentsPortionRecognizedInEarningsNet",
    "GainLossOnInvestments", "NonoperatingGainsLosses")
  
  #under energy / rail
  interest_exp_infra <- "InterestExpense"
  
  
  c("PremiumsEarnedNet",
  "brka_SalesAndServiceRevenue",
  "brka_CargoAndFreightRevenueAndRegulatedAndUnregulatedOperatingRevenue",
  "OtherIncome",
  "GainLossOnInvestmentsExcludingOtherThanTemporaryImpairments",
  "OtherThanTemporaryImpairmentLossesInvestmentsPortionRecognizedInEarningsNet",
  # "SalesRevenueNet",
  "RevenueOtherFinancialServices",
  "RevenueFromContractWithCustomerIncludingAssessedTax",
  "InvestmentIncomeInterestAndDividend",
  "OperatingLeaseLeaseIncome",
  "brka_InvestmentIncomeInterestDividendAndOther",
  "brka_FreightRailTransportationRevenues",
  "brka_UtilityAndEnergyOperatingRevenues",
  "brka_EnergyOperatingRevenues",
  "brka_ServiceRevenuesAndOtherIncome",
  "Revenues",
  "RevenueFromContractWithCustomerExcludingAssessedTax",
  "NonoperatingIncomeExpense",
  "GainLossOnInvestments",
  "NonoperatingGainsLosses",
  "CostsAndExpensesAbstract",
  "LiabilityForUnpaidClaimsAndClaimsAdjustmentExpenseIncurredClaims1",
  "PolicyholderBenefitsAndClaimsIncurredNet",
  "PolicyholderBenefitsAndClaimsIncurredLifeAndAnnuity",
  "brka_PolicyholderBenefitsAndClaimsIncurredLifeAnnuityAndHealth",
  "IncurredClaimsPropertyCasualtyAndLiability",
  "brka_InterestExpenseAndForeignCurrencyTransactionGainLossOnDebt",
  "ExpenseRelatedToDistributionOrServicingAndUnderwritingFees",
  "OtherFinancialServicesCosts",
  "brka_InsuranceUnderwritingExpenses",
  "CostOfGoodsAndServicesSold",
  "brka_CostOfLeasing",
  # "SellingGeneralAndAdministrativeExpense"
  "OperatingExpenses",
  "brka_CostsOfServicesAndOperatingExpenses",
  "OtherExpenses",
  "InterestExpense",
  "CostsAndExpenses")
  
  #summarise(across(everything(), sum, .names = "sum_{col}"))
  
  # Print the aggregated dataframe
  print(aggregated_df)
  
}

# create a time series with share buy back and estimated buy back prices
brkb_shr_buybacks <- function(st){
  
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
  options("openxlsx.numFmt" = "#,##0,,")
  xl.workbook <- openxlsx::createWorkbook()
  for (i in 1:length(st)){
    openxlsx::addWorksheet(xl.workbook, sheetName = paste0("st_",i), zoom = 130)
    openxlsx::writeData(xl.workbook, sheet = paste0("st_",i), x= st[[i]], startRow = 1, startCol = 1)
  }
  openxlsx::saveWorkbook(xl.workbook, file = excel_filename, overwrite = TRUE)
  options("openxlsx.numFmt" = NULL)  
}

check_elementnames <- function(xbrl, fix = FALSE){
  # Checks foreElements name that end on a digit, while there is no apparent reason for it 
  # If fix = TRUE, wil remove last character from element name.
  # Input is a parsed xbrl object
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
