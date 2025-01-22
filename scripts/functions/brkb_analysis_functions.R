# https://www.sec.gov/data-research/sec-markets-data/form-n-port-data-sets
# quarterly sec filings with holdings from all S mutual funds


brkb_statements <- function(form = "10-Q", years = 13, 
                            filename = "BRKB_statements",
                            arc = "presentation"){
  # 1.) obtain CIK
  # 2.) download 10-Q and 10-K links
  # 3.) For each 10)Q form, download xbrl instance and xbrl schema
  # 4.) parse tables from xbrl instance into csv file
  # 5.) extract individual tables (income statement and balance sheet) from csv file, maintain hierarchy
  # 6.) join the same table from from different filings over time to create a time series
  source("./scripts/finstr/finstr.R")
  source("./scripts/functions/10Q_10K_functions.R")
  cik <- "0001067983"
  type <- form
  filing_urls <- edgar_link_to_filings(cik = cik, form = type)
  if (form == "10-Q"){
    n <- years * 3
  } else {
    n <- years
  }
  add_is2 <- TRUE
  # needed to update existing file
  if (file.exists(paste0("./data/", filename, ".RData"))){
    load(paste0("./data/", filename, ".RData"))
  }
  object_names <- c("st_all_10Q", "st_parent_10Q", "st_all_10K", "st_parent_10K")
  for (i in 1:length(object_names)){
    if (!exists(object_names[i])){
      assign(object_names[i], 0)
    }
  }
  
  for (i in 1:min(n, length(filing_urls))){
    # if (i == 14) browser()
    if (sum(is.na(filing_urls) > 0)) browser()
    if (nchar(filing_urls[i]) < 5 | is.na(filing_urls[i])) browser()
    xml_filenames <- edgar_xbrl_URLs(paste0("https://www.sec.gov", filing_urls[i]),
                                     verbose = TRUE)
    if ("WB error" %in% class(xml_filenames)){
      next
    }
    print(paste("i = ", i, "XML file:", xml_filenames[1]))
    
    xbrl <- parse_xbrl(xml_filenames, cache_dir = "xbrl/cache_dir/")
    # xbrl <- check_elementnames(xbrl, fix = TRUE)
    xbrl$fact <- remove_duplicated_facts(xbrl$fact)
    # if (i == 46) browser()
    st <- xbrl_get_statements_WB(xbrl_vars = xbrl, 
                                 lbase = arc,
                                 complete_first = FALSE, 
                                 end_of_quarter = FALSE,
                                 basic_contexts = FALSE,
                                 nonzero_only = TRUE,
                                 regular_sec_reporting_periods = TRUE,
                                 nr_periods = 2)
    # browser() 
    remove_error_p <- FALSE
    remove_error_a <- FALSE
    if (i == 1){
      st_parent <- lapply(st, clean_BRKB_statement, parent_only = TRUE, filter = TRUE)
      st_all <- lapply(st, clean_BRKB_statement, parent_only = FALSE, filter = FALSE)
      class(st_parent) <- class(st_all) <- "statements"
      n_parent <- length(st_parent)
      n_all <- length(st_all)
    } else {
      # if (sum(names(st_parent[[3]]) %in% names(st_parent[[4]])) > 20) browser()
      st_parent_i <- lapply(st, clean_BRKB_statement, parent_only = TRUE, filter = TRUE)
      st_all_i <- lapply(st, clean_BRKB_statement, parent_only = FALSE, filter = TRUE)
      class(st_parent_i) <- class(st_all_i) <- "statements"
      if (sum(lapply(st_all_i, class) |> unlist() == "WB error") == length(st_all_i)){
        print(paste("There are no statements for i", i))
        # only errors, nothing to merge.
        next
      }
      # if (i == 47) browser()
      st_parent_i <- statements_in_same_order(st_parent, st_parent_i)
      st_all_i <- statements_in_same_order(st_all, st_all_i)
      if ("WB error" %in% lapply(st_parent_i, class)){
        #browser()
        remove_error_p <- TRUE
        ind_error_p <- sapply(st_parent_i, function(x) "WB error" %in% class(x))
        remove_p_i <-  st_parent_i[ind_error_p]
        remove_p <- st_parent[ind_error_p]
        #names(st_parent_i) <- names(st_parent)
        st_parent_i <- st_parent_i[!ind_error_p]
        st_parent <- st_parent[!ind_error_p]
        class(st_parent_i) <- class(st_parent) <- "statements"
      } 
      if ("WB error" %in% lapply(st_all_i, class)){
        remove_error_a <- TRUE
        ind_error_a <- sapply(st_all_i, function(x) "WB error" %in% class(x))
        remove_a_i <- st_all_i[ind_error_a]
        remove_a <-  st_all[ind_error_a]
        # names(st_all_i) <- names(st_all)
        st_all_i <- st_all_i[!ind_error_a]
        st_all <- st_all[!ind_error_a]
        class(st_all_i) <- class(st_all) <- "statements"
      } 
      if (length(st_parent_i) > n_parent){
         if ("StatementConsolidatedStatementsOfEarnings2" %in% names(st_parent_i)){
           add_is2 <- TRUE
           IS2_parent <- st_parent_i[["StatementConsolidatedStatementsOfEarnings2"]]
           st_parent_i$StatementConsolidatedStatementsOfEarnings2 <- NULL
         } else {
           browser()
         }
      }
      if (length(st_all_i) > n_all){
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
        if ("StatementConsolidatedStatementsOfEarnings2" %in% names(st_parent)){
          add_is2 <- TRUE
          IS2_parent <- st_parent[["StatementConsolidatedStatementsOfEarnings2"]]
          st_parent[["StatementConsolidatedStatementsOfEarnings2"]] <- NULL
        } else {
          # browser()
        }
        # statements2excel(st_parent_i)
      }
      if (length(st_all_i) < n_parent){
        if ("StatementConsolidatedStatementsOfEarnings2" %in% names(st_all)){
          add_is2 <- TRUE
          IS2_all <- st_all[["StatementConsolidatedStatementsOfEarnings2"]]
          st_all[["StatementConsolidatedStatementsOfEarnings2"]] <- NULL
        } else {
          # browser()
        }
      }
      # different_names <- names(st_parent) != names(st_parent_i)
      # if (length(st_parent) <  4) browser()
      # if (sum(different_names) > 0){
       
        #st_parent_i <- statements_in_same_order(st_parent, st_parent_i)
      #  st_parent_i <- compare_statement_names(st_parent, st_parent_i)
      #}
      # different_names <- names(st_all) != names(st_all_i)
      # if (sum(different_names) > 0){
        # st_all_i <- statements_in_same_order(st_all, st_all_i)
      #   st_all_i <- compare_statement_names(st_all, st_all_i)
      # }
      # if (min(as.Date(st_all_10Q[[2]]$endDate)) == as.Date("2009-09-30")) browser() 
      # if (i == 14) browser()
      
      st_parent <- merge.statements(st_parent, st_parent_i, replace_na = TRUE,
                                    remove_dupes = TRUE, keep_first = TRUE)
      st_all <- merge.statements(st_all, st_all_i, replace_na = FALSE,
                                 remove_dupes = TRUE, keep_first = TRUE)
      if (add_is2){
        st_parent[["StatementConsolidatedStatementsOfEarnings2"]] <- IS2_parent
        st_all[["StatementConsolidatedStatementsOfEarnings2"]] <- IS2_all
      }
      if (remove_error_p){
        ind_error_p <- which(ind_error_p)
        for (j in 1:length(ind_error_p)){
          st_parent_i <- append(st_parent_i, remove_p_i[j], 
                                after = ind_error_p[j]-1)
          st_parent <- append(st_parent, remove_p[j], 
                                after = ind_error_p[j]-1)
          class(st_parent_i) <- class(st_parent) <- "statements"
        }
      }
      if (remove_error_a){
        ind_error_a <- which(ind_error_a)
        for (j in 1:length(ind_error_p)){
          st_all_i <- append(st_all_i, remove_a_i[j], 
                             after = ind_error_a[j]-1)
          st_all <- append(st_all, remove_a[j], 
                             after = ind_error_a[j]-1)
        }
        class(st_all_i) <- class(st_all) <- "statements"
      }
    }
    add_is2 <- FALSE
    if (form == "10-Q"){
      st_all_10Q <- st_all
      st_parent_10Q <- st_parent
    } else {
      st_all_10K <- st_all
      st_parent_10K <- st_parent
    }
    n_parent <- length(st_parent)
    n_all <- length(st_all)
    save(st_all_10Q, st_parent_10Q, st_all_10K, st_parent_10K,
         file = paste0("./data/",filename, ".RData"))
  }
  print("Done")
}

clean_BRKB_statement <- function(st, parent_only = FALSE, filter = FALSE){

  clean_statement <- function(st){
    # browser()
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
      # st_bu[is.na(st_bu)] <- 0 #is this correct?
      if (nrow(st_bu) > 0){
        st_bu[,empty_cols_bu & not_empty_cols_pa] <- NA
        st_bu_total <- st_bu |>
          dplyr::filter(value1 %in% c("brka:InsuranceAndOtherMember", 
                                      "brka:RailroadUtilitiesAndEnergyMember")) |> 
          dplyr::summarise(dplyr::across(value_cols, \(x) sum(x, na.rm = TRUE)))
        ind <- which(empty_cols_pa) - length(desc_vars)
        st_pa[, empty_cols_pa] <- st_bu_total[, ind]  
        st <- rbind(st_bu, st_parent = st_pa)
      } 
      if (parent_only){
        st <- st_pa  
      } 
    }
    return(st)
  }
  
  members <- c(NA, "brka:InsuranceAndOtherMember", "brka:RailroadUtilitiesAndEnergyMember", 
               "brka:CargoAndFreightMember", "brka:UtilitiesAndEnergyMember", 
               "us-gaap:CargoAndFreightMember", "brka:FinanceAndFinancialProductsMember",
               "brka:RailroadMember")
  
  if (class(st)[1] == "WB error") return(st)
  if (filter){
    st_temp <- st# can remove is for debugging
    st <- st |> dplyr::filter(value1 %in% members)
    # if (nrow(st) == 0){
    #   print(paste("filter removed all entries. Will disable filter for this statement"))
    #   st <- st_temp
    # }
  }
  if (nrow(st) == 0){
    ret <- "no valid observations after applying member filter"
    class(ret) <- "WB error"
    return(ret)
  } 
  periodID <- paste(st$startDate, st$endDate)
  st_l <- split(st, periodID)
  st_l <- lapply(st_l, clean_statement)
  if (lapply(st_l, is.null) |> unlist() |> sum() > 0){
    # debuging code, may erase
    browser()
    st_l <- split(st, periodID)
    st_l <- lapply(st_l, clean_statement)
  } 
  st <- do.call(rbind, st_l)
  if (is.null(st)) browser()
  # browser()
  return(st)
}

# present time series for different business units:
# revenue / operating expenses
# insurance
# energy
# rail roads
# investment gains
run_brkb_is_analysis <- function(st10Q, st10K){
  library(dplyr)
  library(xts)
  
  analysis <- function(is){
    is <- is[,-1]
    is$endDate <- as.Date(is$endDate)
    is <- is |> group_by(startDate, endDate, value1) |> summarise(across(everything(), sum))
    ins <- is |> filter(value1 %in% c("brka:InsuranceAndOtherMember", "brka:FinanceAndFinancialProductsMember"))
    # ins <- ins |> select_if(~ any(!is.na(.)) & any(. != 0))
    
    rail <- is |> filter(value1 %in% c("brka:CargoAndFreightMember", 
                                       "us-gaap:CargoAndFreightMember",
                                       "brka:RailroadMember"))
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
    # browser()
    # in "brka:CargoAndFreightMember", "brka:UtilitiesAndEnergyMember", "us-gaap:CargoAndFreightMember"
    freight_rev <- c("Revenues", "OperatingExpenses", "brka_CostsOfServicesAndOperatingExpenses", "brka_FreightRailTransportationRevenues", 
                     "brka_CargoAndFreightRevenueAndRegulatedAndUnregulatedOperatingRevenue") 
    energy_rev <- c("Revenues", "OperatingExpenses","RevenueFromContractWithCustomerExcludingAssessedTax", 
                    "brka_UtilityAndEnergyOperatingRevenues","brka_EnergyOperatingRevenues")
    
    # freight from 2019 to present, can merge these two data frames
    freight <- rail[,c("endDate", "value1", "Revenues", "OperatingExpenses")]
    freight1 <- infra[, c("endDate", "value1", "brka_FreightRailTransportationRevenues", "brka_CargoAndFreightRevenueAndRegulatedAndUnregulatedOperatingRevenue",
                          "OperatingExpenses")]
    freight1$"brka_FreightRailTransportationRevenues" <- 
      apply(freight1[, c("brka_FreightRailTransportationRevenues", 
                         "brka_CargoAndFreightRevenueAndRegulatedAndUnregulatedOperatingRevenue")], 
            1, sum, na.rm=TRUE)
    freight1 <- freight1[, -4]
    names(freight1) <- names(freight)
    freight1 <- freight1 |> 
      dplyr::filter(dplyr::if_any(c(Revenues, OperatingExpenses), ~ . != 0 & !is.na(.)))
    freight <- rbind(freight, freight1) 
    freight <- freight[order(freight$endDate), ]
    # Remove rows where the entire row has only NA or 0 values
    freight <- freight[apply(freight[,3:4], 1, function(row) !all(is.na(row) | row == 0)), ]
    freight <- freight[,-2]
    names(freight) <- c("endDate", "FreightRevenue", "FreightCosts")
    
    # freight <- xts(x = freight[,3:4], order.by = freight$endDate)
    # more data on rail business here: 
    # https://www.sec.gov/Archives/edgar/data/15511/000001551118000005/bnsfrailway-12312017x10xk.htm#sAE294AC985E55CB8887B10681A4F9210
    # https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=0000015511&owner=include&count=40&hidefilings=0
    energy <- energy[,c("endDate", "value1", "RevenueFromContractWithCustomerExcludingAssessedTax", "brka_CostsOfServicesAndOperatingExpenses")]
#    energy1 <- infra[, c("endDate", "value1", "brka_UtilityAndEnergyOperatingRevenues", #"brka_EnergyOperatingRevenues","OperatingExpenses", "brka_CostsOfServicesAndOperatingExpenses")]
        energy1 <- infra[, c("endDate", "value1", "brka_UtilityAndEnergyOperatingRevenues", "brka_EnergyOperatingRevenues", "brka_CostsOfServicesAndOperatingExpenses")]
    energy1$"brka_UtilityAndEnergyOperatingRevenues" <- 
      apply(energy1[, c("brka_UtilityAndEnergyOperatingRevenues", 
                        "brka_EnergyOperatingRevenues")], 
            1, sum, na.rm=TRUE)
    energy1 <- energy1[,-4]
    names(energy) <- names(energy1) <- c("endDate", "value1","Revenues","OperatingExpenses")
    energy1 <- energy1 |> 
      dplyr::filter(dplyr::if_any(c(Revenues, OperatingExpenses), ~ . != 0 & !is.na(.)))
    
    energy <- rbind(energy, energy1) # this can create duplicated rows
    energy <- energy[order(energy$endDate), ]
    energy <- energy[apply(energy[,3:4], 1, function(row) !all(is.na(row) | row == 0)), ]
    energy <- energy[, -2]
    names(energy) <- c("endDate", "EnergyRevenue", "EnergyCosts")
    # energy <- xts(x = energy[,3:4], order.by = energy$endDate)
    # browser()
    insurance <- ins[, c("endDate", "value1", "PremiumsEarnedNet", 
                         "LiabilityForUnpaidClaimsAndClaimsAdjustmentExpenseIncurredClaims1",
                         "IncurredClaimsPropertyCasualtyAndLiability",
                         "PolicyholderBenefitsAndClaimsIncurredNet",
                         "PolicyholderBenefitsAndClaimsIncurredLifeAndAnnuity",
                         "brka_PolicyholderBenefitsAndClaimsIncurredLifeAnnuityAndHealth",
                         # "PolicyholderBenefitsAndClaimsIncurredLifeAnnuityAndHealth",
                         # need to add additional member - "brka_FinanceAndFinancialProductsMember"
                         "brka_InsuranceUnderwritingExpenses",
                         "ExpenseRelatedToDistributionOrServicingAndUnderwritingFees")]
    insurance1 <- insurance |> filter(value1 == "brka:FinanceAndFinancialProductsMember")
    insurance<- insurance |> filter(value1 == "brka:InsuranceAndOtherMember")
    insurance$InsuranceUnderwritingExpenses <- 
      apply(insurance[, c("brka_InsuranceUnderwritingExpenses", 
                          "ExpenseRelatedToDistributionOrServicingAndUnderwritingFees")],
            1, sum, na.rm = TRUE)
    insurance$IncurredClaimsPropertyCasualtyAndLiability <- 
      apply(insurance[, c("IncurredClaimsPropertyCasualtyAndLiability", 
                          "LiabilityForUnpaidClaimsAndClaimsAdjustmentExpenseIncurredClaims1")],
            1, sum, na.rm = TRUE)
    insurance$PolicyHolderBenefitAndClaimsIncured <- 
      apply(insurance[, c("PolicyholderBenefitsAndClaimsIncurredNet",
                          "PolicyholderBenefitsAndClaimsIncurredLifeAndAnnuity",
                          "brka_PolicyholderBenefitsAndClaimsIncurredLifeAnnuityAndHealth")],
            1, sum, na.rm = TRUE)
    insurance <- insurance[, c("endDate", "PremiumsEarnedNet", 
                               "InsuranceUnderwritingExpenses",
                               "IncurredClaimsPropertyCasualtyAndLiability",
                               "PolicyHolderBenefitAndClaimsIncured")]
    # insurance <- xts(x = insurance[,-1], order.by = insurance$endDate)
    names(insurance) <- c("endDate", "InsPremiumsEarned", "InsUnderwritingExpenses", 
                          "ClaimsPropertyLiability", "PolicyHolderBenefits")
    insurance <- insurance |> 
      dplyr::filter(dplyr::if_any(c(InsPremiumsEarned, 
                                    InsUnderwritingExpenses, 
                                  ClaimsPropertyLiability, 
                                    PolicyHolderBenefits), ~ . != 0 & !is.na(.)))
    insurance1 <- insurance1[, c("endDate", "value1", "brka_PolicyholderBenefitsAndClaimsIncurredLifeAnnuityAndHealth")] #what is the use of insurance1?
    
    leasing <- ins[,c("endDate", "value1","OperatingLeaseLeaseIncome", 
                      "OperatingLeasesIncomeStatementLeaseRevenue",
                      "brka_CostOfLeasing")]
    leasing <- leasing |> filter(value1 == "brka:InsuranceAndOtherMember")
    # browser()
    leasing$OperatingLeaseLeaseIncome <- apply(leasing[, 
              c("OperatingLeaseLeaseIncome", "OperatingLeasesIncomeStatementLeaseRevenue")],
              1, sum, na.rm = TRUE)
    leasing <- leasing[, c("endDate", "OperatingLeaseLeaseIncome", 
                           "brka_CostOfLeasing")]
    # leasing <- xts(x = leasing[,3:4], order.by = leasing$endDate)
    names(leasing) <- c("endDate", "LeaseIncome", "CostOfLeasing")

    leasing <- leasing |> 
      dplyr::filter(dplyr::if_any(c(LeaseIncome, CostOfLeasing), ~ . != 0 & !is.na(.)))
    # RevenueFromContractWithCustomerIncludingAssessedTax does not exists in 10K
    service <- ins[,c("endDate", "value1","brka_SalesAndServiceRevenue",
                      "RevenueFromContractWithCustomerIncludingAssessedTax", 
                      "RevenueFromContractWithCustomerExcludingAssessedTax", 
                      "SalesRevenueNet",
                      "CostOfGoodsAndServicesSold")]
    service <- service |> filter(value1 == "brka:InsuranceAndOtherMember")
    service$Revenue <- apply(service[, c("brka_SalesAndServiceRevenue",
                                         "RevenueFromContractWithCustomerIncludingAssessedTax",
                                         "RevenueFromContractWithCustomerExcludingAssessedTax",
                                         "SalesRevenueNet")], 1, sum, na.rm = TRUE)
    service <- service[, c("endDate", "Revenue", "CostOfGoodsAndServicesSold")]
    names(service) <- c("endDate", "ServiceRevenue", "CostOfServices")
    service <- service |> 
      dplyr::filter(dplyr::if_any(c(ServiceRevenue, CostOfServices),
                                  ~ . != 0 & !is.na(.)))
    # service <- xts(x = service[,-1], order.by = service$endDate)
    
    investment1 <- ins[,c("endDate", "value1", "brka_InvestmentIncomeInterestDividendAndOther",
                          "InvestmentIncomeInterestAndDividend", "InterestExpense")]
    investment1 <- investment1 |> filter(value1 == "brka:InsuranceAndOtherMember")
    investment1$InvestmentIncomeInterestAndDividend <- 
      apply(investment1[, c("brka_InvestmentIncomeInterestDividendAndOther",
                            "InvestmentIncomeInterestAndDividend")], 1, sum, na.rm=TRUE)
    investment1 <- investment1[, c("endDate", "InvestmentIncomeInterestAndDividend", "InterestExpense")]
    investment1 <- investment1 |> 
      dplyr::filter(dplyr::if_any(c(InvestmentIncomeInterestAndDividend, InterestExpense),
                                  ~ . != 0 & !is.na(.)))
    
    investment <- is[c("endDate", "value1", "NonoperatingIncomeExpense",
                       "NonoperatingGainsLosses",
                       "GainLossOnInvestments",
                       "GainLossOnInvestmentsExcludingOtherThanTemporaryImpairments",
                       "OtherThanTemporaryImpairmentLossesInvestmentsPortionRecognizedInEarningsNet",
                       "IncomeLossFromEquityMethodInvestments")]
    
    investment <- investment[apply(investment[,3:7], 1, function(row) !all(is.na(row) | row == 0)), ]
    investment <- investment |> filter(is.na(value1))
    investment$GainLossOnInvestments <- 
      apply(investment[, c("NonoperatingGainsLosses", 
                           "NonoperatingIncomeExpense",
                           "GainLossOnInvestments",
                           "GainLossOnInvestmentsExcludingOtherThanTemporaryImpairments",
                           "OtherThanTemporaryImpairmentLossesInvestmentsPortionRecognizedInEarningsNet")], 
            1, sum, na.rm=TRUE)
    investment <- investment[, c("endDate", "GainLossOnInvestments", 
                                 "IncomeLossFromEquityMethodInvestments")]
    investment <- merge(investment, investment1, all = TRUE)
    names(investment) <- c("endDate", "InvestmentGains", "EquityMethodIncome", 
                           "InterestDividendIncome", "InterestExpense")
    # investment <- xts(x = investment[,-1], order.by = investment$endDate)
    
    other_costs_ins <- ins["SellingGeneralAndAdministrativeExpense"]
    interest_exp_infra <- "InterestExpense"
    
    service_cost <- "OtherFinancialServicesCosts" # not part of insurance and other group

    ret <- merge(investment, insurance, all = TRUE)
    ret <- merge(ret, energy, all = TRUE)
    ret <- merge(ret, freight, all = TRUE)
    ret <- merge(ret, leasing, all = TRUE)
    ret <- merge(ret, service, all = TRUE)
    return(ret)
  }
  # browser()
  source("./scripts/finstr/finstr.R")
  is10Q <- st10Q[[2]]
  is10K <- st10K[[2]]
  is <- merge.statement(is10Q, is10K, by = c("contextId", "startDate", "endDate", "decimals", "value1"))
  is$endDate <- as.Date(is$endDate)
  is$startDate <- as.Date(is$startDate)
  period <- lubridate::month(is$endDate) - lubridate::month(is$startDate)
  is9M <- is[period == 8, ]
  is12M <- is[period == 11, ]
  is3M <- is[period == 2, ]
  data3M <- analysis(is3M)
  data9M <- analysis(is9M)
  data12M <- analysis(is12M)
  eoy <- lubridate::ceiling_date(data9M$endDate, "year") - lubridate::days(1)
  data9M <- data9M[eoy %in% data12M$endDate,]
  eoy <- lubridate::ceiling_date(data9M$endDate, "year") - lubridate::days(1)
  ind <- match(eoy, data12M$endDate)
  dataQ4 <- data12M[ind,-1] - data9M[,-1] #ind has NA's data9M
  # browser()
  dataQ4 <- cbind(eoy, dataQ4)
  names(dataQ4)[1] <- "endDate"
  data3M <- rbind(data3M, dataQ4)
  data3M <- xts(x = data3M[, -1], order.by = data3M$endDate)
  data12M <- xts(x = data12M[, -1], order.by = data12M$endDate)
  
  save(data3M, data12M, file = "./data/BRKB_income_bu.Rdata")
}

brkb_operatingincome <- function(st){
  # input statement parent
  library(xts)
  is <- st[[2]]
  is <- is |> select_if(~ any(!is.na(.)) & any(. != 0))
  is$endDate <- as.Date(is$endDate)
  # nr_shares <- is[,c("endDate", "WeightedAverageNumberOfSharesOutstandingBasic")]
  # nr_shares has too many zeros
  income <- is[,c("endDate",
  "ProfitLoss",
  "IncomeTaxExpenseBenefit",
  "NetIncomeLossAttributableToNoncontrollingInterest",
  "NetIncomeLoss")]
  
  investment_income <-is[,c("endDate",
                            "GainLossOnInvestmentsExcludingOtherThanTemporaryImpairments",
                            "OtherThanTemporaryImpairmentLossesInvestmentsPortionRecognizedInEarningsNet",
                            "InvestmentIncomeInterestAndDividend",
                            "brka_InvestmentIncomeInterestDividendAndOther",
                            "GainLossOnInvestments",
                            "NonoperatingGainsLosses",
                            "NonoperatingIncomeExpense",
                            "InterestExpense",
                            "GainLossOnDerivativeInstrumentsNetPretax"
                            )]
  
  investment_income$InvestmentIncomeInterestAndDividend <- 
    investment_income$InvestmentIncomeInterestAndDividend +
    investment_income$brka_InvestmentIncomeInterestDividendAndOther
  
  investment_income <- investment_income |> dplyr::select(-brka_InvestmentIncomeInterestDividendAndOther)
  investment_income <- investment_income |> dplyr::mutate(NonoperatingGainsLosses =
                    NonoperatingGainsLosses + NonoperatingIncomeExpense)
  investment_income <- investment_income |> dplyr::mutate(GainLossOnInvestments =
                                                            GainLossOnInvestments + 
                                                            GainLossOnDerivativeInstrumentsNetPretax)
  investment_income <- investment_income |> dplyr::mutate(GainLossOnInvestments =
      ifelse(GainLossOnInvestments == 0 | NonoperatingGainsLosses == 0,
             GainLossOnInvestments + NonoperatingGainsLosses, GainLossOnInvestments))

    investment_income <- investment_income |> dplyr::select(-c(NonoperatingIncomeExpense,
                                                            GainLossOnDerivativeInstrumentsNetPretax,
                                                            NonoperatingGainsLosses))
  investment_income <- investment_income |> dplyr::filter(endDate > as.Date("2013-01-01")) 
  investment_income <- investment_income |>
    dplyr::select(-c(GainLossOnInvestmentsExcludingOtherThanTemporaryImpairments,
                     OtherThanTemporaryImpairmentLossesInvestmentsPortionRecognizedInEarningsNet))
  investment_income <- xts(x = investment_income[, -1], order.by = investment_income$endDate)
  #nr_shares <- xts(x = nr_shares[, -1], order.by = nr_shares$endDate)
  income <- xts(x = income[,-1], order.by = income$endDate)
  ret <- merge.xts(income, investment_income, all = TRUE)
  return(ret)
}

# create a time series with share buy back and estimated buy back prices
brkb_shr_buybacks_analysis <- function(st){
  library(dplyr)
  library(xts)
  cf <- st[[4]]
  cf <- cf |> select_if(~ any(!is.na(.)) & any(. != 0))
  equity_p_and_s <- cf[,c("startDate", "endDate", 
                          "PaymentsToAcquireEquitySecuritiesFvNi",
                          "ProceedsFromSaleOfEquitySecuritiesFvNi",
    "brka_ProceedsFromSalesAndRedemptionsOfAvailableForSaleSecuritiesEquitySecurities",
    "brka_ProceedsFromSalesAndRedemptionsOfEquitySecurities",
    "PaymentsForRepurchaseOfCommonStock",
    "brka_PaymentsToAcquireEquitySecurities",
    "brka_ProceedsFromSalesOfEquitySecurities",
    "PaymentsToAcquireAvailableForSaleSecuritiesEquity")]
  
  browser()
  
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
