# https://www.sec.gov/data-research/sec-markets-data/form-n-port-data-sets
# quarterly sec filings with holdings from all S mutual funds


brkb_statements <- function(form = "10-Q", years = 13, 
                            filename = "BRKB_statements.RData",
                            arc = "presentation",
                            parent_only = FALSE){
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
  if (file.exists(paste0("./data/", filename))){
    load(paste0("./data/", filename))
  }
  # object_names <- c("st_all_10Q", "st_parent_10Q", "st_all_10K", "st_parent_10K")
  object_names <- c("st_all_10Q", "st_all_10K")
  for (i in 1:length(object_names)){
    if (!exists(object_names[i])){
      assign(object_names[i], 0)
    }
  }
  
  if (!exists("shares_outstanding")) hares_outstanding <- NULL
  
  for (i in 1:min(n, length(filing_urls))){
    if (sum(is.na(filing_urls) > 0)) browser()
    if (nchar(filing_urls[i]) < 5 | is.na(filing_urls[i])) browser()
    xml_filenames <- edgar_xbrl_URLs(paste0("https://www.sec.gov", filing_urls[i]),
                                     verbose = TRUE)
    if ("WB error" %in% class(xml_filenames)){
      next
    }
    print(paste("i = ", i, "XML file:", xml_filenames[1]))
    
    xbrl <- parse_xbrl(xml_filenames, cache_dir = "xbrl/cache_dir/")
    xbrl$fact <- remove_duplicated_facts(xbrl$fact)
    shrs <- outstanding_shares(xbrl)
    shares_outstanding <- rbind(shares_outstanding, shrs)
    shares_outstanding <- shares_outstanding[!duplicated(shares_outstanding), ]
    st <- xbrl_get_statements_WB(xbrl_vars = xbrl, 
                                 lbase = arc,
                                 complete_first = FALSE, 
                                 end_of_quarter = FALSE,
                                 basic_contexts = FALSE,
                                 nonzero_only = TRUE,
                                 regular_sec_reporting_periods = TRUE,
                                 nr_periods = 2)
    remove_error_a <- FALSE
    if (i == 1){
      # st_parent <- lapply(st, clean_BRKB_statement, parent_only = TRUE, filter = TRUE)
      st_all <- lapply(st, clean_BRKB_statement, parent_only = parent_only, filter = FALSE)
      class(st_all) <- "statements"
      # class(st_parent) <- "statements"
      # n_parent <- length(st_parent)
      n_all <- length(st_all)
    } else {
      st_all_i <- lapply(st, clean_BRKB_statement, parent_only = parent_only, filter = TRUE)
      class(st_all_i) <- "statements"
      if (sum(lapply(st_all_i, class) |> unlist() == "WB error") == length(st_all_i)){
        print(paste("There are no statements for i", i))
        # only errors, nothing to merge.
        next
      }
      st_all_i <- statements_in_same_order(st_all, st_all_i)
      if ("WB error" %in% lapply(st_all_i, class)){
        remove_error_a <- TRUE
        ind_error_a <- sapply(st_all_i, function(x) "WB error" %in% class(x))
        remove_a_i <- st_all_i[ind_error_a]
        remove_a <-  st_all[ind_error_a]
        st_all_i <- st_all_i[!ind_error_a]
        st_all <- st_all[!ind_error_a]
        class(st_all_i) <- class(st_all) <- "statements"
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
      if (length(st_all_i) < n_all){
        if ("StatementConsolidatedStatementsOfEarnings2" %in% names(st_all)){
          add_is2 <- TRUE
          IS2_all <- st_all[["StatementConsolidatedStatementsOfEarnings2"]]
          st_all[["StatementConsolidatedStatementsOfEarnings2"]] <- NULL
        } else {
          # browser()
        }
      }
      st_all <- merge.statements(st_all, st_all_i, replace_na = FALSE,
                                 remove_dupes = TRUE, keep_first = TRUE)
      if (add_is2){
      #  st_parent[["StatementConsolidatedStatementsOfEarnings2"]] <- IS2_parent
        st_all[["StatementConsolidatedStatementsOfEarnings2"]] <- IS2_all
      }
      if (remove_error_a){
        ind_error_a <- which(ind_error_a)
        for (j in 1:length(ind_error_a)){
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
    #  st_parent_10Q <- st_parent
    } else {
      st_all_10K <- st_all
    #  st_parent_10K <- st_parent
    }
    n_all <- length(st_all)
    dupes <- duplicated(shares_outstanding)
    shares_outstanding <- shares_outstanding[!dupes, ]
    save(st_all_10Q, st_all_10K, shares_outstanding,
        file = paste0("./data/",filename))
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
  return(st)
}

# present time series for different business units:
# revenue / operating expenses
# insurance
# energy
# rail roads
# investment gains
run_brkb_analysis <- function(st10Q, st10K, shares_outstanding){
  library(dplyr)
  library(xts)
  
  is_analysis <- function(is){
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
    freight <- freight[!duplicated(freight$endDate), ]
    freight <- freight[order(freight$endDate), ]
    # Remove rows where the entire row has only NA or 0 values
    freight <- freight[apply(freight[,3:4], 1, function(row) !all(is.na(row) | row == 0)), ]
    freight <- freight[,-2]
    names(freight) <- c("endDate", "FreightRevenue", "FreightCosts")
    
    # more data on rail business here: 
    # https://www.sec.gov/Archives/edgar/data/15511/000001551118000005/bnsfrailway-12312017x10xk.htm#sAE294AC985E55CB8887B10681A4F9210
    # https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=0000015511&owner=include&count=40&hidefilings=0
    energy <- energy[,c("endDate", "value1", "RevenueFromContractWithCustomerExcludingAssessedTax", "brka_CostsOfServicesAndOperatingExpenses")]
        energy1 <- infra[, c("endDate", "value1", "brka_UtilityAndEnergyOperatingRevenues", "brka_EnergyOperatingRevenues", "brka_CostsOfServicesAndOperatingExpenses")]
    energy1$"brka_UtilityAndEnergyOperatingRevenues" <- 
      apply(energy1[, c("brka_UtilityAndEnergyOperatingRevenues", 
                        "brka_EnergyOperatingRevenues")], 
            1, sum, na.rm=TRUE)
    energy1 <- energy1[,-4]
    names(energy) <- names(energy1) <- c("endDate", "value1","Revenues","OperatingExpenses")
    energy1 <- energy1 |> 
      dplyr::filter(dplyr::if_any(c(Revenues, OperatingExpenses), ~ . != 0 & !is.na(.)))
    # if (rbind(energy, energy1)$endDate |> duplicated() |> sum() > 0) browser()
    energy <- rbind(energy, energy1) # this can create duplicated rows
    energy <- energy[!duplicated(energy$endDate), ]
    energy <- energy[order(energy$endDate), ]
    energy <- energy[apply(energy[,3:4], 1, function(row) !all(is.na(row) | row == 0)), ]
    energy <- energy[, -2]
    names(energy) <- c("endDate", "EnergyRevenue", "EnergyCosts")
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
    leasing$OperatingLeaseLeaseIncome <- apply(leasing[, 
              c("OperatingLeaseLeaseIncome", "OperatingLeasesIncomeStatementLeaseRevenue")],
              1, sum, na.rm = TRUE)
    leasing <- leasing[, c("endDate", "OperatingLeaseLeaseIncome", 
                           "brka_CostOfLeasing")]
    names(leasing) <- c("endDate", "LeaseIncome", "CostOfLeasing")

    leasing <- leasing |> 
      dplyr::filter(dplyr::if_any(c(LeaseIncome, CostOfLeasing), ~ . != 0 & !is.na(.)))
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
  
  cf_analysis <- function(st){
    library(dplyr)
    st <- st[,-1]
    st$endDate <- as.Date(st$endDate)
    st <- st |> filter(is.na(value1) | value1 == 0)
    equity_p_and_s <- st[,c("startDate", "endDate", 
                            "PaymentsToAcquireEquitySecuritiesFvNi",
                            "brka_PaymentsToAcquireEquitySecurities",
                            "ProceedsFromSaleOfEquitySecuritiesFvNi",
                            "brka_ProceedsFromSalesOfEquitySecurities",
                            "PaymentsToAcquireAvailableForSaleSecuritiesEquity",
                            "PaymentsToAcquireEquityMethodInvestments",
                            "brka_ProceedsFromSalesAndRedemptionsOfAvailableForSaleSecuritiesEquitySecurities",
                            "brka_ProceedsFromSalesAndRedemptionsOfEquitySecurities",
                            "PaymentsForRepurchaseOfCommonStock",
                            "brka_PaymentsToAcquirePreferredStockCommonStockAndOtherInvestments")]
    
    equity_p_and_s$equity_purchases <- 
      apply(st[, c("PaymentsToAcquireEquitySecuritiesFvNi",
                   "brka_PaymentsToAcquireEquitySecurities",
                   "PaymentsToAcquireAvailableForSaleSecuritiesEquity",
                   "PaymentsToAcquireEquityMethodInvestments")], 
            1, sum, na.rm=TRUE)
    equity_p_and_s$equity_sales <- 
      apply(st[, c("ProceedsFromSaleOfEquitySecuritiesFvNi",
                   "brka_ProceedsFromSalesOfEquitySecurities",
                   "brka_ProceedsFromSalesAndRedemptionsOfAvailableForSaleSecuritiesEquitySecurities",
                   "brka_ProceedsFromSalesAndRedemptionsOfEquitySecurities")], 
            1, sum, na.rm=TRUE)
    equity_p_and_s <-  equity_p_and_s[, c("startDate", "endDate", 
                                          "equity_purchases",
                                          "equity_sales",
                                          "PaymentsForRepurchaseOfCommonStock")]
    
    other_cf <- st[ ,c("startDate", "endDate", "ProfitLoss",
                       "GainLossOnInvestments", "DepreciationDepletionAndAmortization",
                       "Depreciation", "PaymentsToAcquirePropertyPlantAndEquipment")]
    other_cf$depr <- apply(st[, c("Depreciation", "DepreciationDepletionAndAmortization")],1,sum, na.rm=TRUE)
    other_cf <- other_cf[, c("startDate", "endDate", "ProfitLoss",
                           "GainLossOnInvestments", "depr", "PaymentsToAcquirePropertyPlantAndEquipment")]
    names(other_cf) <- c("startDate", "endDate", "PnL",
                         "investment_gains", "depreciation", "plant_and_equipment")
    class(other_cf) <- setdiff(class(other_cf), "statement")
    class(equity_p_and_s) <- setdiff(class(equity_p_and_s), "statement")
    ret <- merge(other_cf, equity_p_and_s)
    ret <- ret[!apply(ret[,-(1:2)], 1, function(row) all(is.na(row) | row == 0)), ]
    ret <- ret[,-1] # remove start date
    return(ret)
  }
  
  bs_analysis <- function(st){
    bs <- st
    bs <- bs[is.na(bs$value1) | bs$value1 == 0, ]
    bs$endDate <- as.Date(bs$endDate)
    bs <- bs[,-1]
    # bs <- bs |> select_if(~ any(!is.na(.)) & any(. != 0))
    bs <- bs[, c("endDate","CashAndCashEquivalentsAtCarryingValue", 
                 "brka_USTreasuryBills", #"brka_CashCashEquivalentsAndUSTreasuryBills",
                 "AvailableForSaleSecuritiesEquitySecurities",
                 "EquitySecuritiesFvNi", "MarketableSecuritiesEquitySecurities",
                 "brka_EquityMethodInvestmentsInPreferredStockAndCommonStock",
                 "EquityMethodInvestments", "AdditionalPaidInCapitalCommonStock",
                 "StockholdersEquity")]
    bs$cash_equiv <- apply(bs[, c("CashAndCashEquivalentsAtCarryingValue", 
                                  "brka_USTreasuryBills")], 1, sum, na.rm=TRUE)
    bs$m2m_equities <-apply(bs[, c("AvailableForSaleSecuritiesEquitySecurities",
                                   "EquitySecuritiesFvNi", 
                                   "MarketableSecuritiesEquitySecurities")], 
                            1, sum, na.rm=TRUE)
    bs$equities_at_cost <-apply(bs[, c("brka_EquityMethodInvestmentsInPreferredStockAndCommonStock",
                                       "EquityMethodInvestments")], 
                                1, sum, na.rm=TRUE)
    bs <- bs[, c("endDate", "cash_equiv", "m2m_equities", "equities_at_cost",
                 "AdditionalPaidInCapitalCommonStock",
                 "StockholdersEquity")]
    dupe1 <- duplicated(bs$endDate, fromLast = TRUE)
    dupe2 <- duplicated(bs$endDate, fromLast = FALSE)
    missing1 <- rowSums(is.na(bs[dupe1, ]) | bs[dupe1, ] == 0, na.rm = TRUE)
    missing2 <- rowSums(is.na(bs[dupe2, ]) | bs[dupe1, ] == 0, na.rm = TRUE)
    bs <- bs[!dupe1, ]
    return(bs)
  }
  
  run_cashflow_or_income_analysis <- function(st10Q, st10K, analysis = "IS"){
    if (analysis == "IS"){
      st10Q <- st10Q[[2]]
      st10K <- st10K[[2]]
    }
    if (analysis == "CF")
    {
      st10Q <- st10Q[[4]]
      st10K <- st10K[[4]]
    }
    if (analysis == "BS"){
      st10Q <- st10Q[[1]]
      st10K <- st10K[[1]]
    }
    source("./scripts/finstr/finstr.R")
    st <- merge.statement(st10Q, st10K, by = c("contextId", "startDate", "endDate", "decimals", "value1"))
    st$endDate <- as.Date(st$endDate)
    st$startDate <- as.Date(st$startDate)
    if (analysis != "BS"){
      period <- lubridate::month(st$endDate) - lubridate::month(st$startDate)
      st9M <- st[period == 8, ]
      st12M <- st[period == 11, ]
      st3M <- st[period == 2, ]
    }
    if (analysis == "IS"){
      data3M <- is_analysis(st3M)
      data9M <- is_analysis(st9M)
      # browser()
      data12M <- is_analysis(st12M)
    } 
    if (analysis == "CF") {
      data3M <- cf_analysis(st3M)
      data9M <- cf_analysis(st9M)
      data12M <- cf_analysis(st12M)
    }
    if (analysis == "BS"){
      data3M <- bs_analysis(st)
      st12M <- st |> dplyr::filter(endDate == 
                                     lubridate::ceiling_date(st$endDate, "year") -
                                     lubridate::days(1))
      data12M <- bs_analysis(st12M)
    }
    if (analysis != "BS"){
      eoy <- lubridate::ceiling_date(data9M$endDate, "year") - lubridate::days(1)
      data9M <- data9M[eoy %in% data12M$endDate,]
      eoy <- lubridate::ceiling_date(data9M$endDate, "year") - lubridate::days(1)
      ind <- match(eoy, data12M$endDate)
      dataQ4 <- data12M[ind,-1] - data9M[,-1] #ind has NA's data9M
      dataQ4 <- cbind(eoy, dataQ4)
      names(dataQ4)[1] <- "endDate"
      data3M <- rbind(data3M, dataQ4)
    }
    
    data3M <- xts(x = data3M[, -1], order.by = data3M$endDate)
    data12M <- xts(x = data12M[, -1], order.by = data12M$endDate)
    return(list(data3M, data12M))
  }
  
  # add data on number of outstanding shares
  func_nr_shares <- function(shares_outstanding){
    shares_outstanding$fact <- as.numeric(shares_outstanding$fact)
    shares_outstanding$startDate <- as.Date(shares_outstanding$startDate)
    shares_outstanding$endDate <- as.Date(shares_outstanding$endDate)
    shares_outstanding <- shares_outstanding |> dplyr::arrange(endDate, startDate)
    common_shrs <- shares_outstanding |> dplyr::filter(elementId == 
                                       "us-gaap_CommonStockSharesOutstanding")
    common_shrs <- common_shrs[, -1]
    common_shrs <- common_shrs |> tidyr::pivot_wider(values_from = fact, names_from = value1)
    common_shrs <- common_shrs[, c("endDate",
                                   "us-gaap:CommonClassAMember",
                                   "us-gaap:CommonClassBMember",
                                   "brka:EquivalentClassAMember")]
    names(common_shrs) <- c("endDate", "a_shrs", "b_shrs", "a_equiv_shrs")
    ind <- is.na(common_shrs$a_equiv_shrs)
    common_shrs$a_equiv_shrs[ind] <- common_shrs$a_shrs[ind] + common_shrs$b_shrs[ind] / 1500
    common_shrs <- xts(x = common_shrs[,-1], order.by = as.Date(common_shrs$endDate))
    weighted_shrs <- shares_outstanding |> dplyr::filter(!is.na(startDate) & 
             elementId == "us-gaap_WeightedAverageNumberOfSharesOutstandingBasic")
    ind <- weighted_shrs$fact / 10^11 > 1
    weighted_shrs$fact[ind] <-  weighted_shrs$fact[ind]/10^6
    dupes <- duplicated(weighted_shrs[,-5])
    weighted_shrs <- weighted_shrs[!dupes, ]
    weighted_shrs <- weighted_shrs |> tidyr::pivot_wider(values_from = fact, names_from = value1)
    ind <- is.na(weighted_shrs$`brka:EquivalentClassAMember`)
    weighted_shrs$`brka:EquivalentClassAMember`[ind] <- weighted_shrs$`us-gaap:CommonClassAMember`[ind]
    ind <- is.na(weighted_shrs$`brka:EquivalentClassAMember`)
    weighted_shrs$`brka:EquivalentClassAMember`[ind] <- weighted_shrs$`NA`[ind]
    weighted_shrs <- weighted_shrs[,c("startDate","endDate","brka:EquivalentClassAMember")]
    names(weighted_shrs)[3] <- "weighted_a_shrs"
    period <- lubridate::month(weighted_shrs$endDate) - 
      lubridate::month(weighted_shrs$startDate)
    shrs3M <- weighted_shrs[period == 2, ]
    shrs12M <- weighted_shrs[period == 11, ]
    shrs9M <- weighted_shrs[period == 8, ]
    eoy <- lubridate::ceiling_date(shrs9M$endDate, "year") - lubridate::days(1)
    shrs9M <- shrs9M[eoy %in% shrs12M$endDate,]
    eoy <- lubridate::ceiling_date(shrs9M$endDate, "year") - lubridate::days(1)
    ind <- match(eoy, shrs12M$endDate)
    shrsQ4 <- (shrs12M$weighted_a_shrs[ind] - 0.75 * shrs9M$weighted_a_shrs) * 4
    shrsQ4 <- data.frame(endDate = eoy, weighted_a_shrs = shrsQ4)
    names(shrsQ4)[1] <- "endDate"
    shrs3M <- shrs3M[,-1]
    shrs3M <- rbind(shrs3M, shrsQ4)
    shrs3M <- xts(x = shrs3M$weighted_a_shrs, order.by = shrs3M$endDate)
    names(shrs3M) <- "avg_a_equiv_shrs"
    shrs3M <- merge.xts(common_shrs, shrs3M, all = TRUE)
    common_shrs <- common_shrs[format(index(common_shrs), "%m-%d") == "12-31"]
    shrs12M <- xts(x = shrs12M$weighted_a_shrs, order.by = shrs12M$endDate)
    names(shrs12M) <- "avg_a_equiv_shrs"
    shrs12M <- merge.xts(common_shrs, shrs12M, all = TRUE)
    return(list(shrs3M, shrs12M))
  }
  
  is_figures <- run_cashflow_or_income_analysis(st10Q, st10K, analysis = "IS")
  cf_figures <- run_cashflow_or_income_analysis(st10Q, st10K, analysis = "CF")
  bs_figures <- run_cashflow_or_income_analysis(st10Q, st10K, analysis = "BS")
  nr_shares <- func_nr_shares(shares_outstanding)
  data3M <- merge(is_figures[[1]], cf_figures[[1]])
  data3M <- merge(data3M, bs_figures[[1]])
  data3M <- merge(data3M, nr_shares[[1]])
  data12M <- merge(is_figures[[2]], cf_figures[[2]])
  data12M <- merge(data12M, bs_figures[[2]])
  data12M <- merge(data12M, nr_shares[[2]])
  
  save(data3M, data12M, file = "./data/BRKB_income_bu.Rdata")
}

# find common shares outstanding
# find weighted number of shares outstanding
outstanding_shares <- function(xbrl){
  # browser()
  # #matching_facts <- xbrl$fact |> dplyr::filter(unitId == "U_shares")
  # matching_facts <- xbrl$fact
  # matching_facts <- matching_facts |> dplyr::left_join(xbrl$context, by = dplyr::join_by(contextId))
  # matching_facts <- matching_facts |> dplyr::left_join(xbrl$label, by = dplyr::join_by(elementId))
  # ind <- stringr::str_detect(tolower(matching_facts$elementId), pattern = "outstanding") |
  #   stringr::str_detect(tolower(matching_facts$labelString), pattern = "outstanding")
  # matching_facts <- matching_facts[ind, ]
  # matching_facts <- matching_facts[!duplicated(matching_facts$factId), ]
  # matching_facts <- matching_facts |> dplyr::filter(unitId == "U_shares")
  # 
  
  ind_element <- stringr::str_detect(tolower(xbrl$fact$elementId), pattern = "outstanding")
  ind_element[is.na(ind_element)] <- FALSE
  matching_facts <- xbrl$fact[ind_element, ]
  matching_facts <- matching_facts |> dplyr::left_join(xbrl$context, by = dplyr::join_by(contextId))
  matching_facts <- matching_facts |> dplyr::filter(stringr::str_detect(unitId, "shares"))
  # matching_facts <- matching_facts |> dplyr::filter(value1 == "brka:EquivalentClassAMember")
  matching_facts <- matching_facts |> dplyr::select(c("elementId", 
                                                      "startDate", "endDate", 
                                                      "value1", "fact"))
  return(matching_facts)
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



