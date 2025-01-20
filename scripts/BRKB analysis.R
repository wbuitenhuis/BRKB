# from 2022 annual report
# nr_a_shares <- 591564
# nr_b_shares <- -3
# b_shares_equiv <- nr_b_shares + nr_a_shares * 1500
#


# report_date <- as.Date("2023-12-31")

value_BRK_investments <-function(start_dt, end_dt){
  # improvements: do not make share count jump by quarter, but estimate share count by day
  
  library(xts)
  library(NasdaqDataLink)
  
  investor <- "BERKSHIRE HATHAWAY INC"
  symbol = "BRK.B"
  NasdaqDataLink.api_key("s8rJbJ-oz8hPyu6_PC7K")
  
  dates_13F <- seq(as.yearqtr(start_dt), as.yearqtr(end_dt), by = 1/4) |> as.Date(frac = 1)
  dates_13F <- dates_13F[dates_13F < as.Date(Sys.Date() - 7 * 5)]
  end_dates <- c(dates_13F[-1], end_dt)
  
  investments <- NULL# 
  investment_per_share <- NULL
  b_shares_equiv <- NULL
  q_dates <- NULL
  bs_items <- bs_per_shr <- NULL
  prev_per_inv <- NULL
  for (i in 1:length(dates_13F)){
    # obtain investments of Berkshire (source 13F)
    holdings <- NasdaqDataLink.datatable('SHARADAR/SF3', calendardate = dates_13F[i], 
                                         investorname = investor, paginate = TRUE)
    if (nrow(holdings) == 0){
      # No data for 13F
      next
    }
    # Obtain balance sheet items of Berkshire (source 10Q, 10K)
    data <- NasdaqDataLink.datatable('SHARADAR/SF1', calendardate = dates_13F[i], 
                                     ticker = symbol, paginate = TRUE)
    row <- which(data$dimension == "MRQ") #most recent reported quarter
    nr_b_shares <- data[row, "shareswa"]
    latest_b_shares_equiv <- data[row, "shareswadil"]
    i.bs_items <- data[row, c("cashnequsd", "investments")]
    if (nrow(data) == 0){
      # No data for 10Q / 10K
      if (is.null(bs_items)){
        next
      } else {
        latest_b_shares_equiv <- b_shares_equiv[i - 1]
        i.bs_items <- bs_items[i - 1, ]  
      }
    }
    b_shares_equiv <- rbind(b_shares_equiv, latest_b_shares_equiv)
    bs_items <- rbind(bs_items, i.bs_items)
    i.bs_per_shr <- i.bs_items / latest_b_shares_equiv
    bs_per_shr <- rbind(bs_per_shr, i.bs_per_shr)
    q_dates <- rbind(q_dates, dates_13F[i])
    
    # Obtain daily prices of investments of Berkshire
    prices <- NasdaqDataLink.datatable('SHARADAR/SEP', date.gte=as.Date(dates_13F[i] + 1), 
                                       date.lte=end_dates[i], 
                                       ticker=holdings$ticker, paginate = TRUE)
    ind <- match(prices$ticker, holdings$ticker)
    prices$units <- holdings$units[ind]
    prices$value <- prices$closeunadj * prices$units
    i.investments <- tapply(prices$value, prices$date, sum)
    i.investments <- xts(x = i.investments, order.by = as.Date(names(i.investments)))
    i.investments <- i.investments - rep(i.investments[1,], length(i.investments)) + sum(i.bs_items)
    investments <- rbind(i.investments, investments)
    # if (!is.null(prev_per_inv)){
    #   n <- nrow(prev_per_inv)
    #   nr_shares <- seq(from = b_shares_equiv[i-1], to= b_shares_equiv[i], length.out = n)
    #   i.investments_per_share <- prev_per_inv  / nr_shares
    #   investment_per_share <- rbind(i.investments_per_share, investment_per_share)  
    # }
    i.investments_per_share <- i.investments  / b_shares_equiv[i]
     investment_per_share <- rbind(i.investments_per_share, investment_per_share)
    prev_per_inv <- i.investments
  }
  # n <- nrow(prev_per_inv)
  # nr_shares <- seq(from = b_shares_equiv[i-1], to= b_shares_equiv[i], length.out = n)
  # i.investments_per_share <- prev_per_inv  / nr_shares
  # investment_per_share <- rbind(i.investments_per_share, investment_per_share)  
  # 
  investments <- investments / 10^6
  q_data <- cbind(b_shares_equiv, bs_items / 10^6, bs_per_shr)
  q_data <- xts(x=q_data, order.by = as.Date(q_dates))
  colnames(q_data) <- c("shr_out", "cash", "investments", "cash_per_shr", "inv_per_shr")
  data <- cbind(investments, investment_per_share)
  prices <- NasdaqDataLink.datatable('SHARADAR/SEP', date.gte=min(index(data)), 
                                     date.lte=max(index(data)), 
                                     ticker=c("BRK.B", "BRK.A"), paginate = TRUE)
  prices <- prices[, c("ticker", "date", "close")]
  prices <- tidyr::pivot_wider(prices, names_from = "ticker", values_from = "close")
  prices <- xts(prices[, -1], order.by = prices$date)
  data <- merge.xts(data, prices, join = "inner")
  premium <- (data$BRK.B - data$BRK.A / 1500) / data$BRK.B
  colnames(premium) <- "premium"
  d_data <- cbind(data, premium)
  return(list(d_data, q_data))
}

start_dt <- as.Date("2018-03-31")
end_dt <- as.Date(Sys.Date()-1)

#debugonce(value_BRK_investments)
BRKB <- value_BRK_investments(start_dt, end_dt)
#premium <- (BRKB$BRK.B - BRKB$BRK.A / 1500) / BRKB$BRK.B
plot(BRKB[[1]]$BRK.B - BRKB[[1]]$investment_per_share)
plot((BRKB[[1]]$BRK.B - BRKB[[1]]$investment_per_share)/BRKB[[1]]$BRK.B) #this one is key
plot(BRKB$investment_per_share)
plot(BRKB$premium)

data <- NasdaqDataLink.datatable('SHARADAR/SF1', calendardate = as.Date("2023-12-31"), 
                                 ticker = "BRK.B", paginate = TRUE)
fields <- c("investmentsnc", "investmentsc","invcap", "investments")
# investments = all investments in equities, treasuries and loans. excludes cash
data[,fields]

data_dictionary <- NasdaqDataLink.datatable('SHARADAR/INDICATORS')




# A - as reported
# M - Most recent reported
# Y - Year
# Q - quarter
# T - Trailing 12 months

source("./scripts/functions/10Q_10K_functions.R")
edgar_timeseries_10q()

source("./scripts/functions/brkb_analysis_functions.R")
# # sink(file=paste0(Sys.Date(), ".log"))
#brkb_statements(form = "10-K", years = 2, filename = "test2y")
# rm(list = ls())
#load(file = "./data/test2y.RData")

source("./scripts/functions/brkb_analysis_functions.R")
brkb_statements(form = "10-K", years = 15, arc = "presentation")
brkb_statements(form = "10-Q", years = 17, arc = "presentation")

# sink()
print("done")

# load(file = "./data/BRKB_statements.RData")
# # tidyr::pivot_longer(st_all[[2]], names_to = "bu", )
# statements2excel(st_all, file = "statement1.xlsx")
# statements2excel(st_parent, file = "statement.xlsx")

load(file = "./data/BRKB_statements.RData")
source("./scripts/functions/brkb_analysis_functions.R")
run_brkb_is_analysis(st_all_10Q, st_all_10K)

load(file = "./data/BRKB_income_bu.Rdata")

load(file = "./data/BRKB_statements.RData")
source("./scripts/functions/brkb_analysis_functions.R")
brkb_shr_buybacks_analysis(st_all)
brkb_shr_buybacks_analysis(st_parent)
plot(income$ProfitLoss - income$GainLossOnInvestments)

source("./scripts/functions/10Q_10K_functions.R")
source("./scripts/functions/brkb_analysis_functions.R")
#2009-09-30 insurance premiums 6,595
missing_fact <- find_specific_fact(url = 
  "https://www.sec.gov/Archives/edgar/data/1067983/000115752309007839/0001157523-09-007839-index.htm", 
                   fact = "6,?595", clean = TRUE)
is <- st_all_10Q[[2]]
is <- is[is$endDate == "2013-03-31",]
is <- is[is$startDate == "2013-01-01",]
# shows up at parent level, weird