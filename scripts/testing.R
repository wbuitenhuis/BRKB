# from 2022 annual report
# nr_a_shares <- 591564
# nr_b_shares <- -3
# b_shares_equiv <- nr_b_shares + nr_a_shares * 1500



# report_date <- as.Date("2023-12-31")

value_BRK_investments <-function(start_dt, end_dt){
  library(xts)
  library(NasdaqDataLink)
  
  investor <- "BERKSHIRE HATHAWAY INC"
  symbol = "BRK.B"
  NasdaqDataLink.api_key("s8rJbJ-oz8hPyu6_PC7K")
  
  dates_13F <- seq(as.yearqtr(start_dt), as.yearqtr(end_dt), by = 1/4) |> as.Date(frac = 1)
  dates_13F <- dates_13F[dates_13F < as.Date(Sys.Date() - 7 * 5)]
  end_dates <- c(dates_13F[-1], end_dt)
  
  investments <- NULL
  investment_per_share <- NULL
  b_shares_equiv <- rep(NA, length(dates_13F))
  for (i in 1:length(dates_13F)){
    holdings <- NasdaqDataLink.datatable('SHARADAR/SF3', calendardate = dates_13F[i], 
                                         investorname = investor, paginate = TRUE)
    if (nrow(holdings) == 0){
      
      browser()
    }
    prices <- NasdaqDataLink.datatable('SHARADAR/SEP', date.gte=as.Date(dates_13F[i] + 1), 
                                       date.lte=end_dates[i], 
                                       ticker=holdings$ticker, paginate = TRUE)
    ind <- match(prices$ticker, holdings$ticker)
    prices$units <- holdings$units[ind]
    prices$value <- prices$closeunadj * prices$units
    i.investments <- tapply(prices$value, prices$date, sum)
    i.investments <- xts(x = i.investments, order.by = as.Date(names(i.investments)))
    investments <- rbind(i.investments, investments)
    data <- NasdaqDataLink.datatable('SHARADAR/SF1', calendardate = dates_13F[i], 
                                     ticker = symbol, paginate = TRUE)
    row <- which(data$dimension == "MRQ") #most recent reported quarter
    nr_b_shares <- data[row, "shareswa"]
    latest_b_shares_equiv <- data[row, "shareswadil"]
    i.cash <- data[row, "cashnequsd"]
    if (length(latest_b_shares_equiv) == 0){
      latest_b_shares_equiv <- b_shares_equiv[i - 1]
      i.cash <- NA
    }
    b_shares_equiv[i] <- latest_b_shares_equiv
    i.investments_per_share <- i.investments  / b_shares_equiv[i]
    investment_per_share <- rbind(i.investments_per_share, investment_per_share)
  }
  investments <- investments / 10^6
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
  data <- cbind(data, premium)
  return(data)
}

start_dt <- as.Date("2017-12-31")
end_dt <- as.Date(Sys.Date()-1)

debugonce(value_BRK_investments)
BRKB <- value_BRK_investments(start_dt, end_dt)
premium <- (BRKB$BRK.B - BRKB$BRK.A / 1500) / BRKB$BRK.B
plot(BRKB$BRK.B - BRKB$investment_per_share)
plot(BRKB$investment_per_share)
plot(premium)

data <- NasdaqDataLink.datatable('SHARADAR/SF1', calendardate = as.Date("2023-09-30"), 
                                 ticker = "BRK.B", paginate = TRUE)

# A - as reported
# M - Most recent reported
# Y - Year
# Q - quarter
# T - Trailing 12 months
