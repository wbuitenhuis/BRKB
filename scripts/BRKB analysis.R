# from 2022 annual report
# nr_a_shares <- 591564
# nr_b_shares <- -3
# b_shares_equiv <- nr_b_shares + nr_a_shares * 1500
#


# report_date <- as.Date("2023-12-31")


start_dt <- as.Date("2023-03-31")
end_dt <- as.Date(Sys.Date()-1)

debugonce(value_BRK_investments)
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

# source("./scripts/functions/10Q_10K_functions.R")
# find_specific_fact(url = "https://www.sec.gov/Archives/edgar/data/1067983/000119312518057033/0001193125-18-057033-index.htm", pattern = "outstanding", field = "label")
# 
# f <- find_specific_fact(url = "https://www.sec.gov/Archives/edgar/data/1067983/000119312518057033/0001193125-18-057033-index.htm", pattern = "1,?644,?615", field = "fact")
# 

source("./scripts/functions/brkb_analysis_functions.R")
# debugonce(brkb_statements)
brkb_statements(form = "10-K", years = 15, arc = "presentation")
source("./scripts/functions/brkb_analysis_functions.R")
brkb_statements(form = "10-Q", years = 17, arc = "presentation")

load(file = "./data/BRKB_statements.RData")
source("./scripts/functions/brkb_analysis_functions.R")
run_brkb_analysis(st_all_10Q, st_all_10K, shares_outstanding)

load(file = "./data/BRKB_income_bu.Rdata")
source("./scripts/functions/brkb_analysis_functions.R")
add_portfolio_valuation(data3M)

