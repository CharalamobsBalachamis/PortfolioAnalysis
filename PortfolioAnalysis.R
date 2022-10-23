library(PerformanceAnalytics)
library(tidyverse)
library(quantmod)

portfolio_prices <- NULL
tickers <- c("CMRE", "AMD", "NMM", "TNP", "NEE")
#weights <- c(0.49, 0.26, 0.09, 0.01, 0.15)

#get and assign prices for symbols from 4-1-2021 to 6-6-2022
for(ticker in tickers) {
  portfolio_prices <- cbind(portfolio_prices,
                           getSymbols.yahoo(ticker, from= "2021-01-01", to= "2022-6-7", periodicity = "daily", auto.assign=F)[,4])
}

#get and combined portfolio returns 
portfolio_returns <- na.omit(ROC(portfolio_prices))
portfolio_returns_combined <- Return.portfolio(portfolio_Returns, weights = c(0.49, 0.26, 0.09, 0.01, 0.15))

#get and assign prices for S&P500  from 4-1-2021 to 6-6-2022
benchmark <- getSymbols.yahoo("^GSPC", from= "2021-01-01", to= "2022-6-7", periodicity = "daily", auto.assign=F)[,4]
benchmark_returns <- na.omit(ROC(benchmark))

#discriptive statistics of portfolio
portfolio_stats <- fortify.zoo(portfolio_prices)[,-1] %>%
  summary()
  
#calculation of jensens alpha , beta and the shaapre ratio
CAPM <- list(CAPM.beta(portfolio_returns_combined, benchmark_returns, .028/252), 
             CAPM.jensenAlpha(portfolio_returns_combined, benchmark_returns, .028/252),
            SharpeRatio(portfolio_returns_combined, .028/252)
)


