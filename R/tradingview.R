#' Download data from the tradingView site with the passed json string
#' @export
#' @param my_json_string json string from inspect view at the bottom
#' @importFrom data.table data.table rbindlist
#' @importFrom jsonlite fromJSON
#' @importFrom httr content POST
utility_tradingview_data_from_json_string <- function(json_string) {
  headers = c(
    `authority` = 'scanner.tradingview.com',
    `accept` = 'text/plain, */*; q=0.01',
    `origin` = 'https://www.tradingview.com',
    `user-agent` = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.108 Safari/537.36',
    `content-type` = 'application/x-www-form-urlencoded; charset=UTF-8',
    `sec-fetch-site` = 'same-site',
    `sec-fetch-mode` = 'cors',
    `referer` = 'https://www.tradingview.com/',
    `accept-encoding` = 'gzip, deflate, br',
    `accept-language` = 'hu-HU,hu;q=0.9,en-US;q=0.8,en;q=0.7'
  )

  res <- httr::POST(url = 'https://scanner.tradingview.com/america/scan', httr::add_headers(.headers=headers), body = json_string)

  t <- fromJSON(content(res, 'text'))
  df_data <-
    rbindlist(lapply(t$data$d, function(x){
      data.frame(t(data.frame(x)), stringsAsFactors = F)
    }))

  names(df_data) <-  fromJSON(json_string)$columns
  final_data <- cbind( data.table('exchange' = sapply(strsplit(t$data$s, ':'), '[[', 1)),  df_data)
  return(final_data)
}

#' Return all the ticker from tradingview with performance data
#' @export
utility_all_stock_from_tradingvies <- function() {
  adat <- utility_tradingview_data_from_json_string('{"filter":[{"left":"market_cap_basic","operation":"nempty"},{"left":"type","operation":"in_range","right":["stock","dr","fund"]},{"left":"subtype","operation":"in_range","right":["common","","etf","unit","mutual","money","reit","trust"]},{"left":"exchange","operation":"in_range","right":["AMEX","NASDAQ","NYSE"]}],"options":{"lang":"en"},"symbols":{"query":{"types":[]},"tickers":[]},"columns":["logoid","name","close","change","change_abs","Recommend.All","volume","market_cap_basic","price_earnings_ttm","earnings_per_share_basic_ttm","number_of_employees","industry","sector","SMA50","SMA100","SMA200","RSI","Perf.Y","Perf.3M","Perf.6M","Perf.1M","Perf.W","High.3M","High.6M","price_52_week_high","description","name","type","subtype","update_mode","pricescale","minmov","fractional","minmove2","SMA50","close","SMA100","SMA200","RSI","RSI[1]"],"sort":{"sortBy":"market_cap_basic","sortOrder":"desc"},"range":[0,8000]}')
  return(adat)
}

#' Changing character column to numeric if it is numeric
#' @export
#' @param df dataframe to check the columns
utility_change_col_to_num <- function(df) {
  df <- df[,!duplicated(names(df)), with=F]
  nevek <-names(df)
  for (i in nevek) {
    if(suppressWarnings(sum(is.na(as.numeric(  df[[i]][!is.na(df[[i]])] )  ))==0)){
      df[[i]] <- as.numeric(df[[i]])
    }
  }
  return(df)
}

#' All the stock data
#' @export
data_all_stock <- utility_change_col_to_num(utility_all_stock_from_tradingvies())

#' Get one stock info
#' @export
#' @param x ticker name
get_one_stock_data_from_tradingview <-function(x){
  adat <- utility_tradingview_data_from_json_string(sprintf('{"filter":[{"left":"market_cap_basic","operation":"nempty"},{"left":"type","operation":"in_range","right":["stock","dr","fund"]},{"left":"subtype","operation":"in_range","right":["common","","etf","unit","mutual","money","reit","trust"]},{"left":"exchange","operation":"in_range","right":["AMEX","NASDAQ","NYSE"]}],"options":{"lang":"en"},"symbols":{"query":{"types":[]},"tickers":["%s"]},"columns":["logoid","name","close","change","change_abs","Recommend.All","volume","market_cap_basic","price_earnings_ttm","earnings_per_share_basic_ttm","number_of_employees","industry","sector","SMA50","SMA100","SMA200","RSI","Perf.Y","Perf.3M","Perf.6M","Perf.1M","Perf.W","High.3M","High.6M","price_52_week_high","description","name","type","subtype","update_mode","pricescale","minmov","fractional","minmove2","SMA50","close","SMA100","SMA200","RSI","RSI[1]"],"sort":{"sortBy":"market_cap_basic","sortOrder":"desc"},"range":[0,6000]}',paste0(data_all_stock[name==x]$exchange, ':', data_all_stock[name==x]$name ) ))
  return(adat)
}



