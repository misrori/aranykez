#' get all tickers from coingecko
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET content
#' @importFrom data.table setDT
#' @export
get_all_crypto_from_coingecko <- function() {

  headers = c(
    `authority` = 'web-api.coinmarketcap.com',
    `accept` = 'application/json, text/plain, */*',
    `user-agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/84.0.4147.89 Safari/537.36',
    `origin` = 'https://coinmarketcap.com',
    `sec-fetch-site` = 'same-site',
    `sec-fetch-mode` = 'cors',
    `sec-fetch-dest` = 'empty',
    `referer` = 'https://coinmarketcap.com/',
    `accept-language` = 'hu-HU,hu;q=0.9,en-US;q=0.8,en;q=0.7'
  )

  params = list(
    `convert` = 'USD',
    `cryptocurrency_type` = 'all',
    `limit` = '5000',
    `sort` = 'market_cap',
    `sort_dir` = 'desc',
    `start` = '1'
  )

  res <- httr::GET(url = 'https://web-api.coinmarketcap.com/v1/cryptocurrency/listings/latest', httr::add_headers(.headers=headers), query = params)

  adat <- fromJSON(content(res, 'text'))
  adat <- adat$data
  return(data.table(adat))
}

#' All crypto data
#' @export
data_all_crypto <-get_all_crypto_from_coingecko()
