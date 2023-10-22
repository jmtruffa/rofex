#'
#' getRofexCCL
#'
#' Funcion que devuelve el CCL armado entre la base histórica que está en
#' la base de datos historicData y la API de Rofex.
#' Devuelve un df con dos columnas: cclProm y ccl.
#'
#'
#' @param from Desde cuando regresar el ccl
#' @param to hasta cuando inclusive
#'
#' @return Un df con dos columnas. cclProm y ccl
#'
#' @example getRofexCCL(from = "2017-01-01", to = "2023-10-01")
#'
getRofexCCL = function(from = Sys.Date(), to = Sys.Date()) {
  require(httr2)
  require(jsonlite)
  require(tidyverse)
  require(functions)

  #primero chequeamos que esté bien construido el rango.
  if (to >= from) {
    result = request("https://apicem.matbarofex.com.ar/api/v2/spot-prices") %>%
      req_headers(`User-Agent` = "http://github.com/jmtruffa") %>%
      req_url_query(spot = "CCL",
                    from = from,
                    to = to,
                    page = 1,
                    pageSize = 32000) %>%
      req_method("GET") %>%
      req_perform

    ccl = as_tibble(fromJSON(rawToChar(result$body))$data)
    ccl$dateTime = as.Date(ccl$dateTime)
    ccl$normalizedPrice = as.double(ccl$normalizedPrice)
    ccl = ccl[c(1,6)] %>% rename(date = dateTime, cclProm = normalizedPrice)

    result = request("https://apicem.matbarofex.com.ar/api/v2/spot-prices") %>%
      req_headers(`User-Agent` = "http://github.com/jmtruffa") %>%
      req_url_query(spot = "CCL3",
                    from = from,
                    to = to,
                    page = 1,
                    pageSize = 32000) %>%
      req_method("GET") %>%
      req_perform

    ccl3 = as_tibble(fromJSON(rawToChar(result$body))$data)
    ccl3$dateTime = as.Date(ccl3$dateTime)
    ccl3$normalizedPrice = as.double(ccl3$normalizedPrice)
    ccl3 = ccl3[c(1,6)] %>% rename(date = dateTime, ccl = normalizedPrice)
  }

  cclHistorico = getTable("dolar")[c(1,16)] %>% filter(date >= from, date <= to) %>% drop_na()
  cclFull = full_join(cclHistorico, ccl) %>% full_join(ccl3) %>% arrange(date)
  cclFull %<>%
    mutate(ccl = ifelse(is.na(ccl), cclGGAL, ccl)) %>%
    select(-cclGGAL)

  cclFull

}
