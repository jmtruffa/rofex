#'
#' getRofexCCL
#'
#' Funcion que devuelve el CCL armado entre la base histórica que está en
#' la base de datos historicData y la API de Rofex.
#' Devuelve un df con dos columnas: date y ccl.
#'
#'  ccl surge de cclGGAL histórico o, si no hay dato, el promedio de CCL de Rofex.
#'  Si Rofex no tuviera aún el promedio de CCL, entonces arroja el último disponible CC3, CCL2 o CC1. En ese orden.
#'
#' @param from Desde cuando regresar el ccl
#' @param to hasta cuando inclusive
#'
#' @return Un df con dos columnas. date y ccl
#'
#' @example getRofexCCL(from = "2017-01-01", to = "2023-10-01")
#'
getRofexCCL = function(from = Sys.Date(), to = Sys.Date()) {
  require(httr2)
  require(jsonlite)
  require(tidyverse)
  require(functions)

  url = "https://apicem.matbarofex.com.ar/api/v2/spot-prices"

  #primero chequeamos que esté bien construido el rango.
  if (to >= from) {

      result = request(url) %>%
        req_headers(`User-Agent` = "http://github.com/jmtruffa") %>%
        req_url_query(spot = "",
                      from = from,
                      to = to,
                      page = 1,
                      pageSize = 32000) %>%
        req_method("GET") %>%
        req_perform


    resultDF = as_tibble(fromJSON(rawToChar(result$body))$data)
    # filtrar  los que comiencen con CCL
    # cambiamos nombre de columan date
    # eliminamos columnas innecesarias
    # lo armamos wide
    # con coalesce seleccionamos la primera que es non-missing
    resultDF = resultDF %>%
      filter(str_detect(spot,"CCL")) %>%
      rename(date = dateTime) %>%
      select(-representativity, -resolution, -price) %>%
      pivot_wider(names_from = spot, values_from = normalizedPrice) %>%
      mutate(ccl =coalesce(CCL3, CCL, CCL2, CCL1))
    resultDF$ccl = as.double(resultDF$ccl)
    resultDF$date = as.Date(resultDF$date)

  }

  cclHistorico = getTable("dolar")[c(1,16)] %>% filter(date >= from, date <= to) %>% drop_na()
  cclFull = full_join(cclHistorico, resultDF[c(1, 6)]) %>% arrange(date)
  cclFull %<>%
    mutate(ccl = ifelse(is.na(ccl), cclGGAL, ccl)) %>%
    select(-cclGGAL)

  cclFull

}
