#' getRofexPosition
#'
#' Trae una o varias posiciones desde la API de Rofex con toda la información de Rofex
#' más el día que vence cada contrato según el calendario cargado en "~/data/test.sqlite3"
#'
#' @param position La/s posiciones a consultar.
#' @param from  Fecha desde
#' @param to Fecha hasta
#' @param page Numero de página a devolver según paginado
#' @param pageSize Cantidad de valores incluídos en cada páginado
#'
#' @example getRofexPosition(position = c("DLR092023", "DLR082023"), from = "2023-07-01")
#'
#' @return Devuelve una lista. El primer elemento es una tibble con los datos de las posiciones consultadas.
#' El segundo elemento es una tibble con las posiciones que fallaron.
#'
#'

getRofexPosition = function(position, from, to = Sys.Date(), page = 1, pageSize = 32000){
  require(rofex)
  require(tidyverse)
  require(httr2)
  require(jsonlite)

  # creamos el calendario para calcular los días
  cal = create.calendar('tmpCalendar', holidays = getFeriados(), weekdays = c('saturday','sunday'))

  endpoint = "https://apicem.matbarofex.com.ar/api/v2/closing-prices"

  fail = tibble(
    ticker = character()
  )
  created = FALSE
  for (i in 1:length(position)) {
    error = FALSE
    tryCatch(
      {
        rPriceHistory = request(endpoint) %>%
          req_headers(`User-Agent` = "http://github.com/jmtruffa") %>%
          req_url_query(
            symbol = position[i],
            product = "DLR",
            segment = "MONEDAS",
            from = from,
            to = to,
            page = page,
            pageSize = pageSize,
            version = 2
          ) %>%
          req_method("GET") %>%
          req_perform } ,
      error = function(e) { error <<- TRUE; fail <<- fail %>% add_row(ticker = ticker[i]) }
    )

    if (!error) {
      if (created) {
        history = tibble::add_row(history, (fromJSON(rawToChar(rPriceHistory$body))$data))
      } else {
        history = as_tibble(fromJSON(rawToChar(rPriceHistory$body))$data)
        created = TRUE
      }
    }
  }
  # agrega el conteo de días hasta el vencimiento
  history$EOM = getRofexEOM(history$symbol)
  return(list(history, fail))
}


