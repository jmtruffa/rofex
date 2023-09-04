#' getRofexPosition
#'
#' Trae una o varias posiciones desde la API de Rofex con toda la información de Rofex
#' más el día que vence cada contrato según el calendario cargado en "~/data/test.sqlite3"
#'
#' @param position La/s posiciones a consultar. Si se especifica "curva", devuelve las posiciones 1 a 12 para la fecha dada
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

getRofexPosition = function(posicion = "curva", from = Sys.Date() - 1, to = Sys.Date(), page = 1, pageSize = 32000){
  require(rofex)
  require(tidyverse)
  require(httr2)
  require(jsonlite)
  require(bizdays)
  require(functions)
  require(lubridate)

  # creamos el calendario para calcular los días
  cal = create.calendar('tmpCalendar', holidays = getFeriados(), weekdays = c('saturday','sunday'))
  endpoint = "https://apicem.matbarofex.com.ar/api/v2/closing-prices"

  fail = tibble(
    symbol = character()
  )
  created = FALSE
  if (length(posicion) == 1) {
    if (tolower(posicion) == "curva") {
      position = tibble(date = Date(), position = as.character(), to = Date())
      for (i in bizseq(bizdays::adjust.previous(from, cal), to, cal)) {
        temp = .secuencia(seq.Date(from = as.Date(i), length.out = 12, by = "months"), cal)[[1]]
        position = rbind(position, tibble(date = rep(as.Date(i), length(temp)), position = temp, to = rep(as.Date(i), length(temp))))
      }
    } else {
      position = tibble(date = from, position = posicion, to = to)
    }
  } else {
    for (i in 1:length(posicion)) {
      position = tibble(date = rep(from, length(posicion)) , position = posicion, to = rep(to, length(posicion)))
    }
  }


  for (i in 1:nrow(position)) {
    error = FALSE
    tryCatch(
      {
        rPriceHistory = request(endpoint) %>%
          req_headers(`User-Agent` = "http://github.com/jmtruffa") %>%
          req_url_query(
            symbol = position[[i,2]],
            product = "DLR",
            segment = "MONEDAS",
            from = position[[i, 1]],
            to = position[[i, 3]],
            page = page,
            pageSize = pageSize,
            version = 2
          ) %>%
          req_method("GET") %>%
          req_perform } ,
      error = function(e) { error <<- TRUE; fail <<- fail %>% add_row(symbol = symbol[i]) }
    )
    if (!error) {

      data = fromJSON(rawToChar(rPriceHistory$body))$data

      if (length(data) != 0) {
        if (created) {
          history = tibble::add_row(history, (fromJSON(rawToChar(rPriceHistory$body))$data))
        } else {
          history = as_tibble(fromJSON(rawToChar(rPriceHistory$body))$data)
          created = TRUE
        }
      }
      # } else {
      #   history = NULL
      #   print(length(data))
      #   print(history)
      # }
    }
  }
  if (is.data.frame(history)) {
    history$EOM = getRofexEOM(history$symbol)
    history$impliedRate = history$impliedRate / 100
    history$dateTime = as.Date(history$dateTime)
    history$daysToMaturity = as.integer(history$EOM - history$dateTime)
    history$directa = (history$impliedRate * history$daysToMaturity / 365)
    history$impliedRateTEA = ( 1 + history$directa ) ^ ( 365 / history$daysToMaturity) - 1
    history = history %>%
      select(-c(unitsOpenInterest, unitsOpenInterestChange, unitsVolume, optionType, strikePrice, underlying)) %>%
      relocate(impliedRateTEA, .after = impliedRate) %>%
      rename(date = dateTime, impliedRateTNA = impliedRate) %>%
      relocate(date)
    mesVto = as.numeric(substr(history$symbol, 4, 5))
    anioVto = as.numeric(substr(history$symbol, 6,9))
    history$pos = (mesVto - month(history$date) + 1) + (anioVto - year(history$date)) * 12
  } else {
    history = NULL
  }
  return(list(history, fail))
}

#'
#'getRofexCurrentCurve
#'
#'Devuelve la curva desde la posición actual. Mes actual más 11 posiciones
#'
getRofexCurrentCurve = function() {

}

#'
#'getRofexCurCurveNames
getRofexCurCurveNames = function() {

}

#' .secuencia
#'
#' Función interna del paquete Rofex que genera la secuencia de posiciones a consultar a la API de Rofex.
#' @param serie Serie de fechas, separadas por 1 mes para generar la secuencia. e.g.: "1970-01-25" "1970-02-25" "1970-03-25"
#' @returns description Un tibble con futuro y vto.
.secuencia = function (serie, cal) {
  require(lubridate)
  ret = NULL
  end = Date()
  #meses = c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SEP", "OCT", "NOV", "DIC")
  meses = sprintf("%02d", 1:12) #esto lo modifiqué para que de salida DLR012023 en lugar de DLRENE2023
  for (i in seq_along(serie)) {
    ret = append(ret, paste0("DLR",meses[lubridate::month(as.Date(serie[i]))], substr(lubridate::year(as.Date(serie[i])), 1, 4)))
    finMes = lubridate::ceiling_date(as.Date(serie[i]), unit = "month") - 1
    offset = ifelse(bizdays::is.bizday(finMes, cal), 0, -1)
    finMesAjustado = bizdays::offset(finMes, offset, cal)
    end = append(end, finMesAjustado)
  }
  ret = tibble(futuro = ret, vto = end)
  ret
}







