#'
#' getRofexEOM
#'
#' Funcion que se le pasa el futuro y devuelve el último día de ese mes
#' La utiliza getRofexPosition pero se puede llamar directamente.
#' Busca a partir de la 4 posicion con el formato XX -> mes, XXXX -> año
#'
#' @param position Posición de Rofex según formato DLRMMYYYY
#' @param cal Calendario a utilizar. Por default utiliza el guardado en "~/data/test.sqlite3"
#'
#' @return Un valor o vector de valores dependiendo de lo que se le haya pasado como
#' argumento
#'
#' @example getRofexEOM(position = c("DLR122020", "DLR012017"))

getRofexEOM = function(position, cal = "~/data/test.sqlite3") {
  require(functions)
  require(bizdays)
  cal = create.calendar('Argentina/test', getFeriados('~/data/test.sqlite3'), weekdays=c("saturday", "sunday"))

  ret = NULL
  for (i in 1:length(position)) {
    month = substr(position[i], 4, 5)
    year = substr(position[i], 6,9)
    ret = append(ret,
                 bizdays::adjust.previous(lubridate::ceiling_date(as.Date(ISOdate(year, month, 1)), "month") - 1, cal)
    )
  }
  ret
}






