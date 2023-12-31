% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getRofexEOM.R
\name{getRofexEOM}
\alias{getRofexEOM}
\title{getRofexEOM}
\usage{
getRofexEOM(position, cal = "~/data/test.sqlite3")
}
\arguments{
\item{position}{Posición de Rofex según formato DLRMMYYYY}

\item{cal}{Calendario a utilizar. Por default utiliza el guardado en "~/data/test.sqlite3"}
}
\value{
Un valor o vector de valores dependiendo de lo que se le haya pasado como
argumento
}
\description{
Funcion que se le pasa el futuro y devuelve el último día de ese mes
La utiliza getRofexPosition pero se puede llamar directamente.
Busca a partir de la 4 posicion con el formato XX -> mes, XXXX -> año
}
