% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getRofexCCL.R
\name{getRofexCCL}
\alias{getRofexCCL}
\title{getRofexCCL}
\usage{
getRofexCCL(from = Sys.Date(), to = Sys.Date())
}
\arguments{
\item{from}{Desde cuando regresar el ccl}

\item{to}{hasta cuando inclusive}
}
\value{
Un df con dos columnas. date y ccl
}
\description{
Funcion que devuelve el CCL armado entre la base histórica que está en
la base de datos historicData y la API de Rofex.
Devuelve un df con dos columnas: date y ccl.
}
\details{
ccl surge de cclGGAL histórico o, si no hay dato, el promedio de CCL de Rofex.
 Si Rofex no tuviera aún el promedio de CCL, entonces arroja el último disponible CC3, CCL2 o CC1. En ese orden.
}
