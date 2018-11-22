% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funciones.R
\name{guardar_variables_modelo}
\alias{guardar_variables_modelo}
\title{One hot encoding para variables categoricas}
\usage{
guardar_variables_modelo(base, variable_dependiente, directorio = "",
  sufijo = "prueba1")
}
\arguments{
\item{base}{data.frame a transformar sus variables}

\item{variable_dependiente}{vector de nombres de las variables categoricas de la base}

\item{directorio}{directorio donde se guardarian los nombres, defaults to current directory}

\item{sufijo}{sufijo a anexar al nombre del archivo de nombres de variables, defaults to ""}

\item{guardar}{si se guardaran los nombres de las variables oneHot, defaults to FALSE}
}
\description{
Transforma variables categoricas de una base a dummys y la retorna. Tiene la opcion de guardar los nombres de variables oneHot, necesarios para evaluar una nueva base
}
\keyword{categorical}
\keyword{transform}