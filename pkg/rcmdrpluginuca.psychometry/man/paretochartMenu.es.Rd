\name{paretochartMenu.es}
\title{Opción de menú para hacer gráficos de Pareto}
\alias{Gráfico de Pareto (es)}
\docType{package}
\encoding{UTF-8}
\description{
  Dentro del menú "Control de Calidad" -> "Gráfica de Pareto", se proporcionan una nueva entrada para realizar un gráfico de Pareto en el que se representa una variable de tipo factor por orden decreciente de frecuencias.
  Esta gráfica se suele utilizar para identificar las causas más frecuentes de un fallo.
  Esta opción usa la función \code{paretochart} del paquete \code{qicharts2}.
  Para más información véase \code{\link[qicharts2]{paretochart}}.
}

\details{
  Ejemplo de uso del menú "Gráfica de Pareto".

  Cargar los datos "hospital_infections" eligiendo del menu de Rcmdr:
  "Datos" -> "Conjuntos de datos en paquetes" ->
  "Leer conjunto de datos desde paquete adjunto..." pulsar dos veces
  sobre "quicharts2", pulsar sobre "hospital_infections" y sobre
  "Aceptar". 
  Rcmdr responde con la siguiente instrucción en el cuadro de instrucciones (R Script)

  \code{data(hospital_infections, package="qicharts2")}

  Para construir el diagrama de Pareto de la variable "infection", seleccione
  del menú de Rcmdr: "Control de Calidad" -> "Gráfica de Pareto" seleccione
  "infection" y "Aceptar".
  Rcmdr responde con la siguiente instrucción en el cuadro de instrucciones (R Script)

  \code{with(hospital_infections, paretochart(infection))}

  y se obtiene el gráfico de Pareto en una nueva ventana.
}

\author{
  Manuel Munoz-Marquez <manuel.munoz@uca.es>
}

\keyword{package}

\seealso{
  Para más información véase \code{\link[Rcmdr]{Rcmdr-package}}. 
}
