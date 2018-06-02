\name{simgaMenu.es}
\title{Test de varianza}
\alias{Test de varianza (es)}
\docType{package}
\encoding{UTF-8}
\description{
  Dentro del menú "Estadísticos" -> Varianzas", se proporcionan una nueva entrada para
  calcular intervalos de confianza y hacer contrastes sobre la varianza
  en una población normal. Esta opción usa la
  función \code{sigma.test} del paquete \code{TeachingDemos}. Para más
  información véase \code{\link[TeachingDemos]{sigma.test}}.
}

\details{
  Ejemplo de uso del menú "Test de varianza para una muestra...".

  Cargar los datos "sweetpotato" eligiendo del menu de Rcmdr: "Datos" ->
  "Conjuntos de datos en paquetes" ->
  "Leer conjunto de datos desde paquete adjunto..." pulsar dos veces
  sobre "randtests", pulsar sobre "sweetpotato" y sobre "Aceptar". Rcmdr responde con la siguiente instruccion en el cuadro de
  instrucciones (R Script)

  \code{data(sweetpotato, package="randtests")}

  \code{sweetpotato <- as.data.frame(sweetpotato)}

  Para construir el intervalo de confianza para sigma para la variable "yield", seleccione
  del menú de Rcmdr: "Estadísticos" -> "Varianzas" ->
  "Test de varianza para una muestra..." seleccione
  "yield" y "OK". Rcmdr responde con la
  siguiente instruccion en el cuadro de instrucciones (R Script)

  \code{with(sweetpotato, sigma.test(yield[!is.na(yield)], alternative='two.sided', sigmasq=1.0, conf.level=0.95))}
}

\author{
Manuel Munoz-Marquez <manuel.munoz@uca.es>
}

\keyword{package}

\seealso{
Para más información véase \code{\link[Rcmdr]{Rcmdr-package}}. 
}
