\name{RcmdrPlugin.UCA.es}
\title{RcmdrPlugin.UCA.es: Algunas adaptaciones para Rcmdr hechas por el
  proyecto R-UCA}
\alias{RcmdrPlugin.UCA.es}
\docType{package}
\encoding{UTF-8}

\description{
  Algunas extensiones a Rcmdr (R Commander) hechas por el proyecto R-UCA
  y usadas en la \enc{enseñanza}{ensenanza} de la
  \enc{estadística}{estadistica} en la Universidad de \enc{Cádiz}{Cadiz} (UCA).

  Todas las adaptaciones \enc{serán}{seran} propuestas para su \enc{incorporación}{incorporacion} a
  Rcmdr. Las adaptaciones que se incorporen al paquete Rcmdr se \enc{suprimirán}{suprimiran}
  de este paquete.
}
\details{
\tabular{ll}{
Paquete: \tab RcmdrPlugin.UCA\cr
Tipo: \tab Paquete\cr
Version: \tab 2.0-5\cr
Fecha: \tab 2016-01-12\cr
Licencia: \tab GPL (>= 3)\cr
}
}

\author{
Manuel \enc{Muñoz-Márquez}{Munoz-Marquez} <manuel.munoz@uca.es>
}

\keyword{package}
\seealso{
Para \enc{más}{mas} \enc{información}{informacion} \enc{véase}{vease} \code{\link[Rcmdr]{Rcmdr-package}}.
}

\examples{
# Ejemplo de factor de dos niveles
# Cargar los datos "AMSsurvey" eligiendo del menu de Rcmdr:
# "Datos" -> "Conjuntos de datos en paquetes" -> "Leer conjunto de datos desde paquete adjunto..."
# pulsar dos veces sobre "car", pulsar sobre "AMSsurvey" y "Aceptar".
data(AMSsurvey)
# Para hacer el test de aleatoriead para la variable "sex", elegir del menu de Rcmdr:
# "Estadisticos"-> "Test no parametricos"-> "Test de aleatoriedad para un factor de dos niveles..."
# Elegir "sex" y "Aceptar"
with(AMSsurvey, tseries::runs.test(sex))
#
#
# Ejemplo de variable numerica
# Cargar los datos "sweetpotato" eligiendo del menu de Rcmdr:
# "Datos" -> "Conjuntos de datos en paquetes" -> "Leer conjunto de datos desde paquete adjunto..."
# pulsar dos veces sobre "randtests", pulsar sobre "sweetpotato" y "Aceptar".
data(sweetpotato, package="randtests")
# Para hacer el test de aleatoriead para la variable "yield", elegir del menu de Rcmdr:
# "Estadisticos" -> "Test no parametricos" -> "Test de aleatoriedad para variable numerica..."
# Elegir "yield" y "Aceptar"
with(sweetpotato, randtests::runs.test(yield))
}

\references{
[1] \url{http://knuth.uca.es/RcmdrPlugin.UCA}
}

