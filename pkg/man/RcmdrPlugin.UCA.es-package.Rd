\name{RcmdrPlugin.UCA.es}
\title{RcmdrPlugin.UCA.es: Algunas adaptaciones para Rcmdr hechas por el
  proyecto R-UCA}
\alias{RcmdrPlugin.UCA.es}
\docType{package}
\encoding{UTF-8}

\description{
  Algunas extensiones a Rcmdr (R Commander), test de aleatoriedad y
  predicciones utilizando el modelo activo, para la \enc{enseñanza}{ensenanza} de
  un primer curso universitario en \enc{estadística}{estadistica} hechas por el
  proyecto R-UCA y usadas en la Universidad de \enc{Cádiz}{Cadiz} (UCA).

  Todas las adaptaciones \enc{serán}{seran} propuestas para su \enc{incorporación}{incorporacion} a
  Rcmdr. Las adaptaciones que se incorporen al paquete Rcmdr se \enc{suprimirán}{suprimiran}
  de este paquete.
}
\details{
\tabular{ll}{
Paquete: \tab RcmdrPlugin.UCA\cr
Tipo: \tab Paquete\cr
Version: \tab 3.1-2\cr
Fecha: \tab 2016-09-20\cr
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
# Ejemplo de test de aleatoriedad para un factor con dos niveles
# Cargar los datos "AMSsurvey" eligiendo del menu de Rcmdr:
# "Datos" -> "Conjuntos de datos en paquetes" -> "Leer conjunto de datos desde paquete adjunto..."
# pulsar dos veces sobre "car", pulsar sobre "AMSsurvey" y sobre "Aceptar".
# Rcmdr responde con la siguiente instruccion en el cuadro de instrucciones (R Script)
### data(AMSsurvey, package="car")
# Para hacer el test de aleatoriead para la variable "sex", elegir del menu de Rcmdr:
# "Estadisticos"-> "Test no parametricos"-> "Test de aleatoriedad para un factor de dos niveles..."
# Elegir "sex" y "Aceptar"
# Rcmdr responde con la siguiente instruccion en el cuadro de instrucciones (R Script)
### with(AMSsurvey, twolevelfactor.runs.test(sex))
#
#
# Ejemplo de test de aleatoriedad para variable numerica
# Cargar los datos "sweetpotato" eligiendo del menu de Rcmdr:
# "Datos" -> "Conjuntos de datos en paquetes" -> "Leer conjunto de datos desde paquete adjunto..."
# pulsar dos veces sobre "randtests", pulsar sobre "sweetpotato" y "Aceptar".
# Rcmdr responde con la siguiente instruccion en el cuadro de instrucciones (R Script)
### data(sweetpotato, package="randtests")
### sweetpotato <- as.data.frame(sweetpotato)
# Para hacer el test de aleatoriead para la variable "yield", elegir del menu de Rcmdr:
# "Estadisticos" -> "Test no parametricos" -> "Test de aleatoriedad para variable numerica..."
# Elegir "yield" y "Aceptar"
# Rcmdr responde con la siguiente instruccion en el cuadro de instrucciones (R Script)
### with(sweetpotato, numeric.runs.test(yield))
#
#
# Ejemplo de la opcion de menu "Introducir datos y predecir"
# Cargar los datos "Chile" eligiendo del menu de Rcmdr:
# "Datos" -> "Conjuntos de datos en paquetes" -> "Leer conjunto de datos desde paquete adjunto..."
# pulsar dos veces sobre "car", pulsar sobre "Chile" y sobre "Aceptar".
# Rcmdr responde con la siguiente instruccion en el cuadro de instrucciones (R Script)
### data(Chile, package="car")
# Para construir un modelo seleccione del menu de Rcmdr:
# "Estadisticos" -> "Ajuste de modelos" -> "Regresion lineal..."
# Como "Variable explicada" seleccione income y age como "Variables explicativas" y pulse "Aceptar"
# Rcmdr responde con la siguiente instruccion en el cuadro de instrucciones (R Script)
### RegModel.1 <- lm(income~age, data=Chile)
### summary(RegModel.1)
# Observese que el modelo se ha establecido a RegModel.1
# Si quiere predecir el ingreso (income) para personas de 35 y 40 anos de edad (age)
# Del menu de Rcmdr seleccione:
# "Modelos" -> "Predecir usando el modelo activo" -> "Introducir datos y predecir"
# En la columna edad (age) introduzca 35 y 40 y cierre el editor.
# Rcmdr responde con la siguiente instruccion en el cuadro de instrucciones (R Script)
### .data <- edit(Chile[0,])
### .data
### predict(RegModel.1, .data)
### remove(.data)
# Se muestra el valor predicho de ingresos para esas edades usando el modelo activo (RegModel.1)
#
#
# Ejemplo de "Anadir predicciones a un conjunto de datos existente..."
# Cargar los datos "Chile" eligiendo del menu de Rcmdr:
# "Datos" -> "Conjuntos de datos en paquetes" -> "Leer conjunto de datos desde paquete adjunto..."
# pulsar dos veces sobre "car", pulsar sobre "Chile" y "Aceptar".
# Rcmdr responde con la siguiente instruccion en el cuadro de instrucciones (R Script)
### data(Chile, package="car")
# Para construir un modelo seleccione del menu de Rcmdr:
# "Estadisticos" -> "Ajuste de modelos" -> "Regresion lineal..."
# Como "Variable explicada" seleccione income y age como "Variables explicativas" y pulse "Aceptar"
# Rcmdr responde con la siguiente instruccion en el cuadro de instrucciones (R Script)
### RegModel.1 <- lm(income~age, data=Chile)
### summary(RegModel.1)
# Observese que el modelo se ha establecido a RegModel.1
# Si se quiere predecir los valores para los ingresos (income)
# para los valores de edad (age) en el conjunto de datos Chile
# Del menu "Modelos" de Rcmdr seleccione:
# "Predicir usando el modelo activo" -> "Anadir predicciones a un conjunto de datos existente..."
# En el dialogo seleccione un conjunto de datos compatible con el modelo
# En este caso seleccione Chile
# Rcmdr responde con la siguiente instruccion en el cuadro de instrucciones (R Script)
### Chile$fitted <- predict(RegModel.1, Chile)
# Las predicciones para ingreso (income) se han guardado como fitted en el conjunto de datos (Chile)
}

\references{
[1] \url{http://knuth.uca.es/RcmdrPlugin.UCA}
}

