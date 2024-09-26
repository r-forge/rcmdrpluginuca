\name{predictMenu.es}
\title{Opción de menú para hacer predicciones usando el modelo activo}
\alias{Menú predicciones (es)}
\docType{package}
\encoding{UTF-8}
\description{
  La entrada de menú "Predecir usando el modelo activo", en el menú
  modelos, tiene dos opciones para predecir datos usando el modelo
  activo dependiendo de cómo se proporcionen los valores para las variables predictoras.
  
  La primera entrada es "Introducir datos y predecir".
  Si selecciona esta opción, se creará un nuevo conjunto de datos, como
  un data.frame y se abrirá el editor automáticamente.
  A continuación, puede introducir los valores de la variable predictora
  que desea utilizar para la predicción, no es necesario introducir los
  valores de las variables no predictoras.
  Al cerrar el editor de datos se muestran los valores predichos.

  La segunda entrada de menú
  "Agregar predicciones a un conjunto de datos existente ..." permite
  agregar predicciones a un conjunto de datos existente que proporcione
  los datos valores para todas las variables predictoras.
  Después de seleccionar esta opción, el usuario o usuaria puede
  seleccionar un conjunto de datos existente utilizando el cuadro de
  diálogo correspondiente.

  Si el conjunto de datos no proporciona los valores para todas las
  variables predictoras, se producirá un error y no se proporcionarán
  valores predichos.

  A diferencia de la opción del menú
  "Añadir las estadísticas de las observaciones a los datos...", esta
  opción puede ser usada con un conjunto de datos distinto al que se ha
  utilizado para construir el modelo, siempre que dicho conjunto de
  datos proporcione los valores para todas las variables predictoras.
}

\details{
  Ejemplo de uso del menú "Introducir datos y predecir".

  Cargar los datos "Chile" eligiendo del menú de Rcmdr: "Datos" ->
  "Conjuntos de datos en paquetes" ->
  "Leer conjunto de datos desde paquete adjunto..." pulsar dos veces
  sobre "car", pulsar sobre "Chile" y sobre "Aceptar". Rcmdr responde con la siguiente instrucción en el cuadro de
  instrucciones (R Script)

  \code{data(Chile, package="car")}

  Para construir un modelo seleccione del menú de Rcmdr: "Estadísticos"
  -> "Ajuste de modelos" -> "Regresion lineal...". Como
  "Variable explicada" seleccione income y age como
  "Variables explicativas" y pulse "Aceptar". Rcmdr responde con la
  siguiente instrucción en el cuadro de instrucciones (R Script)

  \code{RegModel.1 <- lm(income~age, data=Chile)}

  \code{summary(RegModel.1)}

  Obsérvese que el modelo se ha establecido a RegModel.1. Si quiere
  predecir el ingreso (income) para personas de 35 y 40 años de edad
  (age). Del menú de Rcmdr seleccione: "Modelos" ->
  "Predecir usando el modelo activo" ->
  "Introducir datos y predecir". En la columna edad (age) introduzca 35
  y 40 y cierre el editor. Rcmdr responde con la siguiente instrucción
  en el cuadro de instrucciones (R Script)

  \code{.data <- edit(Chile[0,])}

  \code{.data}

  \code{predict(RegModel.1, .data)}

  \code{remove(.data)}

  Se muestra el valor predicho de ingresos para esas edades usando el modelo activo (RegModel.1)
  
  Ejemplo de uso del menú "Anadir predicciones a un conjunto de datos existente..."

  Cargar los datos "Chile" eligiendo del menú de Rcmdr: "Datos" ->
  "Conjuntos de datos en paquetes" ->
  "Leer conjunto de datos desde paquete adjunto..." pulsar dos veces
  sobre "car", pulsar sobre "Chile" y "Aceptar". Rcmdr responde con la siguiente instrucción en el cuadro de
  instrucciones (R Script)
  
  \code{data(Chile, package="car")}

  Para construir un modelo seleccione del menú de Rcmdr: "Estadísticos"
  -> "Ajuste de modelos" -> "Regresión lineal..." como
  "Variable explicada" seleccione income y age como
  "Variables explicativas" y pulse "Aceptar".

  Rcmdr responde con la siguiente instruccion en el cuadro de
  instrucciones (R Script)

  \code{RegModel.1 <- lm(income~age, data=Chile)}
  \code{summary(RegModel.1)}

  Obsérvese que el modelo se ha establecido a RegModel.1. Si se quiere
  predecir los valores para los ingresos (income) para los valores de
  edad (age) en el conjunto de datos Chile del menu "Modelos" de Rcmdr
  seleccione: "Predicir usando el modelo activo" ->
  "Anadir predicciones a un conjunto de datos existente...". En el
  diálogo seleccione un conjunto de datos compatible con el modelo. En
  este caso seleccione Chile. Rcmdr responde con la siguiente
  instrucción en el cuadro de instrucciones (R Script)

  \code{Chile$fitted <- predict(RegModel.1, Chile)}

  Las predicciones para ingreso (income) se han guardado como fitted en el conjunto de datos (Chile)
  }

\author{
Manuel Munoz-Marquez <manuel.munoz@uca.es>
}

\keyword{package}

\seealso{
Para más información véase \code{\link[Rcmdr]{Rcmdr-package}}. 
}
