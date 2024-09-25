\name{ksMenu.es}
\title{Test de Kolmogorov-Smirnov}
\alias{Test de Kolmogorov-Smirnov (es)}
\docType{package}
\encoding{UTF-8}
\description{
  Dentro del menú "Estadísticos" -> "Test no paramétricos", se
  proporcionan una nueva entrada
  "Test de Kolmogorov-Smirnov para dos muestras..." para realizar el
  test de Kolmogorov-Smirnov para dos muestras, cuya hipótesis nula es
  que ambas muestras han sido extraídas de la misma distribución
  contínua. Esta opción usa la función \code{ks.test} del paquete
  \code{stats}. Para más información véase \code{\link[stats]{ks.test}}.
}

\details{
  Ejemplo de uso del menú "Test de Kolmogorov-Smirnov para dos muestras...".

  Cargar los datos "Baumann" eligiendo del menu de Rcmdr: "Datos" ->
  "Conjuntos de datos en paquetes" ->
  "Leer conjunto de datos desde paquete adjunto..." pulsar dos veces
  sobre "carData", pulsar sobre "Baumann" y sobre "Aceptar". Rcmdr
  responde con la siguiente instrucción en el cuadro de instrucciones (R
  Script)
  
  \code{data(Baumann, package="carData")}

  Para comparar las distribuiones de "pretest.1" y "post.test.1",
  seleccione del menú de Rcmdr: "Estadísticos" -> "Test no paramétricos"
  -> "Test de Kolmogorov-Smirnov para dos muestras..." seleccione
  "pretest.1" y "post.test.1", luego pulse "Aceptar". Rcmdr responde con
  la siguiente \enc{instrucción}{instruccion} en el cuadro de
  instrucciones (R Script)

  \code{with(Baumann, ks.test(pretest.1, post.test.1))}
}

\author{
  Manuel Munoz-Marquez <manuel.munoz@uca.es>
}

\keyword{package}

\seealso{
  Para \enc{más}{mas} \enc{información}{informacion} \enc{véase}{vease} \code{\link[Rcmdr]{Rcmdr-package}}. 
}
