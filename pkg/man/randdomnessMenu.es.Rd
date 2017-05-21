\name{randomnessMenu.es}
\title{Test de aleatoriedad}
\alias{Test de aleatoriedad (es)}
\docType{package}
\encoding{UTF-8}
\description{
  Dentro del menú "Test no paramétricos" se proporcionan dos nuevas
  entradas para realizar el test de aleatoriedad. Esta opción usa la
  función \code{runs.test} del paquete \code{tseries}. Para más
  información véase \code{\link[tseries]{runs.test}}.

  La primera "Test de aleatoriedad para un factor de dos niveles..." sirve para
  contrastar la aleatoriedad de una variable de tipo factor con dos
  niveles. Esta opción usa la función \code{runs.test} del paquete
  \code{randtest}. Para más información véase \code{\link[randtests]{runs.test}}.

  La segunda entrada del menú
  "Test de aleatoriedad para una variable numérica..." sirve para
  contrastar la aleatoriedad de una variable numérica. Esta opción usa
  la función \code{runs.test} del paquete \code{randtest}. Para más
  información véase \code{\link[randtests]{runs.test}}.
}

\details{
  Ejemplo de uso del menú
  "Test de aleatoriedad para un factor con dos niveles...".

  Cargar los datos "AMSsurvey" eligiendo del menu de Rcmdr: "Datos" ->
  "Conjuntos de datos en paquetes" ->
  "Leer conjunto de datos desde paquete adjunto..." pulsar dos veces
  sobre "car", pulsar sobre "AMSsurvey" y sobre "Aceptar". Rcmdr responde con la siguiente instruccion en el cuadro de
  instrucciones (R Script)

  \code{data(AMSsurvey, package="car")}

  Para hacer el test de aleatoriedad a la variable "sex", seleccione
  del menú de Rcmdr: "Estadísticos" -> "Test no paramétricos" ->
  "Test de aleatoriedad para un factor con dos niveles..." seleccione
  "sex" y "OK". Rcmdr responde con la
  siguiente instruccion en el cuadro de instrucciones (R Script)

  \code{with(AMSsurvey, twolevelfactor.runs.test(sex))}

  Ejemplo de uso del menú "Test de aleatoriedad para una variable numérica...".

  Cargar los datos "sweetpotato" eligiendo del menu de Rcmdr: "Datos" ->
  "Conjuntos de datos en paquetes" ->
  "Leer conjunto de datos desde paquete adjunto..." pulsar dos veces
  sobre "randtests", pulsar sobre "sweetpotato" y sobre "Aceptar". Rcmdr responde con la siguiente instruccion en el cuadro de
  instrucciones (R Script)

  \code{data(sweetpotato, package="randtests")}

  \code{sweetpotato <- as.data.frame(sweetpotato)}

  Para hacer el test de aleatoriedad a la variable "yield", seleccione
  del menú de Rcmdr: "Estadísticos" -> "Test no paramétricos" ->
  "Test de aleatoriedad para un factor con dos niveles..." seleccione
  "sex" y "OK". Rcmdr responde con la
  siguiente instruccion en el cuadro de instrucciones (R Script)

  \code{with(sweetpotato, numeric.runs.test(yield))}
}

\author{
Manuel Munoz-Marquez <manuel.munoz@uca.es>
}

\keyword{package}

\seealso{
Para más información véase \code{\link[Rcmdr]{Rcmdr-package}}. 

}
