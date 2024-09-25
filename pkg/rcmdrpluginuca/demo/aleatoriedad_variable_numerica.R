# Ejemplo de test de aleatoriedad para variable numerica
# Cargar los datos "sweetpotato" eligiendo del menu de Rcmdr:
# "Datos" -> "Conjuntos de datos en paquetes" -> "Leer conjunto de datos desde paquete adjunto..."
# pulsar dos veces sobre "randtests", pulsar sobre "sweetpotato" y "Aceptar".
# Rcmdr responde con la siguiente instruccion en el cuadro de instrucciones (R Script)
data(sweetpotato, package="randtests")
sweetpotato <- as.data.frame(sweetpotato)
# Para hacer el test de aleatoriead para la variable "yield", elegir del menu de Rcmdr:
# "Estadisticos" -> "Test no parametricos" -> "Test de aleatoriedad para variable numerica..."
# Elegir "yield" y "Aceptar"
# Rcmdr responde con la siguiente instruccion en el cuadro de instrucciones (R Script)
with(sweetpotato, numeric.runs.test(yield))
#	Runs Test
# data:  yield
# statistic = -4.5751, runs = 17, n1 = 35, n2 = 35, n = 70, p-value = 0.000004759
# alternative hypothesis: nonrandomness
