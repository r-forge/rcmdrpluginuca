# Ejemplo de test e intervalo de confianza para sigma para una poblacion normal
# Cargar los datos "sweetpotato" eligiendo del menu de Rcmdr:
# "Datos" -> "Conjuntos de datos en paquetes" -> "Leer conjunto de datos desde paquete adjunto..."
# pulsar dos veces sobre "randtests", pulsar sobre "sweetpotato" y "Aceptar".
# Rcmdr responde con la siguiente instruccion en el cuadro de instrucciones (R Script)
data(sweetpotato, package="randtests")
sweetpotato <- as.data.frame(sweetpotato)
# Para hacer el test de aleatoriead para la variable "yield", elegir del menu de Rcmdr:
# "Estadisticos" -> "Test no parametricos" -> "Test Chi-cuadrado para una muestra..."
# Elegir "yield" y "Aceptar"
# Rcmdr responde con la siguiente instruccion en el cuadro de instrucciones (R Script)
with(sweetpotato, sigma.test(yield[!is.na(yield)], alternative='two.sided', sigmasq=1.0, conf.level=0.95))
#	One sample Chi-squared test for variance
# data:  yield[!is.na(yield)]
# X-squared = 6514.8, df = 69, p-value < 2.2e-16
# alternative hypothesis: true variance is not equal to 1
# 95 percent confidence interval:
#   69.41231 135.93966
# sample estimates:
# var of yield[!is.na(yield)] 
#                    94.41731 
