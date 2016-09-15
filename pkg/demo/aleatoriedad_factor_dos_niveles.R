# Ejemplo de test de aleatoriedad para un factor con dos niveles
# Cargar los datos "AMSsurvey" eligiendo del menu de Rcmdr:
# "Datos" -> "Conjuntos de datos en paquetes" -> "Leer conjunto de datos desde paquete adjunto..."
# pulsar dos veces sobre "car", pulsar sobre "AMSsurvey" y sobre "Aceptar".
# Rcmdr responde con la siguiente instruccion en el cuadro de instrucciones (R Script)
data(AMSsurvey, package="car")
# Para hacer el test de aleatoriead para la variable "sex", elegir del menu de Rcmdr:
# "Estadisticos"-> "Test no parametricos"-> "Test de aleatoriedad para un factor de dos niveles..."
# Elegir "sex" y "Aceptar"
# Rcmdr responde con la siguiente instruccion en el cuadro de instrucciones (R Script)
with(AMSsurvey, twolevelfactor.runs.test(sex))
