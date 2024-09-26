# Ejemplo de la opcion de menu "Introducir datos y predecir"
# Cargar los datos "Chile" eligiendo del menu de Rcmdr:
# "Datos" -> "Conjuntos de datos en paquetes" -> "Leer conjunto de datos desde paquete adjunto..."
# pulsar dos veces sobre "car", pulsar sobre "Chile" y sobre "Aceptar".
# Rcmdr responde con la siguiente instruccion en el cuadro de instrucciones (R Script)
data(Chile, package="car")
# Para construir un modelo seleccione del menu de Rcmdr:
# "Estadisticos" -> "Ajuste de modelos" -> "Regresion lineal..."
# Como "Variable explicada" seleccione income y age como "Variables explicativas" y pulse "Aceptar"
# Rcmdr responde con la siguiente instruccion en el cuadro de instrucciones (R Script)
RegModel.1 <- lm(income~age, data=Chile)
summary(RegModel.1)
# Observese que el modelo se ha establecido a RegModel.1
# Si quiere predecir el ingreso (income) para personas de 35 y 40 anos de edad (age)
# Del menu de Rcmdr seleccione:
# "Modelos" -> "Predecir usando el modelo activo" -> "Introducir datos y predecir"
# En la columna edad (age) introduzca 35 y 40 y cierre el editor.
# Rcmdr responde con la siguiente instruccion en el cuadro de instrucciones (R Script)
.data <- edit(data.frame(age = numeric(0)))
.data
predict(RegModel.1, .data)
remove(.data)
# Se muestra el valor predicho de ingresos para esas edades usando el modelo activo (RegModel.1)
