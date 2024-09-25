# Ejemplo de "Anadir predicciones a un conjunto de datos existente..."
# Cargar los datos "Chile" eligiendo del menu de Rcmdr:
# "Datos" -> "Conjuntos de datos en paquetes" -> "Leer conjunto de datos desde paquete adjunto..."
# pulsar dos veces sobre "car", pulsar sobre "Chile" y "Aceptar".
# Rcmdr responde con la siguiente instruccion en el cuadro de instrucciones (R Script)
data(Chile, package="car")
# Para construir un modelo seleccione del menu de Rcmdr:
# "Estadisticos" -> "Ajuste de modelos" -> "Regresion lineal..."
# Como "Variable explicada" seleccione income y age como "Variables explicativas" y pulse "Aceptar"
# Rcmdr responde con la siguiente instruccion en el cuadro de instrucciones (R Script)
RegModel.1 <- lm(income~age, data=Chile)
summary(RegModel.1)
# Observese que el modelo se ha establecido a RegModel.1
# Si se quiere predecir los valores para los ingresos (income) para los valores de edad (age) en el conjunto de datos Chile
# Del menu de Rcmdr seleccione:
# "Modelos" -> "Predicir usando el modelo activo" -> "Anadir predicciones a un conjunto de datos existente..."
# En el dialogo seleccione un conjunto de datos compatible con el modelo
# En este caso seleccione Chile
# Rcmdr responde con la siguiente instruccion en el cuadro de instrucciones (R Script)
Chile$fitted <- predict(RegModel.1, Chile)
# Los valores predichos para ingreso (income) se han guardado como fitted en el conjunto de datos (Chile)
