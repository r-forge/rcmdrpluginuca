# Example of "Input data and predict"
# Load data "Chile" selecting from Rcmdr menu:
# "Data" -> "Data in packages" -> "Read data set from an attached package..."
# then double-click on "car", click on "Chile" and on "OK".
# Rcmdr reply with the following command in source pane (R Script)
data(Chile, package="car")
# To build a model select from Rcmdr menu:
# "Statistics" -> "Model fit" -> "Linear Regresion..."
# As "Response variable" select income and age as "Explanatory variables" and click on "OK"
# Rcmdr reply with the following command in source pane (R Script)
RegModel.1 <- lm(income~age, data=Chile)
summary(RegModel.1)
# Note that the active model is set to RegModel.1
# So if you want to predict a new value for a 35 and 40 age person
# Select from Rcmdr menu:
# "Models" -> "Predict using active model" -> "Input data and predict"
# In age column input 35 and 40 and then close the editor.
# Rcmdr reply with the following command in source pane (R Script)
.data <- edit(data.frame(age = numeric(0)))
.data
predict(RegModel.1, .data)
remove(.data)
# And output the predicted value of income for that age using active model (RegModel.1)
