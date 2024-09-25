# Example of "Add predictions to existing dataset..."
# Load data "Chile" selecting from Rcmdr menu:
# "Data" -> "Data in packages" -> "Read data set from an attached package..."
# then double-click on "car", click on "Chile" and "OK".
# Rcmdr reply with the following command in source pane (R Script)
data(Chile, package="car")
# To build a model select from Rcmdr menu:
# "Statistics" -> "Model fit" -> "Linear Regresion..."
# As "Variable explicada" select income and age as "Variables explicativas" and click on "OK"
# Rcmdr reply with the following command in source pane (R Script)
RegModel.1 <- lm(income~age, data=Chile)
summary(RegModel.1)
# Note that the active model is set to RegModel.1
# So if you want to predict the values for income for age data in Chile dataset
# Select from Rcmdr menu:
# "Models" -> "Predict using active model" -> "Add predictions to existing dataset..."
# In the dialog select a compatible dataset with the model
# In this case select Chile
# Rcmdr reply with the following command in source pane (R Script)
Chile$fitted <- predict(RegModel.1, Chile)
# The predicted value of income has been saved as fitted in the selected dataset (Chile)
