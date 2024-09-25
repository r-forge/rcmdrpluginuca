\name{predictMenu}
\title{Menu option to make predictions using active model}
\alias{Predict menu}
\docType{package}
\encoding{UTF-8}
\description{
  The menu entry "Predict using active model", in models menu, has two
  options to predict data using active model depending on how the data
  for predictor variables will be provided.
  
  The first entry is "Input data and predict".
  If you select this option a new data set, as a data.frame, will be created and the
  editor will be invoked.
  Then you can entry the values of the predictor variable that you want
  to use for prediction, the values for non predictor variables are not
  required.
  When you close the data editor the predicted values for predicted
  variable are shown. 

  The second menu entry "Add predictions to existing dataset..." allows
  to us to add predictions to existing dataset that provides the data
  values for all predictor variables.
  After selecting this option, the user can select an existing data set
  using dialog box.

  If the data set does not provides the values for all predicting
  variables an error will occur and no predicted values will be
  provided.

  Unlike the menu option "Add observation statistics to data...", this
  option can be used with a different data set than the one used to
  construct the model, if that dataset provides the values for all the
  predictor variables.
}

\details{
  Here is an example of use of "Input data and predict" menu entry.

  Load data "Chile" selecting from Rcmdr menu: "Data" ->
  "Data in packages" -> "Read data set from an attached package..." then
  double-click on "car", click on "Chile" and on "OK".

  Rcmdr reply with the following command in source pane (R Script)

  \code{data(Chile, package="car")}

  To build a model select from Rcmdr menu: "Statistics" -> "Model fit"
  -> "Linear Regresion...". As "Response variable" select income and age
  as "Explanatory variables" and click on "OK". Rcmdr reply with the
  following command in source pane (R Script)

  \code{RegModel.1 <- lm(income~age, data=Chile)}
  
  \code{summary(RegModel.1)}

  Note that the active model is set to RegModel.1. So if you want to
  predict a new value for a 35 and 40 age person. Select from Rcmdr
  menu: "Models" -> "Predict using active model" ->
  "Input data and predict". In age column input 35 and 40 and then close
  the editor. Rcmdr reply with the following command in source pane (R
  Script)

  \code{.data <- edit(Chile[0,])}

  \code{.data}

  \code{predict(RegModel.1, .data)}

  \code{remove(.data)}

  And output the predicted value of income for that age using active model (RegModel.1)
  
  Here is an example of use of "Add predictions to existing dataset..."
  menu entry.
  
  Load data "Chile" selecting from Rcmdr menu: "Data" ->
  "Data in packages" -> "Read data set from an attached package..." then
  double-click on "car", click on "Chile" and "OK".
  
  Rcmdr reply with the following command in source pane (R Script)
  
  \code{data(Chile, package="car")}
  
  To build a model select from Rcmdr menu: "Statistics" -> "Model fit"
  -> "Linear Regresion..." as "Response variable" select income and age
  as "Explanatory variables" and click on "OK".
  Rcmdr reply with the following command in source pane (R Script)
  
  \code{RegModel.1 <- lm(income~age, data=Chile)}

  \code{summary(RegModel.1)}
  
  Note that the active model is set to RegModel.1
  So if you want to predict the values for income for age data in Chile
  dataset. Select from Rcmdr menu: "Models" ->
  "Predict using active model" ->
  "Add predictions to existing dataset...".
  In the dialog select a compatible dataset with the model. In this case
  select Chile. Rcmdr reply with the following command in source pane (R
  Script)
  
  \code{Chile$fitted <- predict(RegModel.1, Chile)}
  
  The predicted value of income has been saved as fitted in the selected dataset (Chile)
  }

\author{
Manuel Munoz-Marquez <manuel.munoz@uca.es>
}

\keyword{package}

\seealso{
For more information see \code{\link[Rcmdr]{Rcmdr-package}}. 

Para ayuda en \enc{español}{espanol}, \enc{véase}{vease}
\code{\link{Menú predicciones (es)}}. (For Spanish help see \code{\link{Menú predicciones (es)}}.) 
}
