### Psychometry extensions: Test score

#' @name test_score
#' @aliases Test_score
#' 
#' @title Test score
#' 
#' @description
#' Test score calculation from question scores.
#'
#' @details
#' Computes the test scores from binary, discrete numeric, or two level factor variables.
#' For factor type variables, the last level is taken as one and the other as 0.
#' NA's are allowed, but the result will be NA.
#'
#' To use this function from R-Commander, select from menu "Psychometry" -> "Data..." -> "Test Score..."
#' 
#' @return It returns nothing. This function is used for its side effect of creating a new column in the active data set.
#'
#' @export
PsyScoreMenu <- function() {
    gettext("Data...")
    gettext("Test score...")
    ## Setup dialog element list
    elements = list(
        'QScores' = list(type = 'variablelist', title = gettext('Question scores (pick two or more)'), variables = DiscreteNumeric, selectmode = 'multiple', min = 2, error = gettext('You must select two or more variables as question score.')),
        'NewVariable' = list(type = 'entry', title = gettext('New variable name'), vartype = 'varname', error = gettext('You must provide a valid name for new variable.'))
        )
    ## Setup onokcommand function
    onokcommand <- function(elements) {
        paste0(ActiveDataSet(),
               "$",
               elements[[2]]$varname,
               " <- with(",
               ActiveDataSet(),
               ", ",
               paste0(elements[[1]]$variablelist, collapse = " + "),
               ")"
               )
    }
    ## Call menu-dialog function
    .Menu(dialogtitle = gettext('Test score'), elements = elements, help = gettext('Test_score'), recall = PsyScoreMenu, reset = "PsyScoreMenu", apply = "PsyScoreMenu", onokcommand = onokcommand)   
}


#' @rdname puntuaciones_test
#' @name Puntuación_del_test
#' 
#' @title Puntuaciones de test
#' 
#' @description
#' Cálculo de la puntuación del test a partir de las puntuaciones de las preguntas.
#'
#' @details
#' Calcula las puntuaciones del test a partir de variables binarias, numéricas discretas o de factores dos niveles.
#' Para variables de tipo factor, se toma el último nivel como uno y el otro como 0.
#' Se permiten NAs, pero el resultado será NA.
#'
#' Para usar esta función desde R-Commander, seleccione del menú "Psicometría" -> "Datos..." -> "Puntuación del test..."
#'
#' @return No devuelve nada. Esta función se usa por su efecto colateral de crear una nueva variable en el conjunto de datos activo.
NULL
