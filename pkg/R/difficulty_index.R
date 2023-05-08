### Psychometry extensions: Difficulty index

#' @rdname Difficulty-index
#' @name Difficulty index
#' @title Difficulty index
#' 
#' @description
#' Difficulty index, and corrected difficulty index, for binary variables and two level factors.
#'
#' @details Computes the difficulty index for binary variables, discrete numeric variable, or two level factor.
#' For binary variables, 1 is taken as correct.
#' For factor type variables, the last level is taken as correct.
#' If the variable is discrete, they are considered to represent scores starting at 0.
#' It can also be applied to any numeric or factor variable taking into account that any value that differs from the given as success will be considered as failure.
#' This includes NA values.
#' @param x a vector that records if the answers are correct or wrong. It can also be a vector of scores with 0 being the lowest score.
#' @param success the (only) value taken as correct answer. In the case of scores, this value is always taken as the maximum, regardless of the value supplied for this parameter.
#' @param noptions is the number of options in each question. It is used to calculate the corrected difficulty index. It must be integer and at least 2. If it is not an integer it will be rounded. When it is not provided, the uncorrected difficulty index is calculated.
#'
#' To use this function form R-Commander, select from menu "Psychometry" -> "Item analysis" -> "Difficulty index...".
#'
#' To compute the corrected difficulty index select from menu "Psychometry" -> "Item analysis" -> "Corrected difficulty index...".
#' 
#' @return Difficulty index or corrected difficulty index.
#'
#' @export
difficultyindex <- function(x, success = NULL, noptions = NULL) {
    ## Check noptions parameter
    if (!missing(noptions)) {
        noptions <- suppressWarnings(round(noptions))
        if (is.na(noptions) || (noptions < 2)) stop(gettext('noptions is not an integer or is not at least 2'))
    }
    ## Compute the index
    if (!is.numeric(x)) x <- (x == success)
    di <- sum(x, na.rm = TRUE)/length(x)
    ## Non binary case
    if (is.numeric(x)) di <- di/max(x, na.rm = TRUE)
    ## Compute the corrected index
    if (!missing(noptions)) di <- (noptions * di -1)/(noptions -1)
    ## Return
    di
}


#' @rdname Indice-de-dificultad
#' @name Índice de dificultad
#' @title Índice de dificultad
#' 
#' @description
#' Índice de dificultad, e índice de dificultad corregido, para variables binarias y factores con dos niveles.
#'
#' @details
#' Calcula el índice de dificultad para variables binarias, variables numéricas discretas o factor con dos niveles.
#' Para variables binarias se toma 1 como correcta.
#' Para variables de tipo factor, se toma como correcto el último nivel.
#' Si la variable es discreta, se considera que representan puntuaciones que comienzan en 0.
#' También se puede aplicar a cualquier variable numérica o factorial teniendo en cuenta que cualquier valor que difiera del dado como éxito será considerado como fracaso.
#' Esto incluye valores NA.
#' 
#' @param x un vector que indica si las respuestas son correctas o incorrectas. También puede ser un vector de puntuaciones siendo 0 la puntuación más baja.
#' @param success el (único) valor corisderado como correcto. En el caso de puntuaciones, este valos siempre se toma como el máximo, independientemente del valor suministrado para éste parámetro.
#' @param noptions es el número de opciones en cada pregunta. Se usa para calcular el índice de dificultad corregido. Debe ser entero y al menos 2. Cuando no se proporciona se calcula el índice dificultad no corregido.
#'
#' Para usar esta función desde R-Commander, seleccione en el menú "Psicometría" -> "Análisis de ítems" -> "Índice de dificultad...".
#'
#' Para calcular el índice de dificultad corregido, seleccione en el menú "Psicometría" -> "Análisis de ítems" -> "Índice de dificultad corregido...".
#' 
#' @return
#' El índice de dificultad o el índice de dificultad corregido.
NULL

