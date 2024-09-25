### Psychometry extensions: Difficulty index

#' @rdname Difficulty-index-for-answers
#' @name Difficulty index for answers
#' @title Difficulty index for answers
#'
#' @description
#' Difficulty index for answers represented by a factor variable.
#'
#' @details
#' To use this function from R-Commander, select from the menu "Psychometry" -> "Item analysis" -> "Difficulty index for answers..."
#'
#' @seealso \code{\link[RcmdrPlugin.UCA]{Difficulty index}}
#' 
#' @return Difficulty index.
#'
#' @export
PsyDIMenu <- function() {
    gettext('Psychometry')
    gettext('Item analysis')
    gettext('Difficulty index for answers...')
    ## Setup dialgo element list
    elements <- list(
        Variables = list(type = 'variablelist', title = gettext('Item answers'), variables = Dicotomics, selectmode = 'single', error = gettext('No variable were selected.'))
    )
    ## Setup onokcommand function
    onokcommand <- function(elements) {
        ## Get variable type and its levels
        .data <- eval(parse(text=elements[[1]]$variablelist), envir=get(ActiveDataSet(), envir=.GlobalEnv))
        if (is.numeric(.data)) {
            success <- 1
        } else {
            success <- paste0('"', levels(.data)[2], '"')
        }
        ## Build command
        paste0('difficultyindex(',
               ActiveDataSet(),
               '$',
               elements[[1]]$variablelist,
               ') # ',
               gettext('Difficulty index for success = '),
               success
               )
    }
    ## Call menu-dialog function
    .Menu(dialogtitle = gettext('Difficulty index for answers'), elements = elements, help = gettext('Difficulty index for answers'), recall = PsyDIMenu, reset = 'PsyDIMenu', apply = 'PsyDIMenu', onokcommand = onokcommand)
}

#' @rdname Indice-de-dificultad-de-respuestas
#' @name Índice de dificultad de respuestas
#' @title Indice de dificultad de respuestas
#'
#' @description
#' Índice de dificultad de respuestas respresentadas como una variable factor.
#'
#' @details
#' Para usar esta función desde R-Commander, seleccione del menú "Psicometría" -> "Análisis de ítems" -> "Índice de dificultad de respuestas..."
#'
#' @seealso \code{\link[RcmdrPlugin.UCA]{Índice de dificultad}}
#' 
#' @return Difficulty index.
NULL
