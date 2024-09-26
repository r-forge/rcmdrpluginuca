### Psychometry extensions: Score difficulty index

#' @rdname Score-difficulty-index
#' @name Score difficulty index
#' @title Score difficulty index
#'
#' @description
#' Score difficulty index for test score given as a numeric variable.
#'
#' @details
#' To use this function from R-Commander, select from the menu "Psychometry" -> "Item analysis" -> "Score difficulty index..."
#'
#' @seealso \code{\link[RcmdrPlugin.UCA]{Difficulty index}}
#' 
#' @return Difficulty index.
#'
#' @export
PsySDIMenu <- function() {
    gettext('Psychometry')
    gettext('Item analysis')
    gettext('Score difficulty index...')
    ## Setup dialgo element list
    elements <- list(
        Variables = list(type = 'variablelist', title = gettext('Test scores'), variables = Numeric, selectmode = 'single', max = 1, error = gettext('No variable were selected.'))
    )
    ## Setup onokcommand function
    onokcommand <- function(elements) {
        ## Build command
        paste0('difficultyindex(',
               ActiveDataSet(),
               '$',
               elements[[1]]$variablelist,
               ')'
               )
    }
    ## Call menu-dialog function
    .Menu(dialogtitle = gettext('Score difficulty index'), elements = elements, help = gettext('Score difficulty index'), recall = PsySDIMenu, reset = 'PsySDIMenu', apply = 'PsySDIMenu', onokcommand = onokcommand)
}

#' @rdname Indice-de-dificultad-de-puntuaciones
#' @name Índice de dificultad de puntuaciones
#' @title Índice de dificultad de puntuaciones
#'
#' @description
#' Índice de dificultad para una variable numérica que representa la puntuación en un test.
#'
#' @details
#' Para usar esta función desde R-Commander, seleccione del menú "Psicometría" -> "Análisis de ítems" -> "Índice de dificultad de puntuaciones..."
#'
#' @seealso \code{\link[RcmdrPlugin.UCA]{Índice de dificultad}}
#' 
#' @return Difficulty index.
NULL
