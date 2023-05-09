### Psychometry extensions: Corrected difficulty index

#' @rdname Corrected-difficulty-index
#' @name Corrected difficulty index
#' @title Corrected difficulty index
#'
#' @description
#' Corrected difficulty index for a dicotomic variable.
#'
#' @details
#' To use this function from R-Commander, select from the menu "Psychometry" -> "Item analysis" -> "Corrected difficulty index..."
#'
#' @seealso \code{\link[RcmdrPlugin.UCA]{Difficulty index}}
#' 
#' @return Difficulty index.
#'
#' @export
PsyCDIMenu <- function() {
    gettext('Psychometry')
    gettext('Item analysis')
    gettext('Corrected difficulty index...')
    ## Setup dialgo element list
    elements <- list(
        Variables = list(type = 'variablelist', title = gettext('Item answers'), variables = Dicotomics, selectmode = 'single', error = gettext('No variable were selected.')),
        NOptions = list(type = 'entry', title = gettext("Number of options per question:"), vartype = 'integer', min = 2, error = gettext('The number of options per question is not an integer or is not at least 2'))
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
               ', noptions = ',
               elements[[2]]$integer,
               ') # ',
               gettext('Corrected difficulty index for success = '),
               success
               )
    }
    ## Call menu-dialog function
    .Menu(dialogtitle = gettext('Corrected difficulty index'), elements = elements, help = gettext('Corrected difficulty index'), recall = PsyCDIMenu, reset = 'PsyCDIMenu', apply = 'PsyCDIMenu', onokcommand = onokcommand)
}

#' @rdname Indice-de-dificultad-corregido
#' @name Índice de dificultad corregido
#' @title Índice de dificultad corregido
#'
#' @description
#' Índice de dificultad corregido de una variable dicotómica.
#'
#' @details
#' Para usar esta función desde R-Commander, seleccione del menú "Psicometría" -> "Análisis de ítems" -> "Índice de dificultad corregido..."
#'
#' @seealso \code{\link[RcmdrPlugin.UCA]{Índice de dificultad}}
#' 
#' @return Difficulty index.
NULL
