### Psychometry extensions: Numerical summaries of test scores

#' @name Item numerical summary
#' 
#' @title Item numerical summary
#' 
#' @description
#' Calculate some numerical summaries for items
#'
#' @details
#' From a set of question scores (at least two) and a test scores, this function calls to \code{\link[psychometric]{item.exam}} from psychometric packages.
#'
#' Its description in psychometric package documentation is:
#' Conducts an item level analysis. Provides item-total correlations, Standard deviation in items, difficulty, discrimination, and reliability and validity indices.
#'
#' To use this function from R-Commander, select from menu "Psychometry" -> "Item analysis" -> "Numerical summaries..."
#' 
#' @seealso \code{\link[psychometric]{item.exam}}
#'
#' @return The abovementioned statistics.
#'
#' @export
PsySummaryMenu <- function() {
    require('psychometric', quietly = TRUE)
    gettext('Item analysis')
    ## Setup dialog element list
    elements = list(
        'QScores' = list(type = 'variablelist', title = gettext('Question scores (pick two or more)'), variables = DiscreteNumeric, selectmode = 'multiple', min = 2, error = gettext('You must select two or more variables as question scores.')),
        'TScore' = list(type = 'variablelist', title = gettext('Test score (pick one)'), variables = DiscreteNumeric, selectmode = 'single', max = 1, error = gettext('You must select one variables as test score.'))
    )
    ## Setup onokcommand function
    onokcommand <- function(elements) {
        paste0('item.exam(x = ',
               ActiveDataSet(),
               '[ , c(',
               paste0(paste0('\'', elements[[1]]$variablelist, '\''), collapse = ', '),
               ')], y = ',
               ActiveDataSet(),
               '$',
               elements[[2]]$variablelist,
               ', discrim = TRUE)'
               )
    }
    ## Call menu-dialog function
    .Menu(dialogtitle = gettext('Item numerical summary'), elements = elements, help = gettext('Item numerical summary'), recall = PsySummaryMenu, reset = "PsySummaryMenu", apply = "PsySummaryMenu", onokcommand = onokcommand)
}

#' @rdname Resumen-numerico-de-items
#' @name Resumen numérico de ítems
#' 
#' @title Resumen numérico de ítems
#' 
#' @description
#' Calcular algunos resúmenes numéricos para los ítems.
#'
#' @details
#' Desde un conjunto de puntaciones de preguntas (al menos dos) y de test, esta función llama a \code{\link[psychometric]{item.exam}} del paquete psychometric.
#'
#' Su descripción, en inglés, en la documentación del paquete psychometric es:
#' Realiza un análisis a nivel de elemento. Proporciona correlaciones ítem-total, Desviación estándar en ítems, dificultad, discriminación e índices de confiabilidad y validez.
#'
#' Para usar esta función desde R-Commander, seleccione del menú "Psicometría" -> "Análisis de ítems" -> "Resúmenes numéricos..."
#' 
#' @seealso \code{\link[psychometric]{item.exam}}
#'
#' @return Los estadísticos mencionados anteriormente.
#'
NULL
