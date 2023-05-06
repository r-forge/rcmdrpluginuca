### Psychometry extensions: Numerical summaries

#' @name numerical_summaries
#' @aliases Numerical_summaries
#' @aliases Numerical_item_analysis
#' 
#' @title Numerical summaries
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
#' To use this function from R-Commander, select from menu "Psychometry" -> "Item analysis..." -> "Numerical summaries..."
#' 
#' @seealso \code{\link[psychometric]{item.exam}}
#'
#' @return The abovementioned statistics.
#'
#' @export
mvnMenu <- function() {
    require('MVN', quietly = TRUE)
    gettext('Multivariate normality tests...')
    ## Setup dialog element list
    elements = list(
        'Variables' = list(type = 'variablelist', title = gettext('Variables (pick two or more)'), variables = Numeric, selectmode = 'multiple', min = 2, error = gettext('You must select two or more variables as question scores.'))
    )
    ## Setup onokcommand function
    onokcommand <- function(elements) {
        paste0('mvn(',
               ActiveDataSet(),
               '[ , c(',
               paste0(paste0('\'', elements[[1]]$variablelist, '\''), collapse = ', '),
               ')]',
               ')'
               )
    }
    ## Call menu-dialog function
    .Menu(dialogtitle = gettext('Multivariate normality tests'), elements = elements, help = gettext('mvn'), recall = mvnMenu, reset = "mvnMenu", apply = "mvnMenu", onokcommand = onokcommand)
}

#' @rdname resumenes_numericos
#' @name Resúmenes_numéricos
#' 
#' @title Resúmenes numéricos
#' 
#' @description
#' Calcular algunos resúmenes numéricos para los ítems.
#'
#' @details
#' Desde un conjunto de puntaciones de preguntas (al menos dos) y de test, esta función llama a \code{\link[psychometric]{item.exam}} del paquete phychometric.
#'
#' Su descripción, en inglés, en la documentación del paquete psychometric es:
#' Realiza un análisis a nivel de elemento. Proporciona correlaciones ítem-total, Desviación estándar en ítems, dificultad, discriminación e índices de confiabilidad y validez.
#'
#' Para usar esta función desde R-Commander, seleccione del menú "Psicometría" -> "Análisis de ítems..." -> "Resúmenes numéricos..."
#' 
#' @seealso \code{\link[psychometric]{item.exam}}
#'
#' @return Los estadísticos mencionados anteriormente.
#'
NULL
