### Psychometry extensions: Numerical summaries

#' @name Multivariate Normality Test
#' 
#' @title Multivariate Normality Test
#' 
#' @description
#' It performs a Henze-Zirkler multivariate normality test.
#'
#' @details
#' Given a set of numerical variables (at least two), the Henze-Zirkler test for multivariate normality is performed, and the Anderson-Darling normality test is also performed for each variable.
#' This function calls \code{\link[MVN]{mvn}} of the MVN package.
#' Use the link to view additional information about the options for that function.
#'
#' To use this function from R-Commander, select from the menu "Statistics" -> "Summaries" -> "Multivariate normality test..."
#'
#' @seealso \code{\link[MVN]{mvn}}
#'
#' @return The results of the tests mentioned above.
#'
#' @export
mvnMenu <- function() {
    require('MVN', quietly = TRUE)
    gettext('Multivariate normality tests...')
    ## Setup dialog element list
    elements <- list(
        'Variables' = list(type = 'variablelist', title = gettext('Variables (pick two or more)'), variables = Numeric, selectmode = 'multiple', min = 2, error = gettext('You must select two or more variables.'))
    )
    ## Setup onokcommand function
    onokcommand <- function(elements) {
        paste0('mvn(',
               ActiveDataSet(),
               '[ , c(',
               paste0(paste0('\'', elements[[1]]$variablelist, '\''), collapse = ', '),
               ')], desc = FALSE',
               ')'
               )
    }
    ## Call menu-dialog function
    .Menu(dialogtitle = gettext('Multivariate normality tests'), elements = elements, help = gettext('Multivariate Normality Test'), recall = mvnMenu, reset = "mvnMenu", apply = "mvnMenu", onokcommand = onokcommand)
}

#' @rdname Test-de-normalidad-multivariante
#' @name Test de normalidad multivariante
#' 
#' @title Test de normalidad multivariante
#' 
#' @description
#' Realiza el test de normalidad multivariante de Henze-Zirkler.
#'
#' @details
#' Dado un conjunto de variables numéricas (al menos dos) se realiza el test de Henze-Zirkler para normlidad multivariante, además para cada variable se realiza el test de normalidad Anderson-Darling.
#' Esta función llama a \code{\link[MVN]{mvn}} del paquete MVN.
#' Use el enlace para ver información adicional sobre las opciones de dicha función.
#'
#' Para usar esta función desde R-Commander, seleccione del menú "Estadísticos" -> "Resúmenes" -> "Test de normalidad multivariante..."
#' 
#' @seealso \code{\link[MVN]{mvn}}
#'
#' @return Los resultados de los test mencionados anteriormente.
#'
NULL
