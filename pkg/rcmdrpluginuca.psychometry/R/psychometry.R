### Psychometry extensions

#' @export
PsyA2SMenu <- function() {
    stop('Not yet implemented')
    ## Setup dialog element list
    elements <- list(
        'Answers' = list(type = 'variablelist', title = gettext('Answers (pick one or more)'), variables = DiscreteNumeric, selectmode = 'multiple',  error = gettext('You must select one or more variables as answers.')),
        'Score' = list(type = 'variablelist', title = gettext('Answer key (pick one)'), variables = DiscreteNumeric, selectmode = 'single', max = 1, error = gettext('You must select one variables as answer key.'))
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
    .Menu(dialogtitle = gettext('Numerical item analysis'), elements = elements, help = gettext('Numerical_item_analysis'), recall = PsyA2SMenu, reset = "PsyA2SMenu", apply = "PsyA2SMenu", onokcommand = onokcommand)
}


#' @export
PsyAlphaMenu <- function() {
    gettext('Cronbach\'s coefficient alpha...')
    ## Setup dialog element list
    elements = list(
        'Alpha' = list(type = 'variablelist', title = gettext('Question scores (pick two or more)'), variables = DiscreteNumeric, selectmode = 'multiple', min = 2, error = gettext('You must select two or more variables as question scores.'))
    )
    ## Setup onokcommand function
    onokcommand <- function(elements) {
        paste0('alpha(x = ',
               ActiveDataSet(),
               '[ , c(',
               paste0(paste0('\'', elements[[1]]$variablelist, '\''), collapse = ', '),
               ')]) # ',
               gettext('Cronbach\'s coefficient alpha')
               )
    }
    ## Call menu-dialog function
    .Menu(dialogtitle = gettext('Cronbach\'s coefficient alpha'), elements = elements, help = gettext('Cronbach\'s_coefficient_alpha'), recall = PsyAlphaMenu, reset = "PsyAlphaMenu", apply = "PsyAlphaMenu", onokcommand = onokcommand)
}


#' @export
PsyBSCMenu <- function() {
    gettext('Biserial correlations...')
    ## Setup dialog element list
    elements = list(
        'QScores' = list(type = 'variablelist', title = gettext('Question scores (pick one or more)'), variables = DiscreteNumeric, selectmode = 'multiple', error = gettext('You must select one or more variables as question score.')),
        'TScore' = list(type = 'variablelist', title = gettext('Test score (pick one)'), variables = DiscreteNumeric, selectmode = 'single', max = 1, error = gettext('You must select one variables as test score.'))
    )
    ## Setup onokcommand function
    onokcommand <- function(elements) {
        paste0('biserial(x = ',
               ActiveDataSet(),
               '$',
               elements[[2]]$variablelist,
               ', y = ',
               ActiveDataSet(),
               '[ , c(',
               paste0(paste0('\'', elements[[1]]$variablelist, '\''), collapse = ', '),
               ')])'
               )
    }
    ## Call menu-dialog function
    .Menu(dialogtitle = gettext('Biserial correlations'), elements = elements, help = gettext('Biserial_correlations'), recall = PsyBSCMenu, reset = "PsyBSCMenu", apply = "PsyBSCMenu", onokcommand = onokcommand)
}


#' @export
PsyGuttmanMenu <- function() {
    gettext('Guttman\'s coefficient...')
    ## Setup dialog element list
    elements = list(
        'Alpha' = list(type = 'variablelist', title = gettext('Question scores (pick two or more)'), variables = DiscreteNumeric, selectmode = 'multiple', min = 2, error = gettext('You must select two or more variables as question scores.'))
    )
    ## Setup onokcommand function
    onokcommand <- function(elements) {
        paste0('splitHalf(r = ',
               ActiveDataSet(),
               '[ , c(',
               paste0(paste0('\'', elements[[1]]$variablelist, '\''), collapse = ', '),
               ')]) # ',
               gettext('Guttman\'s coefficient')
               )
    }
    ## Call menu-dialog function
    .Menu(dialogtitle = gettext('Guttman\'s coefficient'), elements = elements, help = gettext('Guttman\'s_coefficient'), recall = PsyGuttmanMenu, reset = "PsyGuttmanMenu", apply = "PsyGuttmanMenu", onokcommand = onokcommand)
}


#' @export
PsyAlphaCIMenu <- function() {
    ## Setup dialog element list
    elements <- list(
        alpha = list(type = 'entry', title = gettext('alpha'), vartype = 'numeric', error = gettext('You must provide a number for alpha.')),
        k = list(type = 'entry', title = gettext('Number of items'), vartype = 'numeric', min = 1, error = gettext('You must provide a number, at least 1, as number of items.')),
        N = list(type = 'entry', title = gettext('Sample size'), vartype = 'numeric', min = 1, error = gettext('You must provide a number, at least as 1, as sample size.')),
        level = list(type = 'entry', title = gettext('Confidence level'), vartype = 'numeric', min = 0, max = 1, default = .9, error = gettext('You must provide a number in (0, 1) as conficende level.'))
    )
    ## Setup onokcommand function
    onokcommand <- function(elements) {
        paste0('alpha.CI(alpha =',
               elements[[1]]$numeric,
               ', k =',
               elements[[2]]$numeric,
               ', N = ',
               elements[[3]]$numeric,
               ', level = ',
               elements[[4]]$numeric,
               ')')
    }
    ## Call menu-dialog function
    gettext('Confidence interval for coefficient alpha...')
    .Menu(dialogtitle = gettext('Confidence interval for coefficient alpha'), elements = elements, help = gettext('Confidence_interval_for_coefficient_alpha'), recall = PsyAlphaCIMenu, reset = 'PsyAlphaCIMenu', apply = 'PsyAlphaCIMenu', onokcommand = onokcommand)
}


## omega from psych package
#' @export
PsySaturationMenu <- function() {
    gettext('Saturation (McDonald\'s omega)...')
    ## Setup dialog element list
    elements = list(
        'omega' = list(type = 'variablelist', title = gettext('Question scores (pick two or more)'), variables = DiscreteNumeric, selectmode = 'multiple', min = 2, error = gettext('You must select two or more variables as question scores.')),
        'nfactors' = list(type = 'entry', title = gettext('Number of factors'), vartype = 'integer', default = 3, min = 1, error = 'You must provide a number, at least 1, as number of factors')
    )
    ## Setup onokcommand function
    onokcommand <- function(elements) {
        nfactors <- min(elements[[2]]$integer, length(elements[[1]]$variablelist))
        paste0('omega(m = ',
               ActiveDataSet(),
               '[ , c(',
               paste0(paste0('\'', elements[[1]]$variablelist, '\''), collapse = ', '),
               ')], nfactors = ',
               nfactors,
               ') # ',
               gettext('Saturation (McDonald\'s omega with'),
               ' ',
               nfactors,
               ' ',
               gettext('factors'),
               ')'
               )
    }
    ## Call menu-dialog function
    .Menu(dialogtitle = gettext('Saturation (McDonald\'s omega)'), elements = elements, help = gettext('omega'), recall = PsySaturationMenu, reset = "PsySaturationMenu", apply = "PsySaturationMenu", onokcommand = onokcommand)
}



#' @export
#' Use the function spearman_brown from splithalfr package
PsySBMenu <-function() {
    gettext('Reliability of scores...')
    gettext('Spearman-Brown coefficient...')
    ## Setup dialog element list
    elements = list(
        'Test1' = list(type = 'variablelist', title = gettext('First part question scores'), variables = DiscreteNumeric, selectmode = 'multiple', error = gettext('Select at least one variable as first part question scores.')),
        'Test2' = list(type = 'variablelist', title = gettext('Second part question scores'), variables = DiscreteNumeric, selectmode = 'multiple', error = gettext('Select at least one variable as second part question scores.'))
    )
    ## Setup onokcommand function
    onokcommand <- function(elements) {
        if (length(elements[[1]]$variablelist) != length(elements[[2]]$variablelist)) {
            errorCondition(recall = PsySBMenu, message = gettext("Select the same number of variables for both parts"))
            return()            
        }
        paste0('with(',
               ActiveDataSet(),
               ', spearman_brown(x = cbind(',
               paste0(elements[[1]]$variablelist, collapse = ", "),
               '), y = cbind(',
               paste0(elements[[2]]$variablelist, collapse = ", "),
               ')))'
               )
    }
    ## Call menu-dialog function
    .Menu(dialogtitle = gettext('Spearman-Brown coefficient'), elements = elements, help = gettext('spearman_brown'), recall = PsySBMenu, reset = "PsySBMenu", apply = "PsySBMenu", onokcommand = onokcommand) 
}


#' @export
PsySBReliabilityMenu <-function() {
    gettext('Spearman-Browm formula for reliability...')
    ## Setup dialog element list
    elements = list(
        'Reliability' = list(type = 'entry', title = gettext('Original reliability'), vartype = 'numeric', error = gettext('You must provide a number for reliability.')),
        'Resize' = list(type = 'entry', title = gettext('Resize factor'), vartype = 'numeric', error = gettext('You must provide a number for resize factor.'))
    )
    ## Setup onokcommand function
    ## CTT package version
    onokcommand.ctt <- function(elements) {
        paste0('spearman.brown(r.xx = ',
               elements[[1]]$numeric,
               ', input = ',
               elements[[2]]$numeric,
               ', n.or.r = "n") # ',
               gettext('New reliability')
               )
    }
    ## psychometric package version
    onokcommand.psy <- function(elements) {
        paste0('SBrel(rxx = ',
               elements[[1]]$numeric,
               ', Nlength = ',
               elements[[2]]$numeric,
               ') # ',
               gettext('New reliability')
               )
    }
    ## Call menu-dialog function
    .Menu(dialogtitle = gettext('Spearman-Browm formula for reliability'), elements = elements, help = gettext('Spearman-Brown_reliability'), recall = PsySBReliabilityMenu, reset = "PsySBReliabilityMenu", apply = "PsySBReliabilityMenu", onokcommand = onokcommand.psy) 
}


#' @export
PsySBSizeMenu <-function() {
    gettext('Spearman-Browm formula for size...')
    ## Setup dialog element list
    elements = list(
        'Reliability' = list(type = 'entry', title = gettext('Original reliability'), vartype = 'numeric', error = gettext('You must provide a number for original reliability.')),
        'NewReliability' = list(type = 'entry', title = gettext('New reliability'), vartype = 'numeric', error = gettext('You must provide a number for new reliability.'))
    )
    ## Setup onokcommand function
    ## CTT package version
    onokcommand.ctt <- function(elements) {
        paste0('spearman.brown(r.xx = ',
               elements[[1]]$numeric,
               ', input = ',
               elements[[2]]$numeric,
               ', n.or.r = "r") # ',
               gettext('Resize factor')
               )
    }
    ## psychometric package version
    onokcommand.psy <- function(elements) {
        paste0('SBlength(rxx = ',
               elements[[1]]$numeric,
               ', rxxp = ',
               elements[[2]]$numeric,
               ') # ',
               gettext('Resize factor')
               )
    }
    ## Call menu-dialog function
    .Menu(dialogtitle = gettext('Spearman-Browm formula for size'), elements = elements, help = gettext('Spearman-Brown_size'), recall = PsySBSizeMenu, reset = "PsySBSizeMenu", apply = "PsySBSizeMenu", onokcommand = onokcommand.psy) 
}

