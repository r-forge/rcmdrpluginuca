### Psychometry extensions

#' @name difficultyindex
#' @aliases Corrected_difficulty_index
#' @aliases difficultyindex
#' @aliases Difficulty_index
#' @aliases Score_difficulty_index
#' 
#' @title Difficulty index
#' 
#' @description Difficulty index, and corrected difficulty index, for binary variables and two level factors.
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


#' @rdname indicedificultad
#' @name indicedificultad
#' @aliases Índice_de_dificultad
#' @aliases Índice_de_dificultad_corregido
#' @aliases Índice_de_dificultad_de_puntuaciones
#' 
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
#' @return
#' El índice de dificultad o el índice de dificultad corregido.
NULL


#' @export
PsyCDIMenu <- function() DIMenu_(corrected = TRUE)
#' @export
PsyDIMenu <- function() DIMenu_(corrected = FALSE)
#' @export
SDIMenu <- function() DIMenu_(discrete = TRUE)
## Low level function for menus
DIMenu_ <- function(corrected = FALSE, discrete = FALSE)
{
    ## Check input parameters
    if (!isTRUE(corrected) && !isFALSE(corrected)) stop('corrected is not TRUE neither FALSE')
    if (!isTRUE(discrete) && !isFALSE(discrete)) stop('discrete is not TRUE neither FALSE')
    ## To ensure that menu name is included in pot file
    gettext('Corrected difficulty index...')
    gettext('Difficulty index...')
    gettext('Item analysis...')
    gettext('Psychometry')
    gettext('Score difficulty index...')
    ## Build dialog
    if (discrete) {
        help <- gettext("Score_difficulty_index")
        menu <- "SDIMenu"
        recall <- SDIMenu
        title <- gettext("Score difficulty index")
        variables <- DiscreteNumeric()
    } else {
        variables <- Dicotomics()
        if (corrected) {
            help <- gettext("Corrected_difficulty_index")
            menu <- "PsyCDIMenu"
            recall <- PsyCDIMenu
            title <- gettext("Corrected difficulty index")
        } else {
            help <- gettext("Difficulty_index")
            menu <- "PsyDIMenu"
            recall <- PsyDIMenu
            title <- gettext("Difficulty index")
        }
    }
    initializeDialog(title = title)
    ## Define variable selection box
    variablesBox <- variableListBox(top, variables, selectmode="single", initialSelection=NULL, title=gettextRcmdr("Variable (pick one)"))
    ## Define noptions input box
    if (corrected) {
        noptionsVar <- tclVar("")
        noptionsEntry <- tkentry(top, width = "8", textvariable = noptionsVar)
    }
    onOK <- function() {
        x <- getSelection(variablesBox)
        if (length(x) == 0) {
            errorCondition(recall = menu, message = gettextRcmdr("No variable were selected."))
            return()
        }
        ## Get variable type and its levels
        .data <- eval(parse(text=x), envir=get(ActiveDataSet(), envir=.GlobalEnv))
        if (is.numeric(.data)) {
            success <- 1
        } else {
            success <- paste0('"', levels(.data)[2], '"')
        }
        command <- paste0("with(", ActiveDataSet(), ", difficultyindex(", x)
        if (isFALSE(discrete)) command <- paste0(command, ", success = ", success)
        if (corrected) {
            suppressWarnings(noptions <- round(as.numeric(tclvalue(noptionsVar))))
            if (is.na(noptions) || (noptions < 2)) {
                errorCondition(recall = recall, message = gettext('The number of options per question is not an integer or is not at least 2'))
                return()
            }
            command <- paste0(command, ", noptions = ", noptions)
        }
        command <- paste0(command, ")) # ", title)
        if (isFALSE(discrete)) command <- paste0(command, " ", gettext("for success"), " = ", success)
        closeDialog()
        ## Execute command
        doItAndPrint(command)
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject = help, reset = menu, apply = menu)
    tkgrid(getFrame(variablesBox), sticky="nw")
    if (corrected) {
        tkgrid(tklabel(top, text = gettext("Number of options per question:")), noptionsEntry, sticky = "w")
    }
    tkgrid(buttonsFrame, sticky="w", columnspan = 8)
    dialogSuffix(rows=3, columns = 8)
}




#' @export
NIAMenu <- function() {
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
    .Menu(dialogtitle = gettext('Numerical item analysis'), elements = elements, help = gettext('Numerical_item_analysis'), recall = NIAMenu, reset = "NIAMenu", apply = "NIAMenu", onokcommand = onokcommand)
}


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
    .Menu(dialogtitle = gettext('Numerical item analysis'), elements = elements, help = gettext('Numerical_item_analysis'), recall = NIAMenu, reset = "NIAMenu", apply = "NIAMenu", onokcommand = onokcommand)
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


#' @export
PsySaturationMenu <- function() {
    gettext('Saturation (McDonald\'s omega)...')
    ## Setup dialog element list
    elements = list(
        'omega' = list(type = 'variablelist', title = gettext('Question scores (pick three or more)'), variables = DiscreteNumeric, selectmode = 'multiple', min = 3, error = gettext('You must select three or more variables as question scores.')),
        'nfactors' = list(type = 'entry', title = gettext('Number of factors'), vartype = 'integer', min = 3, error = 'You must provide a number, at least 3, as number of factors')
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
               gettext('Saturation (McDonald\'s omega)')
               )
    }
    ## Call menu-dialog function
    .Menu(dialogtitle = gettext('Saturation (McDonald\'s omega)'), elements = elements, help = gettext('Saturation_McDonald_omega'), recall = PsySaturationMenu, reset = "PsySaturationMenu", apply = "PsySaturationMenu", onokcommand = onokcommand)
}



#' @export
PsySBMenu <-function() {
    gettext('Reliability of scores...')
    gettext('Spearman-Brown coefficient...')
    ## Setup dialog element list
    elements = list(
        'Test1' = list(type = 'variablelist', title = gettext('First test scores (pick one)'), variables = DiscreteNumeric, selectmode = 'single', max = 1, error = gettext('You must select one variable as first test scores.')),
        'Test2' = list(type = 'variablelist', title = gettext('Second test scores (pick one)'), variables = DiscreteNumeric, selectmode = 'single', max = 1, error = gettext('You must select one variable as second test scores.'))
    )
    ## Setup onokcommand function
    onokcommand <- function(elements) {
        paste0('with(',
               ActiveDataSet(),
               ', spearman_brown(x = ',
               elements[[1]]$variablelist,
               ', y = ',
               elements[[2]]$variablelist,
               '))'
               )
    }
    ## Call menu-dialog function
    .Menu(dialogtitle = gettext('Spearman-Brown coefficient'), elements = elements, help = gettext('Spearman-Brown_coefficient'), recall = PsySBMenu, reset = "PsySBMenu", apply = "PsySBMenu", onokcommand = onokcommand) 
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

