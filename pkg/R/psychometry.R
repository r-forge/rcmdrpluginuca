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
CDIMenu <- function() DIMenu_(corrected = TRUE)
#' @export
DIMenu <- function() DIMenu_(corrected = FALSE)
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
            menu <- "CDIMenu"
            recall <- CDIMenu
            title <- gettext("Corrected difficulty index")
        } else {
            help <- gettext("Difficulty_index")
            menu <- "DIMenu"
            recall <- DIMenu
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
        'QScores' = list(type = 'variableListBox', title = gettext('Question scores (pick two or more)'), selectmode = 'multiple', min = 2, error = gettext('You must select two or more variables as question scores.')),
        'TScores' = list(type = 'variableListBox', title = gettext('Test score (pick one)'), selectmode = 'single', max = 1, error = gettext('You must select one variables as test score.'))
    )
    ## Setup onokcommand function
    onokcommand <- function(elements) {
        paste0('item.exam(x = ',
               ActiveDataSet(),
               '[ , c(',
               paste0(paste0('\'', elements[[1]]$variableListBoxSelected, '\''), collapse = ', '),
               ')], y = ',
               ActiveDataSet(),
               '$',
               elements[[2]]$variableListBoxSelected,
               ', discrim = TRUE)'
               )
    }
    ## Call menu-dialog function
    .Menu(dialogtitle = gettext('Numerical item analysis'), elements = elements, help = gettext('Numerical_item_analysis'), recall = NIAMenu, reset = "NIAMenu", apply = "NIAMenu", onokcommand = onokcommand)
}

#' @export
TestScoreMenu <- function() {
    gettext("Test score...")
    initializeDialog(title = gettext("Test score"))
    variablesBox <- variableListBox(top, DiscreteNumeric(), selectmode="multiple", initialSelection=NULL, title = gettext("Question scores (pick two or more)"))
    variablesFrame <- tkframe(top)
    nameVar <- tclVar()
    nameEntry <- ttkentry(variablesFrame, width="20", textvariable = nameVar)
    onOK <- function() {
        scores <- getSelection(variablesBox)
        name <- tclvalue(nameVar)
        if (length(scores) < 2) {
            errorCondition(recall = TestScoreMenu, message = gettext("You must select two or more variables."))
            return()
        }
        if (!is.valid.name(name)) {
            errorCondition(recall = TestScoreMenu, message = paste(name, gettextRcmdr("is not a valid name.")))
            return()
        }
        closeDialog()
        if (is.element(name, Variables())) {
            if ("no" == tclvalue(checkReplace(name, gettext("TestScore")))) {
                TestScoreMenu()
                return()
            }
        }
        command <- paste0(ActiveDataSet(), "$", name, " <- with(", ActiveDataSet(), ", ", paste0(scores, collapse = " + "), ")")
        doItAndPrint(command)
        activeDataSet(ActiveDataSet(), flushModel = FALSE, flushDialogMemory = FALSE)
        ## activeDataSetP()
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject = "Test_score", reset = "TestScoreMenu", apply = "TestScoreMenu")
    tkgrid(getFrame(variablesBox), sticky="nw")
    tkgrid(tklabel(variablesFrame, text=gettextRcmdr("New variable name")), nameEntry, sticky = "w")
    tkgrid(variablesFrame, sticky = "w")
    tkgrid(buttonsFrame, sticky = "w", columnspan = 8)
    dialogSuffix(columns = 8)
}
