### Psychometry extensions

#' @name difficultyindex
#' @aliases Corrected_difficulty_index
#' @aliases difficultyindex
#' @aliases difficulty_index
#' @title Difficulty index
#' 
#' @description Difficulty index, and corrected difficulty index, for binary variables and two level factors.
#'
#' @details Computes the difficulty index for binary variables or two level factor.
#' It can also be applied to any numeric or factor variable taking into account that any value that differs from the given as success will be considered as failure.
#' This includes NA values.
#' @param x a vector that records if the answers are correct or wrong.
#' @param success the (only) value taken as correct answer.
#' @param noptions is the number of options in each question. It is used to calculate the corrected difficulty index. It must be integer and at least 2. If it is not an integer it will be rounded. When it is not provided, the uncorrected difficulty index is calculated.
#' @return Difficulty index or corrected difficulty index.
#'
#' @export
difficultyindex <- function(x, success, noptions = NA) {
    ## Check noptions parameter
    if (!missing(noptions)) {
        noptions <- suppressWarnings(round(noptions))
        if (is.na(noptions) || (noptions < 2)) stop(gettext('noptions is not an integer or is not at least 2'))
    }
    ## Compute the index
    if (!is.numeric(x)) x <- (x == success)
    di <- sum(x, na.rm = TRUE)/length(x)
    ## Compute the corrected index
    if (!missing(noptions)) di <- (noptions * di -1)/(noptions -1)
    ## Return
    di
}

#' @rdname indicedificultad
#' @name indicedificultad
#' @aliases Índice_de_dificultad
#' @aliases Índice_de_dificultad_corregido
#' @title Índice de dificultad
#' 
#' @description
#' Índice de dificultad, e índice de dificultad corregido, para variables binarias y factores con dos niveles.
#'
#' @details
#' Calcula el índice de dificultad para variables binarias o factor de dos niveles.
#' También se puede aplicar a cualquier variable numérica o factorial teniendo en cuenta que cualquier valor que difiera del dado como éxito será considerado como fracaso.
#' Esto incluye valores NA.
#' 
#' @param x un vector que indica si las respuestas son correctas o incorrectas.
#' @param success el (único) valor corisderado como correcto.
#' @param noptions es el número de opciones en cada pregunta. Se usa para calcular el índice de dificultad corregido. Debe ser entero y al menos 2. Cuando no se proporciona se calcula el índice dificultad no corregido.
#' 
#' @return
#' El índice de dificultad o el índice de dificultad corregido.
NULL

#' @export
DIMenu <- function() DIMenu_(corrected = FALSE)
#' @export
CDIMenu <- function() DIMenu_(corrected = TRUE)
DIMenu_ <- function(corrected)
{
    ## Check input parameter
    if (!isTRUE(corrected) && !isFALSE(corrected)) stop('corrected is not TRUE neither FALSE')
    ## To ensure that menu name is included in pot file
    gettext('Psychometry')
    gettext('Item analysis...')
    gettext('Difficulty index...')
    gettext('Corrected difficulty index...')
    ## Build dialog
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
    initializeDialog(title = title)
    ## Define variable selection box
    variablesBox <- variableListBox(top, Dicotomics(), selectmode="single", initialSelection=NULL, title=gettextRcmdr("Variable (pick one)"))
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
        command <- paste0("with(", ActiveDataSet(), ", difficultyindex(", x, ", success = ", success)
        if (corrected) {
            suppressWarnings(noptions <- round(as.numeric(tclvalue(noptionsVar))))
            if (is.na(noptions) || (noptions < 2)) {
                errorCondition(recall = recall, message = gettext('The number of options per question is not an integer or is not at least 2'))
                return()
                }
            command <- paste0(command, ", noptions = ", noptions)
            }
        command <- paste0(command, ")) # ", title, " ", gettext("for success"), " = ", success)
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
