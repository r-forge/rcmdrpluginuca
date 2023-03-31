### Psychometry extensions

#' @name difficultyindex
#' @aliases difficultyindex
#' @aliases difficulty_index
#' @title Difficulty index
#' 
#' @description Difficulty index definition for binary variables and two level factors.
#'
#' @details Computes the difficulty index for binary variables or two level factor.
#' It can also be applied to any numeric or factor variable taking into account that any value that differs from the given as success will be considered as failure.
#' This includes NA values.
#' @param x a vector that records if the answers are correct or wrong.
#' @param success the (only) value taken as correct answer.
#' @return The computed difficulty index.
#
#' @export
difficultyindex <- function(x, success) {
    if (!is.numeric(x)) x <- (x == success)
    sum(x, na.rm = TRUE)/length(x)
}

#' @rdname indicedificultad
#' @name indicedificultad
#' @aliases Índice_de_dificultad
#' @title Índice de dificultad
#' 
#' @description
#' Definición del índice de dificultad para variables binarias y factores de dos niveles.
#'
#' @details
#' Calcula el índice de dificultad para variables binarias o factor de dos niveles.
#' También se puede aplicar a cualquier variable numérica o factorial teniendo en cuenta que cualquier valor que difiera del dado como éxito será considerado como fracaso.
#' Esto incluye valores NA.
#' 
#' @param x: un vector que indica si las respuestas son correctas o incorrectas.
#' @param success: el (único) valor corisderado como correcto.
#' 
#' @return
#' El índice de dificultad calculado.
NULL

#' @export
DIMenu <- function(numeric)
{
    ## To ensure that menu name is included in pot file
    gettext('Psychometry')
    gettext('Item analysis...')
    gettext('Difficulty index...')
    ## Build dialog
    initializeDialog(title=gettext("Difficulty index"))
    variablesBox <- variableListBox(top, Dicotomics(), selectmode="single", initialSelection=NULL, title=gettextRcmdr("Variable (pick one)"))
    onOK <- function(){
        x <- getSelection(variablesBox)
        if (length(x) == 0) {
            errorCondition(recall=difficultyindex, message=gettextRcmdr("No variable were selected."))
            return()
        }
        closeDialog()
        ## Get variable type and its levels
        .data <- eval(parse(text=x), envir=get(ActiveDataSet(), envir=.GlobalEnv))
        if (is.numeric(.data)) {
            success <- 1
        } else {
            success <- paste0('"', levels(.data)[2], '"')
        }
        ## Apply test
        doItAndPrint(paste("with(", ActiveDataSet(), ", difficultyindex(", x, ", success = ", success, ")) # ", gettext("for success"), " = ", success, sep = ""))
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject=gettext("Difficulty_index"), reset = "DIMenu", apply = "DIMenu")
    tkgrid(getFrame(variablesBox), sticky="nw")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=2, columns=1)
}
