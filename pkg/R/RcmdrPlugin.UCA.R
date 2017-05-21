# Some Rcmdr useful extensions

# Note for the next function (last modified: 2013-01-24 by J. Fox): the following function (with contributions from Richard Heiberger and Milan Bouchet-Valat)
# can be included in any Rcmdr plug-in package to cause the package to load
# the Rcmdr if it is not already loaded
.onAttach <- function(libname, pkgname) {
    if (!interactive()) return()
    putRcmdr("slider.env", new.env())    
    Rcmdr <- options()$Rcmdr
    plugins <- Rcmdr$plugins
    if (!pkgname %in% plugins) {
        Rcmdr$plugins <- c(plugins, pkgname)
        options(Rcmdr=Rcmdr)
        if("package:Rcmdr" %in% search()) {
            if(!getRcmdr("autoRestart")) {
                closeCommander(ask=FALSE, ask.save=TRUE)
                Commander()
            }
        }
        else {
            Commander()
        }
    }
}

# Function to input data and predict values using active model
input2predict <- function() {
    # To ensure that menu name is included in pot file
    gettext("Predict using active model", domain="R-RcmdrPlugin.UCA")
    gettext("Input data and predict", domain="R-RcmdrPlugin.UCA")
    doItAndPrint(paste0(".data <- edit(", ActiveDataSet(), "[0,])"))
    doItAndPrint(".data")
    doItAndPrint(paste0("predict(", ActiveModel(), ", .data)"))
    doItAndPrint("remove(.data)")
    }

# Funnction to predict values for existing data set
predict4dataset <- function() {
    # To ensure that menu name is included in pot file
    gettext("Add predictions to existing dataset...", domain="R-RcmdrPlugin.UCA")
    dataSets <- listDataSets()
    .activeDataSet <- ActiveDataSet()
    initializeDialog(title=gettextRcmdr("Select Data Set"))
    dataSetsBox <- variableListBox(top, dataSets, title=gettextRcmdr("Data Sets (pick one)"), initialSelection=if (is.null(.activeDataSet)) NULL else which(.activeDataSet == dataSets) - 1)
    onOK <- function(){
        selection <- getSelection(dataSetsBox)
        closeDialog()
        setBusyCursor()
        on.exit(setIdleCursor())
        doItAndPrint(paste0(selection, "$fitted <- predict(", ActiveModel(), ", ", selection, ")"))
        activeDataSet(selection)
        tkfocus(CommanderWindow())
    }
    OKCancelHelp()
    tkgrid(getFrame(dataSetsBox), sticky="nw")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix()

}

# Function to be called by Rcmdr to test for randomness using runs.test from tseries packages
randomnessFTest <- function() {
  # To ensure that menu name is included in pot file
  gettext("Randomness test for two level factor...", domain="R-RcmdrPlugin.UCA")
  # Build dialog
  initializeDialog(title=gettext("Randomness test for two level factor", domain="R-RcmdrPlugin.UCA"))
  variablesBox <- variableListBox(top, TwoLevelFactors(), selectmode="single", initialSelection=NULL, title=gettextRcmdr("Variable (pick one)"))
	onOK <- function(){
		x <- getSelection(variablesBox)
		if (length(x) == 0) {
                    errorCondition(recall=randomnessNTest, message=gettextRcmdr("No variables were selected."))
                    return()
                }
		closeDialog()
                # Apply test
                doItAndPrint(paste("with(", ActiveDataSet(), ", twolevelfactor.runs.test(", x, "))", sep = ""))
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject="runs.test", reset = "randomnessFTest", apply = "randomnessFTest")
	tkgrid(getFrame(variablesBox), sticky="nw")
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=6, columns=1)
}

# Function to be called by Rcmdr to test for randomness using runs.test from randtest packages
randomnessNTest <- function() {
  # To ensure that menu name is included in pot file
  gettext("Randomness test for numeric variable...", domain="R-RcmdrPlugin.UCA")
  # Build dialog
  initializeDialog(title=gettext("Randomness test for numeric variable", domain="R-RcmdrPlugin.UCA"))
  variablesBox <- variableListBox(top, Numeric(), selectmode="single", initialSelection=NULL, title=gettextRcmdr("Variable (pick one)"))
	onOK <- function(){
		x <- getSelection(variablesBox)
		if (length(x) == 0) {
                    errorCondition(recall=randomnessNTest, message=gettextRcmdr("No variables were selected."))
                    return()
                }
		closeDialog()
                # Apply test
                doItAndPrint(paste("with(", ActiveDataSet(), ", numeric.runs.test(", x, "))", sep = ""))
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject="runs.test", reset = "randomnessNTest", apply = "randomnessNTest")
	tkgrid(getFrame(variablesBox), sticky="nw")
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=6, columns=1)
}

sigmaTest <- function() {
    ## This function is developed from singleSampleTTest in Rcmdr and use function sigma.test in package TeachingDemos
    defaults <- list (initial.x = NULL, initial.alternative = "two.sided", initial.level = ".95", initial.sigma = "1.0")
    dialog.values <- getDialog ("sigmaTest", defaults)
    initializeDialog(title = gettextRcmdr("Single-Sample Chi-squared-Test"))
    xBox <- variableListBox(top, Numeric(), title = gettextRcmdr("Variable (pick one)"), initialSelection = varPosn(dialog.values$initial.x, "numeric"))
    onOK <- function() {
        x <- getSelection(xBox)
        if (length(x) == 0) {
            errorCondition(recall = sigmaTest, message = gettextRcmdr("You must select a variable."))
            return()
        }
        alternative <- as.character(tclvalue(alternativeVariable))
        level <- tclvalue(confidenceLevel)
        sigma <- tclvalue(sigmaVariable)
        putDialog("sigmaTest", list (initial.x = x, initial.alternative = alternative, initial.level = level, initial.sigma = sigma))
        closeDialog()
        doItAndPrint(paste("with(", ActiveDataSet (), ", (sigma.test(", x, ", alternative='", alternative, "', sigma=", sigma, ", conf.level=", level, ")))", sep = ""))
        tkdestroy(top)
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject = "sigma.test", reset = "sigmaTest", apply = "sigmaTest")
    optionsFrame <- tkframe(top)
    radioButtons(optionsFrame, name = "alternative", buttons = c("twosided", "less", "greater"), values = c("two.sided", "less", "greater"), labels = gettextRcmdr(c("Population variance != sigma0", "Population variance < sigma0", "Population variance > sigma0")), title = gettextRcmdr("Alternative Hypothesis"), initialValue = dialog.values$initial.alternative)
    rightFrame <- tkframe(optionsFrame)
    confidenceFrame <- tkframe(rightFrame)
    confidenceLevel <- tclVar(dialog.values$initial.level)
    confidenceField <- ttkentry(confidenceFrame, width = "6", textvariable = confidenceLevel)
    sigmaFrame <- tkframe(rightFrame)
    sigmaVariable <- tclVar(dialog.values$initial.sigma)
    sigmaField <- ttkentry(sigmaFrame, width = "8", textvariable = sigmaVariable)
    tkgrid(getFrame(xBox), sticky = "nw")
    tkgrid(labelRcmdr(rightFrame, text = ""), sticky = "w")
    tkgrid(labelRcmdr(sigmaFrame, text = gettextRcmdr("Null hypothesis: sigma = ")),  sigmaField, sticky = "w", padx=c(10, 0))
    tkgrid(sigmaFrame, sticky = "w")
    tkgrid(labelRcmdr(confidenceFrame, text = gettextRcmdr("Confidence Level: ")), confidenceField, sticky = "w", padx=c(10, 0))
    tkgrid(confidenceFrame, sticky = "w")
    tkgrid(alternativeFrame, rightFrame, sticky = "nw")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
    tkgrid.configure(confidenceField, sticky = "e")
    dialogSuffix()
    }

numeric.runs.test <- function(...) randtest.runs.test(...)
twolevelfactor.runs.test <- function(...) tseries.runs.test(...)
