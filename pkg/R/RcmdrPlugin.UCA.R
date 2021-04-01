### Some Rcmdr useful extensions

### Note for the next function (last modified: 2013-01-24 by J. Fox): the following function (with contributions from Richard Heiberger and Milan Bouchet-Valat)
### can be included in any Rcmdr plug-in package to cause the package to load
### the Rcmdr if it is not already loaded
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


### Function to input data and predict values using active model
input2predict <- function() {
    ## To ensure that menu name is included in pot file
    gettext("Predict using active model", domain="R-RcmdrPlugin.UCA")
    gettext("Input data and predict...", domain="R-RcmdrPlugin.UCA")
    doItAndPrint(paste0(".data <- edit(", ActiveDataSet(), "[0,])"))
    doItAndPrint(".data")
    doItAndPrint(paste0("predict(", ActiveModel(), ", .data)"))
    doItAndPrint("remove(.data)")
}


### Function to predict values for existing data set
predict4dataset <- function() {
    ## To ensure that menu name is included in pot file
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
        doItAndPrint(paste0(selection, "$fitted.", ActiveModel(), " <- predict(", ActiveModel(), ", ", selection, ")"))
        if (selection != .activeDataSet) activeDataSet(selection)
        tkfocus(CommanderWindow())
    }
    OKCancelHelp()
    tkgrid(getFrame(dataSetsBox), sticky="nw")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix()
}

### Function to be called by Rcmdr to test for randomness using runs.test from tseries packages
randomnessFTest <- function() {
    ## To ensure that menu name is included in pot file
    gettext("Randomness test for two level factor...", domain="R-RcmdrPlugin.UCA")
    ## Build dialog
    initializeDialog(title=gettext("Randomness test for two level factor", domain="R-RcmdrPlugin.UCA"))
    variablesBox <- variableListBox(top, TwoLevelFactors(), selectmode="single", initialSelection=NULL, title=gettextRcmdr("Variable (pick one)"))
    onOK <- function(){
        x <- getSelection(variablesBox)
        if (length(x) == 0) {
            errorCondition(recall=randomnessNTest, message=gettextRcmdr("No variables were selected."))
            return()
        }
        closeDialog()
        ## Apply test
        doItAndPrint(paste("with(", ActiveDataSet(), ", twolevelfactor.runs.test(", x, "))", sep = ""))
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject="Randomness test", reset = "randomnessFTest", apply = "randomnessFTest")
    tkgrid(getFrame(variablesBox), sticky="nw")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=6, columns=1)
}

### Function to be called by Rcmdr to test for randomness using runs.test from randtest packages
randomnessNTest <- function() {
    ## To ensure that menu name is included in pot file
    gettext("Randomness test for numeric variable...", domain="R-RcmdrPlugin.UCA")
    ## Build dialog
    initializeDialog(title=gettext("Randomness test for numeric variable", domain="R-RcmdrPlugin.UCA"))
    variablesBox <- variableListBox(top, Numeric(), selectmode="single", initialSelection=NULL, title=gettextRcmdr("Variable (pick one)"))
    onOK <- function(){
        x <- getSelection(variablesBox)
        if (length(x) == 0) {
            errorCondition(recall=randomnessNTest, message=gettextRcmdr("No variables were selected."))
            return()
        }
        closeDialog()
        ## Apply test
        doItAndPrint(paste("with(", ActiveDataSet(), ", numeric.runs.test(", x, "))", sep = ""))
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject="Randomness test", reset = "randomnessNTest", apply = "randomnessNTest")
    tkgrid(getFrame(variablesBox), sticky="nw")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=6, columns=1)
}

SampleSizeZMenu <- function() {
    ## Initialize dialog
    dialogName <- "SampleSizeZMenu"
    dialogTitle <- gettext("Normal mean with known variance", domain="R-RcmdrPlugin.UCA")
    defaults <- list(difference = "")
    dialog.values <- getDialog(dialogName, defaults=defaults)
    initializeDialog(title=dialogTitle)
    ## Define functions to handle buttons
    onOK <- function(){
        ##difference <- tclvalue(differenceVariable)
        closeDialog()
        tkfocus(CommanderWindow())
        ##difference <- tclvalue(difference)
        ##inputDialog(dialogName, list(difference = difference))
        doItAndPrint("-10:0")
        doItAndPrint("1:10")
    }
    OKCancelHelp(helpSubject="pwr.norm.test", reset=dialogName, apply=dialogName)
    ## Define tk objects
    entryFrame <- tkframe(top)
    differenceFrame <- tkframe(entryFrame)
    differenceVariable <- tclVar(dialog.values$difference)
    differenceField <- ttkentry(differenceFrame, width="6", textvariable = differenceVariable)
    ##tkgrid(entryFrame, sticky="w")
    ##tkgrid(labelRcmdr(differenceFrame, text = gettext("Difference to detect =", domain="R-RcmdrPlugin.UCA")), differenceField, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    ##dialogSuffix(columns=3)
    dialogSuffix()
}

sigmaTest <- function() {
    ## This function is developed from singleSampleTTest in Rcmdr and use function sigma.test in package TeachingDemos
    ## To ensure that menu name is included in pot file
    gettext("Single-Sample Variance Test...", domain="R-RcmdrPlugin.UCA")
    defaults <- list (initial.x = NULL, initial.alternative = "two.sided", initial.level = "0.95", initial.sigma = "1.0")
    dialog.values <- getDialog ("sigmaTest", defaults)
    initializeDialog(title = gettext("Single-Sample Variance Test", domain="R-RcmdrPlugin.UCA"))
    xBox <- variableListBox(top, Numeric(), title = gettextRcmdr("Variable (pick one)"), initialSelection = varPosn(dialog.values$initial.x, "numeric"))
    onOK <- function() {
        x <- getSelection(xBox)
        if (length(x) == 0) {
            errorCondition(recall = sigmaTest, message = gettextRcmdr("You must select a variable."))
            return()
        }
        alternative <- tclvalue(alternativeVariable)
        level <- tclvalue(confidenceLevel)
        sigma <- tclvalue(sigmaVariable)
        putDialog("sigmaTest", list (initial.x = x, initial.alternative = alternative, initial.level = level, initial.sigma = sigma))
        closeDialog()
        doItAndPrint(paste("with(", ActiveDataSet (), ", sigma.test(", x, "[!is.na(", x, ")], alternative='", alternative, "', sigmasq=", sigma, ", conf.level=", level, "))", sep = ""))
        tkdestroy(top)
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject = "sigma.test", reset = "sigmaTest", apply = "sigmaTest")
    optionsFrame <- tkframe(top)
    radioButtons(optionsFrame, name = "alternative", buttons = c("twosided", "less", "greater"), values = c("two.sided", "less", "greater"), labels = paste(gettext("Population variance", domain="R-RcmdrPlugin.UCA"), c("!= sigma0^2", "< sigma0^2", "> sigma0^2")), title = gettextRcmdr("Alternative Hypothesis"), initialValue = dialog.values$initial.alternative)
    rightFrame <- tkframe(optionsFrame)
    confidenceFrame <- tkframe(rightFrame)
    confidenceLevel <- tclVar(dialog.values$initial.level)
    confidenceField <- ttkentry(confidenceFrame, width = "6", textvariable = confidenceLevel)
    sigmaFrame <- tkframe(rightFrame)
    sigmaVariable <- tclVar(dialog.values$initial.sigma)
    sigmaField <- ttkentry(sigmaFrame, width = "8", textvariable = sigmaVariable)
    tkgrid(getFrame(xBox), sticky = "nw")
    tkgrid(labelRcmdr(rightFrame, text = ""), sticky = "w")
    tkgrid(labelRcmdr(sigmaFrame, text = gettext("Null hypothesis: sigma^2 =", domain="R-RcmdrPlugin.UCA")),  sigmaField, sticky = "w", padx=c(10, 0))
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

subset2Box <- function (window = top, subset.expression = NULL, model = FALSE) 
{
    tmp <- substitute({
        on.exit(remove(list = objects(pattern = "^\\.\\.", all.names = TRUE)))
        subset2Variable <- if (!is.null(subset.expression)) tclVar(gettextRcmdr(subset.expression)) else if (model) {
                                                                                                        if (currentModel && currentFields$subset != "") tclVar(currentFields$subset) else tclVar(gettextRcmdr("<all valid cases>"))
                                                                                                    } else tclVar(gettextRcmdr("<all valid cases>"))
        subset2Frame <- tkframe(window)
        subset2Entry <- ttkentry(subset2Frame, width = "20", textvariable = subset2Variable)
        subset2Scroll <- ttkscrollbar(subset2Frame, orient = "horizontal", command = function(...) tkxview(subset2Entry, ...))
        tkconfigure(subset2Entry, xscrollcommand = function(...) tkset(subset2Scroll, ...))
        tkgrid(labelRcmdr(subset2Frame, text = gettextRcmdr("Subset expression"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
        tkgrid(subset2Entry, sticky = "ew")
        tkgrid(subset2Scroll, sticky = "ew")
        tkgrid.columnconfigure(subset2Frame, 0, weight = 1)
    })
    eval(tmp, parent.frame())
}
