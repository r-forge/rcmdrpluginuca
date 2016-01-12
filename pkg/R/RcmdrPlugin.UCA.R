# Some Rcmdr dialogs for the orloca package (non graphical functions)

.onAttach <- function(libname, pkgname){
    if (!interactive()) return()
    Rcmdr <- options()$Rcmdr
    plugins <- Rcmdr$plugins
    # Load required packages
    if ((!pkgname %in% plugins) && !getRcmdr("autoRestart")) {
        Rcmdr$plugins <- c(plugins, pkgname)
        options(Rcmdr=Rcmdr)
        closeCommander(ask=FALSE, ask.save=TRUE)
        Commander()
        }
    }

# Function to be called by Rcmdr to drop unused levels in active data set
Rcmdr.droplevels <- function()
  {
  # To ensure that menu name is included in pot file
  gettext("Drop unused factor levels", domain="R-RcmdrPlugin.UCA")
  ### Base function subsetDataSet <- function(){
  dataSet <- activeDataSet()
  ### initializeDialog(title=gettextRcmdr("Subset Data Set"))
  initializeDialog(title=gettext("Drop unused factor levels", domain="R-RcmdrPlugin.UCA"))
  allVariablesFrame <- tkframe(top)
	allVariables <- tclVar("1")
	allVariablesCheckBox <- tkcheckbutton(allVariablesFrame, variable=allVariables)
	variablesBox <- variableListBox(top, Factors(), selectmode="multiple",
		initialSelection=NULL, title=gettextRcmdr("Variables (select one or more)"))
	subsetVariable <- tclVar(gettextRcmdr("<all cases>"))
	subsetFrame <- tkframe(top)
	newDataSetName <- tclVar(gettextRcmdr("<same as active data set>"))
	dataSetNameFrame <- tkframe(top)
	dataSetNameEntry <- ttkentry(dataSetNameFrame, width="25", textvariable=newDataSetName)
	onOK <- function(){
		newName <- trim.blanks(tclvalue(newDataSetName))
		if (newName == gettextRcmdr("<same as active data set>")) newName <- ActiveDataSet()
		if (!is.valid.name(newName)){
			errorCondition(recall=Rcmdr.droplevels,
				message=paste('"', newName, '" ', gettextRcmdr("is not a valid name."), sep=""))
			return()
		}
		if (is.element(newName, listDataSets())) {
			if ("no" == tclvalue(checkReplace(newName, type=gettextRcmdr("Data set")))){
				closeDialog()
				return()
			}
		}
		selectVars <- if (tclvalue(allVariables) == "1") ""
			else {
				x <- getSelection(variablesBox)
				if (0 > length(x)) {
					errorCondition(recall=Rcmdr.droplevels,
						message=gettextRcmdr("No variables were selected."))
					return()
				}
				paste("[, c(\"", paste(x, collapse="\",\""), "\")]", sep="")
			}
		closeDialog()
                # If new name is given, it is neccesary to create the new data set befor filter
                if (!identical(newName, ActiveDataSet())){
                  command <- paste(newName, " <- ", ActiveDataSet(), sep="")
                  logger(command)
                  justDoIt(command)
                }
                command <- paste(newName, selectVars, " <- droplevels(", ActiveDataSet(), selectVars, ")",sep="")
		logger(command)
		result <- justDoIt(command)
		if (class(result)[1] !=  "try-error") activeDataSet(newName)
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject="subset")
	tkgrid(labelRcmdr(allVariablesFrame, text=gettextRcmdr("Include all variables")),
		allVariablesCheckBox, sticky="w")
	tkgrid(allVariablesFrame, sticky="w")
	tkgrid(labelRcmdr(top, text=gettextRcmdr("   OR"), fg="red"), sticky="w")
	tkgrid(getFrame(variablesBox), sticky="nw")
	tkgrid(subsetFrame, sticky="w")
	tkgrid(labelRcmdr(dataSetNameFrame, text=gettextRcmdr("Name for new data set")), sticky="w")
	tkgrid(dataSetNameEntry, sticky="w")
	tkgrid(dataSetNameFrame, sticky="w")
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=6, columns=1)
}

