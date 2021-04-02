### Quality control extensions

types.withsamplesize <- c('c', 'np', 'p', 'u')
types.multicolumn <- c('R', 'S', 'xbar')

gettext("Phase I...", domain="R-RcmdrPlugin.UCA")
gettext("Phase I (multiple columns)...", domain="R-RcmdrPlugin.UCA")
gettext("Phase II (multiple columns)...", domain="R-RcmdrPlugin.UCA")

### Auxiliary function to all quality control for attributes
.qcc1Menu <- function(title, type, vartitle, help, recall, reset, apply) {
    initializeDialog(title=title)
    x1Box <- variableListBox(top, Numeric(), selectmode = ifelse(type %in% types.multicolumn, "single", "multiple"), initialSelection = NULL, title = vartitle)
    n1Box <- variableListBox(top, Numeric(), selectmode = "single", initialSelection = NULL, title = gettext("Sample size", domain="R-RcmdrPlugin.UCA"))
    subsetBox(subset.expression = gettextRcmdr("<all valid cases>"))
    onOK <- function(){
        x1 <- getSelection(x1Box)
        if (length(x1) == 0) {
            errorCondition(recall=recall, message=gettext("No data variable was selected", domain="R-RcmdrPlugin.UCA"))
            return()
        }
        if (length(x1) == 1 && type %in% types.multicolumn) {
            errorCondition(recall=recall, message=gettext("Select at least two variables (Phase I)", domain="R-RcmdrPlugin.UCA"))
            return()
        }
        if (type %in% types.withsamplesize) {
            n1 <- getSelection(n1Box)
            if (length(n1) == 0) {
                errorCondition(recall=recall, message=gettext("No sample size variable was selected", domain="R-RcmdrPlugin.UCA"))
                return()
            }
        }
        subset <- trim.blanks(tclvalue(subsetVariable))
        closeDialog()
        ## Plot graph
        command <- paste0("with(", ActiveDataSet(), ", qcc(data = ")
        command <- paste0(command, ifelse(type %in% types.multicolumn, paste0('cbind(', paste(x1, collapse = ','), ')'), x1))
        if ((subset != gettextRcmdr("<all valid cases>")) && (subset != "")) command <- paste0(command, '[', subset, ifelse(type %in% types.multicolumn, ', ', ''), ']')
        if (type %in% types.withsamplesize) {
            command <- paste0(command, ", sizes = ", n1)
            if ((subset != gettextRcmdr("<all valid cases>")) && (subset != "")) command <- paste0(command, '[', subset, ']')
        }
        command <- paste0(command, ", type = \"", type, "\"")
        if (type %in% types.multicolumn) command <- paste0(command, title = paste0(", title = \"", type, gettext(" Chart for ", domain="R-RcmdrPlugin.UCA"), "(", paste(x1, collapse = ', '), ")\""))
        command <- paste0(command, "))")
        doItAndPrint(command)
        tkdestroy(top)
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject = help, reset = reset, apply = apply)
    tkgrid(getFrame(x1Box), sticky="w")
    if (type %in% types.withsamplesize) {
        tkgrid(getFrame(n1Box), sticky="w")
    }
    tkgrid(subsetFrame, sticky = "w")
    tkgrid(buttonsFrame, sticky="e")
    dialogSuffix()
}

### Auxiliary function to all quality control for attributes
.qcc12Menu <- function(title, vartitle, type, help, recall, reset, apply) {
    initializeDialog(title=title)
    vartitle1 <- paste0(vartitle, " (", gettext("Phase I", domain="R-RcmdrPlugin.UCA"), ")")
    vartitle2 <- paste0(vartitle, " (", gettext("Phase II", domain="R-RcmdrPlugin.UCA"), ")")
    x1Box <- variableListBox(top, Numeric(), selectmode=ifelse(type %in% types.multicolumn, "single", "multiple"), initialSelection=NULL, title = vartitle1)
    n1Box <- variableListBox(top, Numeric(), selectmode="single", initialSelection=NULL, title=gettext("Sample size (Phase I)", domain="R-RcmdrPlugin.UCA"))
    subsetBox(subset.expression = gettextRcmdr("<all valid cases>"))
    x2Box <- variableListBox(top, Numeric(), selectmode=ifelse(type %in% types.multicolumn, "single", "multiple"), initialSelection=NULL, title = vartitle2)
    n2Box <- variableListBox(top, Numeric(), selectmode="single", initialSelection=NULL, title=gettext("Sample size (Phase II)", domain="R-RcmdrPlugin.UCA"))
    subset2Box(subset.expression = gettextRcmdr("<all valid cases>"))
    onOK <- function(){
        x1 <- getSelection(x1Box)
        if (length(x1) == 0) {
            errorCondition(recall = recall, message=gettext("No data variable was selected (Phase I)", domain="R-RcmdrPlugin.UCA"))
            return()
        }
        if (length(x1) == 1 && type %in% types.multicolumn) {
            errorCondition(recall=recall, message=gettext("Select at least two variables (Phase I)", domain="R-RcmdrPlugin.UCA"))
            return()
        }
        if (type %in% types.withsamplesize) {
            n1 <- getSelection(n1Box)
            if (length(n1) == 0) {
                errorCondition(recall = recall, message=gettext("No sample size variable was selected (Phase I)", domain="R-RcmdrPlugin.UCA"))
                return()
            }
        }
        x2 <- getSelection(x2Box)
        if (length(x2) == 0) {
            errorCondition(recall = recall, message=gettext("No data variable was selected (Phase II)", domain="R-RcmdrPlugin.UCA"))
            return()
        }
        if (length(x2) == 1 && type %in% types.multicolumn) {
            errorCondition(recall=recall, message=gettext("Select at least two variables (Phase II)", domain="R-RcmdrPlugin.UCA"))
            return()
        }
        if (type %in% types.withsamplesize) {
            n2 <- getSelection(n2Box)
            if (length(n2) == 0) {
                errorCondition(recall = recall, message=gettext("No sample size variable was selected (Phase II)", domain="R-RcmdrPlugin.UCA"))
                return()
            }
        }
        subset <- trim.blanks(tclvalue(subsetVariable))
        subset2 <- trim.blanks(tclvalue(subset2Variable))
        closeDialog()
        ## Plot graph
        command <- paste0("with(", ActiveDataSet(), ", qcc(data = ")
        command <- paste0(command, ifelse(type %in% types.multicolumn, paste0('cbind(', paste(x1, collapse = ','), ')'), x1))
        if ((subset != gettextRcmdr("<all valid cases>")) && (subset != "")) command <- paste0(command, '[', subset, ifelse(type %in% types.multicolumn, ', ', ''), ']')
        if (type %in% types.withsamplesize) {
            command <- paste0(command, ", sizes = ", n1)
            if ((subset != gettextRcmdr("<all valid cases>")) && (subset != "")) command <- paste0(command, '[', subset, ']')
        }
        command <- paste0(command, ", newdata = ")
        command <- paste0(command, ifelse(type %in% types.multicolumn, paste0('cbind(', paste(x2, collapse = ','), ')'), x2))
        if ((subset2 != gettextRcmdr("<all valid cases>")) && (subset2 != "")) command <- paste0(command, '[', subset2, ifelse(type %in% types.multicolumn, ', ', ''), ']')
        if (type %in% types.withsamplesize) {
            command <- paste0(command, ", newsizes = ", n2)
            if ((subset2 != gettextRcmdr("<all valid cases>")) && (subset2 != "")) command <- paste0(command, '[', subset2, ']')
        }
        command <- paste0(command, ", type = \"", type, "\"")
        if (type %in% types.multicolumn) command <- paste0(command, title = paste0(", title = \"", type, gettext(" Chart for ", domain="R-RcmdrPlugin.UCA"), "(", paste(x2, collapse = ', '), ")\""))
        command <- paste0(command, "))")
        doItAndPrint(command)
        tkdestroy(top)
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject = help, reset = reset, apply = apply)
    tkgrid(getFrame(x1Box), sticky = "w")
    if (type %in% types.withsamplesize) {
        tkgrid(getFrame(n1Box), sticky = "w")
    }
    tkgrid(subsetFrame, sticky = "w")
    tkgrid(getFrame(x2Box), sticky = "w")
    if (type %in% types.withsamplesize) {
        tkgrid(getFrame(n2Box), sticky = "w")
    }
    tkgrid(subset2Frame, sticky = "w")
    tkgrid(buttonsFrame, sticky="e")
    dialogSuffix(rows=6, columns=2)
}


### Auxiliary function to all quality control for attributes
.qcc2Menu <- function(title, type, vartitle, help, recall, reset, apply) {
    initializeDialog(title=title)
    vartitle1 <- paste0(vartitle, " (", gettext("Phase I", domain="R-RcmdrPlugin.UCA"), ")")
    vartitle2 <- paste0(vartitle, " (", gettext("Phase II", domain="R-RcmdrPlugin.UCA"), ")")
    x1Box <- variableListBox(top, Numeric(), selectmode="single", initialSelection=NULL, title = vartitle1)
    n1Box <- variableListBox(top, Numeric(), selectmode="single", initialSelection=NULL, title = gettext("Sample size (Phase I)", domain="R-RcmdrPlugin.UCA"))
    subsetBox(subset.expression = gettextRcmdr("<all valid cases>"))
    x2Box <- variableListBox(top, Numeric(), selectmode="single", initialSelection=NULL, title = vartitle2)
    n2Box <- variableListBox(top, Numeric(), selectmode="single", initialSelection=NULL, title = gettext("Sample size (Phase II)", domain="R-RcmdrPlugin.UCA"))
    subset2Box(subset.expression = gettextRcmdr("<all valid cases>"))
    centerVar <- tclVar("")
    centerEntry <- tkentry(top, width="8", textvariable=centerVar)
    ## stddevVar <- tclVar("")
    ## stddevEntry <- tkentry(top, width="8", textvariable=stddevVar)
    onOK <- function(){
        x1 <- getSelection(x1Box)
        if (length(x1) == 0) {
            errorCondition(recall=recall, message=gettext("No data variable was selected (Phase I)", domain="R-RcmdrPlugin.UCA"))
            return()
        }
        n1 <- getSelection(n1Box)
        if (length(n1) == 0) {
            errorCondition(recall=recall, message=gettext("No sample size variable was selected (Phase I)", domain="R-RcmdrPlugin.UCA"))
            return()
        }
        x2 <- getSelection(x2Box)
        if (length(x2) == 0) {
            errorCondition(recall=recall, message=gettext("No data variable was selected (Phase II)", domain="R-RcmdrPlugin.UCA"))
            return()
        }
        n2 <- getSelection(n2Box)
        if (length(n2) == 0) {
            errorCondition(recall=recall, message=gettext("No sample size variable was selected (Phase II)", domain="R-RcmdrPlugin.UCA"))
            return()
        }
        centerValue <- as.numeric(tclvalue(centerVar))
        if (is.na(centerValue)) {
            errorCondition(recall=recall, message=gettext("No value for center has been provided", domain="R-RcmdrPlugin.UCA"))
            return()
        }
        ## stddevValue <- as.numeric(tclvalue(stddevVar))
        ## if (is.na(stddevValue) || stddevValue <= 0) {
        ##    errorCondition(recall=recall, message=gettext("No positive value for standard deviation has been provided.", domain="R-RcmdrPlugin.UCA"))
        ##    return()
        ##    }
        subset <- trim.blanks(tclvalue(subsetVariable))
        subset2 <- trim.blanks(tclvalue(subset2Variable))
        closeDialog()
        ## Plot graph
        command <- paste0("with(", ActiveDataSet(), ", qcc(data = ", x1)
        if ((subset != gettextRcmdr("<all valid cases>")) && (subset != "")) command <- paste0(command, '[', subset, ']')
        command <- paste0(command, ", sizes = ", n1)
        if ((subset != gettextRcmdr("<all valid cases>")) && (subset != "")) command <- paste0(command, '[', subset, ']')
        command <- paste0(command, ", newdata = ", x2)
        if ((subset2 != gettextRcmdr("<all valid cases>")) && (subset2 != "")) command <- paste0(command, '[', subset2, ']')
        command <- paste0(command, ", newsizes = ", n2)
        if ((subset2 != gettextRcmdr("<all valid cases>")) && (subset2 != "")) command <- paste0(command, '[', subset2, ']')
        command <- paste0(command, ", type = \"", type, "\"")
        command <- paste0(command, ", center = ", centerValue)
        ##command <- paste0(command, ", std.dev = ", stddevValue)
        ##command <- paste0(command, ", limits = c(", centerValue -3 * stddevValue, ", ", centerValue + 3 * stddevValue, ")")
        command <- paste0(command, "))")
        doItAndPrint(command)
        tkdestroy(top)
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject = help, reset = reset, apply = apply)

    tkgrid(tklabel(top, text = gettext("Center", domain="R-RcmdrPlugin.UCA")), centerEntry, sticky = "e")
    ##tkgrid(tklabel(top, text = gettext("Standard Deviation", domain="R-RcmdrPlugin.UCA")), stddevEntry, sticky = "e")
    tkgrid(getFrame(x1Box), sticky = "w")
    tkgrid(getFrame(n1Box), sticky = "w")
    tkgrid(subsetFrame, sticky = "w")
    tkgrid(getFrame(x2Box), sticky = "w")
    tkgrid(getFrame(n2Box), sticky = "w")
    tkgrid(subset2Frame, sticky = "w")
    tkgrid(buttonsFrame, sticky="e")
    tkgrid.configure(centerEntry, sticky = "w")
    ##tkgrid.configure(stddevEntry, sticky = "w")
    dialogSuffix(rows=6, columns=2)
}


### Function to produce a phase-I c-chart using cchart.p function in qcc package
cchart1Menu <- function()
{
    ## To ensure that menu name is included in pot file
    gettext("Counts", domain="R-RcmdrPlugin.UCA")
    gettext("c-chart (rate)...", domain="R-RcmdrPlugin.UCA")
    .qcc1Menu(
        title = gettext("c-chart (rate)", domain="R-RcmdrPlugin.UCA"),
        vartitle = gettext("Nonconformities", domain="R-RcmdrPlugin.UCA"),
        type = "c", help = "c-chart", recall = cchart1Menu, reset = "cchart1Menu", apply = "cchart1Menu")
}

### Function to produce a phase-II c-chart using cchart.p function in qcc package
cchart12Menu <- function()
{
    ## To ensure that menu name is included in pot file
    gettext("Counts", domain="R-RcmdrPlugin.UCA")
    gettext("c-chart (rate)...", domain="R-RcmdrPlugin.UCA")
    .qcc12Menu(
        title = gettext("c-chart (rate)", domain="R-RcmdrPlugin.UCA"),
        vartitle = gettext("Nonconformities", domain="R-RcmdrPlugin.UCA"),
        type = "c", help = "c-chart", recall = cchart12Menu, reset = "cchart12Menu", apply = "cchart12Menu")
}

### Function to produce a phase-II c-chart using cchart.p function in qcc package
cchart2Menu <- function()
{
    ## To ensure that menu name is included in pot file
    gettext("Counts", domain="R-RcmdrPlugin.UCA")
    gettext("c-chart (rate)...", domain="R-RcmdrPlugin.UCA")
    .qcc2Menu(
        title = gettext("c-chart (rate)", domain="R-RcmdrPlugin.UCA"),
        vartitle = gettext("Nonconformities", domain="R-RcmdrPlugin.UCA"),
        type = "c", help = "c-chart", recall = cchart2Menu, reset = "cchart2Menu", apply = "cchart2Menu")
}

### Function to produce a phase-I np-chart using cchart.p function in qcc package
npchart1Menu <- function()
{
    ## To ensure that menu name is included in pot file
    gettext("Attributes", domain="R-RcmdrPlugin.UCA")
    gettext("np-chart (count)", domain="R-RcmdrPlugin.UCA")
    gettext("np-chart (Phase I)...", domain="R-RcmdrPlugin.UCA")
    .qcc1Menu(
        title = gettext("np-chart (Phase I)", domain="R-RcmdrPlugin.UCA"),
        vartitle = gettext("Nonconforming units", domain="R-RcmdrPlugin.UCA"),
        type = "np", help = "p-chart", recall = npchart1Menu, reset = "npchart1Menu", apply = "npchart1Menu")
}

### Function to produce a phase-II p-chart using cchart.p function in qcc package
npchart12Menu <- function()
{
    .qcc12Menu(
        title = gettext("Phase II from data...", domain="R-RcmdrPlugin.UCA"),
        vartitle = gettext("Nonconforming units", domain="R-RcmdrPlugin.UCA"),
        type = "np", help = "np-chart",  recall = npchart12Menu, reset = "npchart12Menu", apply = "npchart12Menu")
}


### Function to produce a phase-I np-chart using cchart.p function in qcc package
npchart2Menu <- function()
{
    .qcc2Menu(
        title = gettext("Phase II from parameters...", domain="R-RcmdrPlugin.UCA"),
        vartitle = gettext("Nonconforming units", domain="R-RcmdrPlugin.UCA"),
        type = "np", help = "p-chart", recall = npchart2Menu, reset = "npchart2Menu", apply = "npchart2Menu")
}

### Function to produce a Pareto char using paretochar function in qicharts2 package
paretochartMenu <- function() {
    ## To ensure that menu name is included in pot file
    gettext("Quality Control", domain="R-RcmdrPlugin.UCA")
    initializeDialog(title=gettext("Pareto chart", domain="R-RcmdrPlugin.UCA"))
    variablesBox <- variableListBox(top, Factors(), selectmode="single", initialSelection=NULL, title=gettextRcmdr("Variable"))
    onOK <- function() {
        x <- getSelection(variablesBox)
        if (length(x) == 0) {
            errorCondition(recall=paretochartMenu, message=gettextRcmdr("No variable was selected."))
            return()
        }
        closeDialog()
        ## Apply test
        doItAndPrint(paste("with(", ActiveDataSet(), ", paretochart(", x, "))", sep = ""))
        tkdestroy(top)
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject="paretochart", reset = "paretochartMenu", apply = "paretochartMenu")
    tkgrid(getFrame(variablesBox), sticky="nw")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=6, columns=1)
}

### Function to produce a phase-I p-chart using cchart.p function in qcc package
pchart1Menu <- function()
{
    ## To ensure that menu name is included in pot file
    gettext("Attributes", domain="R-RcmdrPlugin.UCA")
    gettext("p-chart (proportion)", domain="R-RcmdrPlugin.UCA")
    gettext("p-chart (Phase I)...", domain="R-RcmdrPlugin.UCA")
    .qcc1Menu(
        title = gettext("p-chart (Phase I)", domain="R-RcmdrPlugin.UCA"),
        vartitle = gettext("Nonconforming units", domain="R-RcmdrPlugin.UCA"),
        type = "p", help = "p-chart", recall = pchart1Menu, reset = "pchart1Menu", apply = "pchart1Menu")
}

### Function to produce a phase-II p-chart using cchart.p function in qcc package
pchart12Menu <- function()
{
    .qcc12Menu(
        title = gettext("Phase II from data...", domain="R-RcmdrPlugin.UCA"),
        vartitle = gettext("Nonconforming units", domain="R-RcmdrPlugin.UCA"),
        type = "p", help = "p-chart",  recall = pchart12Menu, reset = "pchart12Menu", apply = "pchart12Menu")
}

### Function to produce a phase-I p-chart using cchart.p function in qcc package
pchart2Menu <- function()
{
    .qcc2Menu(
        title = gettext("Phase II from parameters...", domain="R-RcmdrPlugin.UCA"),
        vartitle = gettext("Nonconforming units", domain="R-RcmdrPlugin.UCA"),
        type = "p", help = "p-chart", recall = pchart2Menu, reset = "pchart2Menu", apply = "pchart2Menu")
}

R1mcMenu <- function()
{
    .qcc1Menu(
        title = gettext("R Chart Phase I (multiple columns)", domain="R-RcmdrPlugin.UCA"),
        vartitle = gettext("Measurements (pick two variables or more)", domain="R-RcmdrPlugin.UCA"),
        type = "R", help = "R-chart", recall = R1mcMenu, reset = "R1mcMenu", apply = "R1mcMenu")
}

R12mcMenu <- function()
{
    .qcc12Menu(
        title = gettext("R Chart Phase II (multiple columns)", domain="R-RcmdrPlugin.UCA"),
        vartitle = gettext("Measurements (pick two variables or more)", domain="R-RcmdrPlugin.UCA"),
        type = "R", help = "R-chart", recall = R12mcMenu, reset = "R12mcMenu", apply = "R12mcMenu")
}

S1mcMenu <- function()
{
    .qcc1Menu(
        title = gettext("S Chart Phase I (multiple columns)", domain="R-RcmdrPlugin.UCA"),
        vartitle = gettext("Measurements (pick two variables or more)", domain="R-RcmdrPlugin.UCA"),
        type = "S", help = "S-chart", recall = S1mcMenu, reset = "S1mcMenu", apply = "S1mcMenu")
}

S12mcMenu <- function()
{
    .qcc12Menu(
        title = gettext("S Chart Phase II (multiple columns)", domain="R-RcmdrPlugin.UCA"),
        vartitle = gettext("Measurements (pick two variables or more)", domain="R-RcmdrPlugin.UCA"),
        type = "S", help = "S-chart", recall = S12mcMenu, reset = "S12mcMenu", apply = "S12mcMenu")
}

### Function to produce a phase-I u-chart using cchart.p function in qcc package
uchart1Menu <- function()
{
    ## To ensure that menu name is included in pot file
    gettext("Counts", domain="R-RcmdrPlugin.UCA")
    gettext("u-chart (average rate)...", domain="R-RcmdrPlugin.UCA")
    .qcc1Menu(
        title = gettext("u-chart (average rate)", domain="R-RcmdrPlugin.UCA"),
        vartitle = gettext("Nonconformities", domain="R-RcmdrPlugin.UCA"),
        type = "u", help = "u-chart", recall = uchart1Menu, reset = "uchart1Menu", apply = "uchart1Menu")
}


### Function to produce a phase-I u-chart using cchart.p function in qcc package
uchart12Menu <- function()
{
    ## To ensure that menu name is included in pot file
    gettext("Counts", domain="R-RcmdrPlugin.UCA")
    gettext("u-chart (average rate)...", domain="R-RcmdrPlugin.UCA")
    .qcc12Menu(
        title = gettext("u-chart (average rate)", domain="R-RcmdrPlugin.UCA"),
        vartitle = gettext("Nonconformities", domain="R-RcmdrPlugin.UCA"),
        type = "u", help = "u-chart", recall = uchart12Menu, reset = "uchart12Menu", apply = "uchart12Menu")
}

### Function to produce a phase-I u-chart using cchart.p function in qcc package
uchart2Menu <- function()
{
    ## To ensure that menu name is included in pot file
    gettext("Counts", domain="R-RcmdrPlugin.UCA")
    gettext("u-chart (average rate)...", domain="R-RcmdrPlugin.UCA")
    .qcc2Menu(
        title = gettext("u-chart (average rate)", domain="R-RcmdrPlugin.UCA"),
        vartitle = gettext("Nonconformities", domain="R-RcmdrPlugin.UCA"),
        type = "u", help = "u-chart", recall = uchart2Menu, reset = "uchart2Menu", apply = "uchart2Menu")
}

xbarone1Menu <- function()
{
    gettext("Continuous", domain="R-RcmdrPlugin.UCA")
    gettext("One-at-time data", domain="R-RcmdrPlugin.UCA")
    .qcc1Menu(
        title = gettext("One-at-time data", domain="R-RcmdrPlugin.UCA"),
        vartitle = gettext("Measurements", domain="R-RcmdrPlugin.UCA"),
        type = "xbar.one", help = "xbar.one-chart", recall = xbarone1Menu, reset = "xbarone1Menu", apply = "xbarone1Menu")
}

xbar1mcMenu <- function()
{
    .qcc1Menu(
        title = gettext("xbar Chart Phase I (multiple columns)", domain="R-RcmdrPlugin.UCA"),
        vartitle = gettext("Measurements (pick two variables or more)", domain="R-RcmdrPlugin.UCA"),
        type = "xbar", help = "xbar-chart", recall = xbar1mcMenu, reset = "xbar1mcMenu", apply = "xbar1mcMenu")
}

xbar12mcMenu <- function()
{
    .qcc12Menu(
        title = gettext("xbar Chart Phase II (multiple columns)", domain="R-RcmdrPlugin.UCA"),
        vartitle = gettext("Measurements (pick two variables or more)", domain="R-RcmdrPlugin.UCA"),
        type = "xbar", help = "xbar-chart", recall = xbar12mcMenu, reset = "xbar12mcMenu", apply = "xbar12mcMenu")
}
