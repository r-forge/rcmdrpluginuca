### Quality control extensions

types.withsamplesize <- c('c', 'np', 'p', 'u')
types.multicolumn <- c('R', 'S', 'xbar')

gettext("Phase I...", domain="R-RcmdrPlugin.UCA")
gettext("Phase I (multiple columns)...", domain="R-RcmdrPlugin.UCA")
gettext("Phase II (multiple columns)...", domain="R-RcmdrPlugin.UCA")
gettext("Phase II from data (multiple columns)...", domain="R-RcmdrPlugin.UCA")
gettext("Phase II from parameters (multiple columns)...", domain="R-RcmdrPlugin.UCA")

### Auxiliary function to all quality control for attributes
.qccMenu <- function(dialogtitle, graphtitle, x1title = "", n1title = "", x2title = "", n2title = "", type, phase = c('1', '2', 'p'), help, recall, reset, apply) {
    phase = match.arg(phase)
    initializeDialog(title = dialogtitle)
    ## Parameters
    if (phase == 'p') {
        centerVar <- tclVar("")
        dataFrame <- tkframe(top)
        centerEntry <- tkentry(dataFrame, width="8", textvariable=centerVar)
        if (type %in% types.multicolumn) {
            stddevVar <- tclVar("")
            stddevEntry <- tkentry(top, width="8", textvariable=stddevVar)
        }
    }
    ## Phase I
    x1Box <- variableListBox(top, Numeric(), selectmode=ifelse(type %in% types.multicolumn, "single", "multiple"), initialSelection=NULL, title = x1title)
    n1Box <- variableListBox(top, Numeric(), selectmode="single", initialSelection=NULL, title = n1title)
    subsetBox(subset.expression = gettextRcmdr("<all valid cases>"))
    ## Phase II
    if (phase == '2' || phase == 'p') {
        x2Box <- variableListBox(top, Numeric(), selectmode=ifelse(type %in% types.multicolumn, "single", "multiple"), initialSelection=NULL, title = x2title)
        n2Box <- variableListBox(top, Numeric(), selectmode="single", initialSelection=NULL, title = n2title)
        subset2Box(subset.expression = gettextRcmdr("<all valid cases>"))
        ## Options buttons
        plotall <- tclVar()
        renumber <- tclVar()
        optionsFrame <- tkframe(top)
        plotallcheckBox <- ttkcheckbutton(optionsFrame, variable = plotall)
        renumbercheckBox <- ttkcheckbutton(optionsFrame, variable = renumber)
    }
    onOK <- function() {
        ## Setup and get options values
        if (phase == '2' || phase == 'p') {
            plotall <- (tclvalue(plotall) == "1")
            renumber <- (tclvalue(renumber) == "1")
        } else {
            plotall <- FALSE
            renumber <- FALSE
        }
        ##
        ## Read data
        ##
        ## Read parameters
        if (phase == 'p') {
            center <- as.numeric(tclvalue(centerVar))
            if (is.na(center)) {
                errorCondition(recall = recall, message=gettext("No valid numeric value has been provided for the center parameter", domain="R-RcmdrPlugin.UCA"))
                return()
            }
            if (type %in% types.multicolumn) {
                stddev <- as.numeric(tclvalue(stddevVar))
            }
        }
        ## Read phase I data
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
        subset <- trim.blanks(tclvalue(subsetVariable))
        ## Read phase II data
        if (phase == '2' || phase == 'p') {
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
            subset2 <- trim.blanks(tclvalue(subset2Variable))
        }
        closeDialog()
        ##
        ## Build title
        ##
        if (phase == '1' || plotall) {
            graphtitle <- paste0(graphtitle, ' ', ifelse(type %in% types.multicolumn, paste0('(', paste(x1, collapse = ','), ')'), x1))
            if ((subset != gettextRcmdr("<all valid cases>")) && (subset != "")) graphtitle <- paste0(graphtitle, '[', subset, ifelse(type %in% types.multicolumn, ', ', ''), ']')
        }
        if (phase == '2' || phase == 'p') {
            if (plotall) graphtitle <- paste0(graphtitle, ' ', gettext('and', domain = "R-RcmdrPlugin.UCA"))
            graphtitle <- paste0(graphtitle, ' ', ifelse(type %in% types.multicolumn, paste0('(', paste(x2, collapse = ','), ')'), x2))
            if ((subset2 != gettextRcmdr("<all valid cases>")) && (subset2 != "")) graphtitle <- paste0(graphtitle, '[', subset2, ifelse(type %in% types.multicolumn, ', ', ''), ']')
        }
        if (phase == 'p') {
            graphtitle <- paste0(graphtitle, '\\n', gettext('with Center', domain = "R-RcmdrPlugin.UCA"), ' ', center)
        }
        if (phase == 'p' && type %in% types.multicolumn && !is.na(stddev)) {
            graphtitle <- paste0(graphtitle, ' ', gettext('and StdDev', domain = "R-RcmdrPlugin.UCA"), ' ', stddev)
        }
        ##
        ## Build command
        ##
        command <- paste0("with(", ActiveDataSet(), ", qcc(data = ")
        command <- paste0(command, ifelse(type %in% types.multicolumn, paste0('cbind(', paste(x1, collapse = ','), ')'), x1))
        if ((subset != gettextRcmdr("<all valid cases>")) && (subset != "")) command <- paste0(command, '[', subset, ifelse(type %in% types.multicolumn, ', ', ''), ']') 
        if (type %in% types.withsamplesize) {
            command <- paste0(command, ", sizes = ", n1)
            if ((subset != gettextRcmdr("<all valid cases>")) && (subset != "")) {
                command <- paste0(command, '[', subset, ']')
            }
        }
        if (phase == '2' || phase == 'p') {
            command <- paste0(command, ", newdata = ")
            command <- paste0(command, ifelse(type %in% types.multicolumn, paste0('cbind(', paste(x2, collapse = ','), ')'), x2))
            if ((subset2 != gettextRcmdr("<all valid cases>")) && (subset2 != "")) command <- paste0(command, '[', subset2, ifelse(type %in% types.multicolumn, ', ', ''), ']')
            if (type %in% types.withsamplesize) {
                command <- paste0(command, ", newsizes = ", n2)
                if ((subset2 != gettextRcmdr("<all valid cases>")) && (subset2 != "")) {
                    command <- paste0(command, '[', subset2, ']')
                }
            }
        }
        command <- paste0(command, ", type = \"", type, "\"")
        if (phase == 'p') command <- paste0(command, ", center = ", center)
        if (phase == 'p' && type %in% types.multicolumn && !is.na(stddev)) {
            command <- paste0(command, ", std.dev =", stddev)
        }
        command <- paste0(command, ", title = \"", graphtitle, "\"")
        if (phase == '2' || phase == 'p') {
            if (!plotall) command <- paste0(command, ", chart.all = ", plotall)
            ## Compute length of x2
            if (renumber) {
                lx2 <- paste0("with(", ActiveDataSet())
                if (type %in% types.multicolumn) {
                    lx2 <- paste0(lx2, ", nrow(")
                } else {
                    lx2 <- paste0(lx2, ", length(")
                }
                lx2 <- paste0(lx2, ifelse(type %in% types.multicolumn, paste0('cbind(', paste(x2, collapse = ','), ')'), x2))
                if ((subset2 != gettextRcmdr("<all valid cases>")) && (subset2 != "")) lx2 <- paste0(lx2, '[', subset2, ifelse(type %in% types.multicolumn, ', ', ''), ']')
                lx2 <- paste0(lx2, "))")
                lx2 <- eval(parse(text = lx2))
                command <- paste0(command, ", newlabel = 1:", lx2)
            }
        }
        command <- paste0(command, "))")
        doItAndPrint(command)
        tkdestroy(top)
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject = help, reset = reset, apply = apply)
    deltarow <- 0
    ## Parameters
    if (phase == 'p') {
        tkgrid(tklabel(dataFrame, text = paste0(gettext("Center", domain="R-RcmdrPlugin.UCA"), " ")), centerEntry, sticky = "w")
        ## tkgrid(centerEntry, sticky = "w")
        deltarow <- 1
        if (type %in% types.multicolumn) {
            tkgrid(tklabel(dataFrame, text = gettext("Standard Deviation", domain="R-RcmdrPlugin.UCA")), stddevEntry, sticky = "w")
            deltarow <- deltarow + 1
        }
        tkgrid(dataFrame, sticky = "w")
    }
    
    ## Phase I
    tkgrid(getFrame(x1Box), sticky = "w")
    if (type %in% types.withsamplesize) {
        tkgrid(getFrame(n1Box), sticky = "w", row = 0 + deltarow, column = 1)
    }
    tkgrid(subsetFrame, sticky = "w")
    ## Phase II
    if (phase == '2' || phase == 'p') {
        tkgrid(getFrame(x2Box), sticky = "w")
        if (type %in% types.withsamplesize) {
            tkgrid(getFrame(n2Box), sticky = "w", row = 2 + deltarow, column = 1)
        }
        tkgrid(subset2Frame, sticky = "w")
        ## type <- tclvalue(typeVariable)
        tkgrid(plotallcheckBox, tklabel(optionsFrame, text = gettext("Plot all data", domain="R-RcmdrPlugin.UCA")), sticky = "w")
        tkgrid(renumbercheckBox, tklabel(optionsFrame, text = gettext("Renumber 2nd phase data", domain="R-RcmdrPlugin.UCA")), sticky = "w")
        tkgrid(optionsFrame, sticky = "w")
    }
    tkgrid(buttonsFrame, sticky="w", columnspan = 2)
    dialogSuffix()
}

### Function to produce a phase-I c-chart using cchart.p function in qcc package
cchart1Menu <- function()
{
    ## To ensure that menu name is included in pot file
    gettext("Counts", domain="R-RcmdrPlugin.UCA")
    gettext("c-chart (rate)...", domain="R-RcmdrPlugin.UCA")
    .qccMenu(
        dialogtitle = gettext("c-chart (rate)", domain="R-RcmdrPlugin.UCA"),
        graphtitle = gettext("c-chart\\nfor", domain="R-RcmdrPlugin.UCA"),
        x1title = gettext("Nonconformities", domain="R-RcmdrPlugin.UCA"),
        n1title = gettext("Sizes", domain="R-RcmdrPlugin.UCA"),
        type = "c", phase = '1', help = "c-chart", recall = cchart1Menu, reset = "cchart1Menu", apply = "cchart1Menu")
}

### Function to produce a phase-II c-chart using cchart.p function in qcc package
cchart2Menu <- function()
{
    ## To ensure that menu name is included in pot file
    gettext("Counts", domain="R-RcmdrPlugin.UCA")
    gettext("c-chart (rate)...", domain="R-RcmdrPlugin.UCA")
    .qccMenu(
        dialogtitle = gettext("c-chart (rate) from data", domain="R-RcmdrPlugin.UCA"),
        graphtitle = gettext("c-chart\\nfor", domain="R-RcmdrPlugin.UCA"),
        x1title = gettext("Nonconformities (Phase I)", domain="R-RcmdrPlugin.UCA"),
        n1title = gettext("Sizes (Phase I)", domain="R-RcmdrPlugin.UCA"),
        x2title = gettext("Nonconformities (Phase II)", domain="R-RcmdrPlugin.UCA"),
        n2title = gettext("Sizes (Phase II)", domain="R-RcmdrPlugin.UCA"),
        type = "c", phase = "2", help = "c-chart", recall = cchart2Menu, reset = "cchart2Menu", apply = "cchart2Menu")
}

### Function to produce a phase-II c-chart using cchart.p function in qcc package
cchartpMenu <- function()
{
    ## To ensure that menu name is included in pot file
    gettext("Counts", domain="R-RcmdrPlugin.UCA")
    gettext("c-chart (rate)...", domain="R-RcmdrPlugin.UCA")
    .qccMenu(
        dialogtitle = gettext("c-chart (rate) from parameter", domain="R-RcmdrPlugin.UCA"),
        graphtitle = gettext("c-chart\\nfor", domain="R-RcmdrPlugin.UCA"),
        x1title = gettext("Nonconformities (Phase I)", domain="R-RcmdrPlugin.UCA"),
        n1title = gettext("Sizes (Phase I)", domain="R-RcmdrPlugin.UCA"),
        x2title = gettext("Nonconformities (Phase II)", domain="R-RcmdrPlugin.UCA"),
        n2title = gettext("Sizes (Phase II)", domain="R-RcmdrPlugin.UCA"),
        type = "c", phase = "p", help = "c-chart", recall = cchartpMenu, reset = "cchartpMenu", apply = "cchartpMenu")
}

### Function to produce a phase-I np-chart using cchart.p function in qcc package
npchart1Menu <- function()
{
    ## To ensure that menu name is included in pot file
    gettext("Attributes", domain="R-RcmdrPlugin.UCA")
    gettext("np-chart (count)", domain="R-RcmdrPlugin.UCA")
    gettext("np-chart (Phase I)...", domain="R-RcmdrPlugin.UCA")
    .qccMenu(
        dialogtitle = gettext("np-chart", domain="R-RcmdrPlugin.UCA"),
        graphtitle = gettext("np-chart\\nfor", domain="R-RcmdrPlugin.UCA"),
        x1title = gettext("Nonconforming units", domain="R-RcmdrPlugin.UCA"),
        n1title = gettext("Sample size", domain="R-RcmdrPlugin.UCA"),
        type = "np", phase = "1", help = "np-chart", recall = npchart1Menu, reset = "npchart1Menu", apply = "npchart1Menu")
}

### Function to produce a phase-II p-chart using cchart.p function in qcc package
npchart2Menu <- function()
{
    .qccMenu(
        dialogtitle = gettext("np-chart phase II from data", domain="R-RcmdrPlugin.UCA"),
        graphtitle = gettext("np-chart phase II\\nfor", domain="R-RcmdrPlugin.UCA"),
        x1title = gettext("Nonconforming units (Phase I)", domain="R-RcmdrPlugin.UCA"),
        n1title = gettext("Sample size (Phase I)", domain="R-RcmdrPlugin.UCA"),
        x2title = gettext("Nonconforming units (Phase II)", domain="R-RcmdrPlugin.UCA"),
        n2title = gettext("Sample size (Phase II)", domain="R-RcmdrPlugin.UCA"),
        type = "np", phase = "2", help = "np-chart",  recall = npchart2Menu, reset = "npchart2Menu", apply = "npchart2Menu")
}


### Function to produce a phase-I np-chart using cchart.p function in qcc package
npchartpMenu <- function()
{
    .qccMenu(
        dialogtitle = gettext("np-chart phase II from parameter", domain="R-RcmdrPlugin.UCA"),
        graphtitle = gettext("np-chart phase II\\nfor", domain="R-RcmdrPlugin.UCA"),
        x1title = gettext("Nonconforming units (Phase I)", domain="R-RcmdrPlugin.UCA"),
        n1title = gettext("Sample size (Phase I)", domain="R-RcmdrPlugin.UCA"),
        x2title = gettext("Nonconforming units (Phase II)", domain="R-RcmdrPlugin.UCA"),
        n2title = gettext("Sample size (Phase II)", domain="R-RcmdrPlugin.UCA"),
        type = "np", phase = "p", help = "np-chart", recall = npchartpMenu, reset = "npchartpMenu", apply = "npchartpMenu")
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
    gettext("Phase I...", domain="R-RcmdrPlugin.UCA")
    .qccMenu(
        dialogtitle = gettext("p-chart", domain="R-RcmdrPlugin.UCA"),
        graphtitle = gettext("p-chart\\nfor", domain="R-RcmdrPlugin.UCA"),
        x1title = gettext("Nonconforming units", domain="R-RcmdrPlugin.UCA"),
        n1title = gettext("Sample size", domain="R-RcmdrPlugin.UCA"),
        type = "p", phase = "1", help = "p-chart", recall = pchart1Menu, reset = "pchart1Menu", apply = "pchart1Menu")
}

### Function to produce a phase-II p-chart using cchart.p function in qcc package
pchart2Menu <- function()
{
    gettext("Phase II from data...", domain="R-RcmdrPlugin.UCA")
    .qccMenu(
        dialogtitle = gettext("p-chart phase II from data", domain="R-RcmdrPlugin.UCA"),
        graphtitle = gettext("p-chart phase II\\nfor", domain="R-RcmdrPlugin.UCA"),
        x1title = gettext("Nonconforming units (Phase I)", domain="R-RcmdrPlugin.UCA"),
        n1title = gettext("Sample size (Phase I)", domain="R-RcmdrPlugin.UCA"),
        x2title = gettext("Nonconforming units (Phase II)", domain="R-RcmdrPlugin.UCA"),
        n2title = gettext("Sample size (Phase II)", domain="R-RcmdrPlugin.UCA"),
        type = "p", phase = "2", help = "p-chart",  recall = pchart2Menu, reset = "pchart2Menu", apply = "pchart2Menu")
}

### Function to produce a phase-I p-chart using cchart.p function in qcc package
pchartpMenu <- function()
{
    gettext("Phase II from parameter...", domain="R-RcmdrPlugin.UCA")
    .qccMenu(
        dialogtitle = gettext("p-chart phase II from parameter", domain="R-RcmdrPlugin.UCA"),
        graphtitle = gettext("p-chart phase II\\nfor", domain="R-RcmdrPlugin.UCA"),
        x1title = gettext("Nonconforming units (Phase I)", domain="R-RcmdrPlugin.UCA"),
        n1title = gettext("Sample size (Phase I)", domain="R-RcmdrPlugin.UCA"),
        x2title = gettext("Nonconforming units (Phase II)", domain="R-RcmdrPlugin.UCA"),
        n2title = gettext("Sample size (Phase II)", domain="R-RcmdrPlugin.UCA"),
        type = "p", phase = "p", help = "p-chart", recall = pchartpMenu, reset = "pchartpMenu", apply = "pchartpMenu")
}

R1mcMenu <- function()
{
    gettext("Range", domain="R-RcmdrPlugin.UCA")
    .qccMenu(
        dialogtitle = gettext("R-chart phase I (multiple columns)", domain="R-RcmdrPlugin.UCA"),
        graphtitle = gettext("R-chart\\nfor", domain="R-RcmdrPlugin.UCA"),
        x1title = gettext("Measurements (pick two variables or more)", domain="R-RcmdrPlugin.UCA"),
        type = "R", phase = "1", help = "R-chart", recall = R1mcMenu, reset = "R1mcMenu", apply = "R1mcMenu")
}

R2mcMenu <- function()
{
    .qccMenu(
        dialogtitle = gettext("R-chart phase II from data (multiple columns)", domain="R-RcmdrPlugin.UCA"),
        graphtitle = gettext("R-chart\\nfor", domain="R-RcmdrPlugin.UCA"),
        x1title = gettext("Measurements phase I from data (pick two variables or more)", domain="R-RcmdrPlugin.UCA"),
        x2title = gettext("Measurements phase II from data (pick two variables or more)", domain="R-RcmdrPlugin.UCA"),
        type = "R", phase = "2", help = "R-chart", recall = R2mcMenu, reset = "R2mcMenu", apply = "R2mcMenu")
}

RpmcMenu <- function()
{
    .qccMenu(
        dialogtitle = gettext("R-chart phase II from parameters (multiple columns)", domain="R-RcmdrPlugin.UCA"),
        graphtitle = gettext("R-chart\\nfor", domain="R-RcmdrPlugin.UCA"),
        x1title = gettext("Measurements phase I from parameters (pick two variables or more)", domain="R-RcmdrPlugin.UCA"),
        x2title = gettext("Measurements phase II from parameters (pick two variables or more)", domain="R-RcmdrPlugin.UCA"),
        type = "R", phase = "p", help = "R-chart", recall = RpmcMenu, reset = "RpmcMenu", apply = "RpmcMenu")
}

S1mcMenu <- function()
{
    .qccMenu(
        dialogtitle = gettext("S-chart (multiple columns)", domain="R-RcmdrPlugin.UCA"),
        graphtitle = gettext("S-chart\\nfor", domain="R-RcmdrPlugin.UCA"),
        x1title = gettext("Measurements (pick two variables or more)", domain="R-RcmdrPlugin.UCA"),
        type = "S", phase = "1", help = "S-chart", recall = S1mcMenu, reset = "S1mcMenu", apply = "S1mcMenu")
}

S2mcMenu <- function()
{
    .qccMenu(
        dialogtitle = gettext("S-chart phase II from data (multiple columns)", domain="R-RcmdrPlugin.UCA"),
        graphtitle = gettext("S-chart\\nfor", domain="R-RcmdrPlugin.UCA"),
        x1title = gettext("Measurements phase I (pick two variables or more)", domain="R-RcmdrPlugin.UCA"),
        x2title = gettext("Measurements phase II (pick two variables or more)", domain="R-RcmdrPlugin.UCA"),
        type = "S", phase = "2", help = "S-chart", recall = S2mcMenu, reset = "S2mcMenu", apply = "S2mcMenu")
}

SpmcMenu <- function()
{
    .qccMenu(
        dialogtitle = gettext("S-chart phase II from parameters (multiple columns)", domain="R-RcmdrPlugin.UCA"),
        graphtitle = gettext("S-chart\\nfor", domain="R-RcmdrPlugin.UCA"),
        x1title = gettext("Measurements phase I (pick two variables or more)", domain="R-RcmdrPlugin.UCA"),
        x2title = gettext("Measurements phase II (pick two variables or more)", domain="R-RcmdrPlugin.UCA"),
        type = "S", phase = "p", help = "S-chart", recall = SpmcMenu, reset = "SpmcMenu", apply = "SpmcMenu")
}

### Function to produce a phase-I u-chart using cchart.p function in qcc package
uchart1Menu <- function()
{
    ## To ensure that menu name is included in pot file
    gettext("Counts", domain="R-RcmdrPlugin.UCA")
    gettext("u-chart (average rate)...", domain="R-RcmdrPlugin.UCA")
    .qccMenu(
        dialogtitle = gettext("u-chart (average rate)", domain="R-RcmdrPlugin.UCA"),
        graphtitle = gettext("u-chart\\nfor", domain="R-RcmdrPlugin.UCA"),
        x1title = gettext("Nonconformities", domain="R-RcmdrPlugin.UCA"),
        n1title = gettext("Sizes", domain="R-RcmdrPlugin.UCA"),
        type = "u", phase = "1", help = "u-chart", recall = uchart1Menu, reset = "uchart1Menu", apply = "uchart1Menu")
}

### Function to produce a phase-I u-chart using cchart.p function in qcc package
uchart2Menu <- function()
{
    ## To ensure that menu name is included in pot file
    gettext("Counts", domain="R-RcmdrPlugin.UCA")
    gettext("u-chart (average rate)...", domain="R-RcmdrPlugin.UCA")
    .qccMenu(
        dialogtitle = gettext("u-chart phase II from data", domain="R-RcmdrPlugin.UCA"),
        graphtitle = gettext("u-chart\\nfor", domain="R-RcmdrPlugin.UCA"),
        x1title = gettext("Nonconformities (Phase I)", domain="R-RcmdrPlugin.UCA"),
        n1title = gettext("Sizes (Phase I)", domain="R-RcmdrPlugin.UCA"),
        x2title = gettext("Nonconformities (Phase II)", domain="R-RcmdrPlugin.UCA"),
        n2title = gettext("Sizes (Phase II)", domain="R-RcmdrPlugin.UCA"),
        type = "u", phase = "2", help = "u-chart", recall = uchart2Menu, reset = "uchart2Menu", apply = "uchart2Menu")
}

### Function to produce a phase-I u-chart using cchart.p function in qcc package
uchartpMenu <- function()
{
    ## To ensure that menu name is included in pot file
    gettext("Counts", domain="R-RcmdrPlugin.UCA")
    gettext("u-chart (average rate)...", domain="R-RcmdrPlugin.UCA")
    .qccMenu(
        dialogtitle = gettext("u-chart phase II from parameter", domain="R-RcmdrPlugin.UCA"),
        graphtitle = gettext("u-chart\\nfor", domain="R-RcmdrPlugin.UCA"),
        x1title = gettext("Nonconformities (Phase I)", domain="R-RcmdrPlugin.UCA"),
        n1title = gettext("Sizes (Phase I)", domain="R-RcmdrPlugin.UCA"),
        x2title = gettext("Nonconformities (Phase II)", domain="R-RcmdrPlugin.UCA"),
        n2title = gettext("Sizes (Phase II)", domain="R-RcmdrPlugin.UCA"),
        type = "u", phase = "p", help = "u-chart", recall = uchartpMenu, reset = "uchartpMenu", apply = "uchartpMenu")
}

xbarone1Menu <- function()
{
    gettext("Continuous", domain="R-RcmdrPlugin.UCA")
    gettext("One-at-time data", domain="R-RcmdrPlugin.UCA")
    .qccMenu(
        dialogtitle = gettext("One-at-time data", domain="R-RcmdrPlugin.UCA"),
        x1title = gettext("Measurements", domain="R-RcmdrPlugin.UCA"),
        type = "xbar.one", help = "xbar.one-chart", recall = xbarone1Menu, reset = "xbarone1Menu", apply = "xbarone1Menu")
}

xbar1mcMenu <- function()
{
    .qccMenu(
        dialogtitle = gettext("xbar-chart (multiple columns)", domain="R-RcmdrPlugin.UCA"),
        graphtitle = gettext("xbar-chart\\nfor", domain="R-RcmdrPlugin.UCA"),
        x1title = gettext("Measurements (pick two variables or more)", domain="R-RcmdrPlugin.UCA"),
        type = "xbar", phase = "1", help = "xbar-chart", recall = xbar1mcMenu, reset = "xbar1mcMenu", apply = "xbar1mcMenu")
}

xbar2mcMenu <- function()
{
    .qccMenu(
        dialogtitle = gettext("xbar-chart phase II from data (multiple columns)", domain="R-RcmdrPlugin.UCA"),
        graphtitle = gettext("xbar-chart\\nfor", domain="R-RcmdrPlugin.UCA"),
        x1title = gettext("Measurements phase I (pick two variables or more)", domain="R-RcmdrPlugin.UCA"),
        x2title = gettext("Measurements phase II (pick two variables or more)", domain="R-RcmdrPlugin.UCA"),
        type = "xbar", phase = "2", help = "xbar-chart", recall = xbar2mcMenu, reset = "xbar2mcMenu", apply = "xbar2mcMenu")
}

xbarpmcMenu <- function()
{
    .qccMenu(
        dialogtitle = gettext("xbar-chart phase II from parameters (multiple columns)", domain="R-RcmdrPlugin.UCA"),
        graphtitle = gettext("xbar-chart\\nfor", domain="R-RcmdrPlugin.UCA"),
        x1title = gettext("Measurements phase I (pick two variables or more)", domain="R-RcmdrPlugin.UCA"),
        x2title = gettext("Measurements phase II (pick two variables or more)", domain="R-RcmdrPlugin.UCA"),
        type = "xbar", phase = "p", help = "xbar-chart", recall = xbarpmcMenu, reset = "xbarpmcMenu", apply = "xbarpmcMenu")
}
