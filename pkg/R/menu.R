## Internal generic menu-dialog function
.Menu <- function(varname, dialogtitle, elements, help, recall, reset, apply, onokcommand) {
    ## Initialize dialog
    initializeDialog(title = dialogtitle)

    ## Main loop to create elements
    for(i in 1:length(elements)) {
        switch(elements[[i]]$type,
               variableListBox = {
                   ## Check min parameter 
                   suppressWarnings(elements[[i]]$min <- as.numeric(elements[[i]]$min))
                   if (!length(elements[[i]]$min) || is.na(elements[[i]]$min) || (elements[[i]]$min < 1)) elements[[i]]$min <- 1
                   ## Check max parameter 
                   suppressWarnings(elements[[i]]$max <- as.numeric(elements[[i]]$max))
                   if (!length(elements[[i]]$max) || is.na(elements[[i]]$max)) elements[[i]]$max <- Inf
                   if ((elements[[i]]$max < 1) || (elements[[i]]$min > elements[[i]]$max)) elements[[i]]$max <- elements[[i]]$min
                   ## Check selectmode
                   elements[[i]]$selectmode <- match.arg(elements[[i]]$selectmode, c('single', 'multiple'))
                   ## Create element
                   elements[[i]]$variableListBox <- variableListBox(top, DiscreteNumeric(), selectmode = elements[[i]]$selectmode, initialSelection=NULL, title = elements[[i]]$title)
               },
               entry = {
                   ## Check min parameter 
                   suppressWarnings(elements[[i]]$min <- as.numeric(elements[[i]]$min))
                   if (!length(elements[[i]]$min) || is.na(elements[[i]]$min)) elements[[i]]$min <- -Inf
                   ## Check max parameter 
                   suppressWarnings(elements[[i]]$max <- as.numeric(elements[[i]]$max))
                   if (!length(elements[[i]]$max) || is.na(elements[[i]]$max)) elements[[i]]$max <- Inf
                   if (elements[[i]]$min > elements[[i]]$max) elements[[i]]$max <- elements[[i]]$min
                   ## Check var type
                   if (is.null(elements[[i]]$vartype)) stop(gettext('Var type, vartype, not provided'))
                   if (!(elements[[i]]$vartype %in% c('integer', 'numeric', 'text'))) stop(gettext('Var type'), elements[[i]]$vartype, gettext('not yet implemented.'))
                   ## Create element
                   elements[[i]]$tclVar <- tclVar('')
                   if (!is.null(elements[[i]]$default)) tclvalue(elements[[i]]$tclVar) <- elements[[i]]$default
                   elements[[i]]$entry <- tkentry(top, textvariable = elements[[i]]$tclVar)
               },
               stop(gettext('Element type '), elements[[i]]$type, gettext(' not yet implemented.'))
               )
    }

    onOK <- function() {
        ## Main loop to read data from elements
        for(i in 1:length(elements)) {
            switch(elements[[i]]$type,
                   variableListBox = {
                       elements[[i]]$variableListBoxSelected <- getSelection(elements[[i]]$variableListBox)
                       if (length(elements[[i]]$variableListBoxSelected) < elements[[i]]$min) {
                           errorCondition(recall = recall, message = elements[[i]]$error)
                           return()
                       }
                       if (length(elements[[i]]$variableListBoxSelected) > elements[[i]]$max) {
                           errorCondition(recall = recall, message = elements[[i]]$error)
                           return()
                       }
                   },
                   entry = {
                       switch(elements[[i]]$vartype,
                              integer = {
                                  suppressWarnings(value <- as.integer(tclvalue(elements[[i]]$tclVar)))
                                  if (!length(value) || is.na(value) || (value < elements[[i]]$min) || (value > elements[[i]]$max)) {
                                      errorCondition(recall = recall, message = elements[[i]]$error)
                                      return()
                                  }
                                  elements[[i]]$VarInteger <- value
                              },
                              text = {
                                  elements[[i]]$VarText <- tclvalue(elements[[i]]$tclVar)
                              },
                              numeric = {
                                  suppressWarnings(value <- as.numeric(tclvalue(elements[[i]]$tclVar)))
                                  if (!length(value) || is.na(value) || (value < elements[[i]]$min) || (value > elements[[i]]$max)) {
                                      errorCondition(recall = recall, message = elements[[i]]$error)
                                      return()
                                  }
                                  elements[[i]]$VarNumeric <- value
                              },
                              stop(gettext('Var type'), elements[[i]]$vartype, gettext('not yet implemented.'))
                              )
                   }
                   )
        }
        ## Prepare command
        if (!is.function(onokcommand)) stop(gettext('onokcommand parameter is not a function'))
        command <- onokcommand(elements)
        ## Note closeDialog must be called after onokcommand
        closeDialog()
        tkdestroy(top)
        tkfocus(CommanderWindow())
        ## Do command
        doItAndPrint(command)
    }

    ## Create button panel
    OKCancelHelp(helpSubject = help, reset = reset, apply = apply)
    ## Show elements
    for(i in 1:length(elements)) {
        switch(elements[[i]]$type,
               variableListBox = {
                   tkgrid(getFrame(elements[[i]]$variableListBox), sticky = 'w', row = 1, column = i - 1)
               },
               entry = {
                   tkgrid(tklabel(top, text = elements[[i]]$title), elements[[i]]$entry, sticky = 'w')
               },
               stop(gettext('Element type'), elements[[i]]$type, gettext('not yet implemented.'))
               )
    }
    tkgrid(buttonsFrame, sticky='w', columnspan = 8)
    dialogSuffix()
}
