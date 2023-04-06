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
               stop(gettext('Element type'), elements[[i]]$type, gettext('not yet implemented.'))
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
                   stop(gettext('Element type'), elements[[i]]$type, gettext('not yet implemented.'))
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
        tkgrid(getFrame(elements[[i]]$variableListBox), sticky = 'w', row = 1, column = i - 1)
    }
    tkgrid(buttonsFrame, sticky='w', columnspan = 2)
    dialogSuffix()
}
