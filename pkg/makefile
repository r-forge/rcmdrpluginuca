package := RcmdrPlugin.UCA
version := 6.8-1
R := $(wildcard R/*.R)
Rd := $(wildcard man/*.Rd)
Rmd := $(wildcard vignettes/*.Rmd)
html := $(patsubst vignettes/%.Rmd,inst/doc/%.html,$(Rmd))

0: $(package).log
	@echo Test done
check: $(package).Rcheck
	@echo Check done
dev: dev.log
	make dev.log
dev.log: makefile $(R)
	date > dev.log
	echo "library('devtools')" >.Rprofile
	echo "library('RcmdrPlugin.UCA')" >>.Rprofile
	echo "source('R/menu.R')" >>.Rprofile
	echo "source('R/psychometry.R')" >>.Rprofile
	echo "set.seed(1)" >>.Rprofile
	echo "d <- data.frame(b1 = rbinom(10, 1, .3), b2 = rbinom(10, 1, .3), p1 = rbinom(10, 5, .3), p2 = rbinom(10, 5, .3))" >>.Rprofile
	echo "d\$$bTotal <- d\$$b1 + d\$$b2" >>.Rprofile
	echo "d\$$dTotal <- d\$$p1 + d\$$p2" >>.Rprofile
	echo "activeDataSet('d')" >>.Rprofile
	echo "PsyAlphaCIMenu()" >>.Rprofile
	R --interactive --no-save
	rm .Rprofile
	reset
inst/doc/%.html: $(R) $(Rmd)
	make vignettes
inst/po/es/LC_MESSAGES/R-$(package).mo: po/R-$(package)-es.po
	msgfmt -c -o inst/po/es/LC_MESSAGES/R-$(package).mo po/R-$(package)-es.po
$(package).log: makefile ~/R_LIBS/$(package)
	date > $(package).log
	echo "library(RcmdrPlugin.UCA)" >.Rprofile
	echo "data(Chile)" >>.Rprofile
	echo "activeDataSet('Chile')" >>.Rprofile
	echo "set.seed(1)" >>.Rprofile
	echo "Chile\$$p1 <- rbinom(nrow(Chile), 5, .1)" >>.Rprofile
	echo "Chile\$$p2 <- rbinom(nrow(Chile), 5, .2)" >>.Rprofile
	echo "Chile\$$p3 <- rbinom(nrow(Chile), 5, .3)" >>.Rprofile
	echo "Chile\$$p4 <- rbinom(nrow(Chile), 5, .4)" >>.Rprofile
	echo "Chile\$$p5 <- rbinom(nrow(Chile), 5, .5)" >>.Rprofile
	echo "activeDataSet('Chile')" >>.Rprofile
	R --interactive --no-save
	rm .Rprofile
	reset
$(package).Rcheck: $(package)_$(version).tar.gz
	R CMD check --as-cran $(package)_$(version).tar.gz
## Final version
$(package)_$(version).tar.gz: $(R) $(Rd) $(Rmd) $(html) ChangeLog DESCRIPTION inst/etc/menus.txt inst/po/es/LC_MESSAGES/R-$(package).mo NAMESPACE po/R-$(package)-es.po po/R-$(package).pot
## Development version
## $(package)_$(version).tar.gz: $(R) inst/etc/menus.txt NAMESPACE
	R -e "library('roxygen2'); roxygenize('.')"
	R CMD build --no-build-vignettes .
	R CMD INSTALL $(package)_$(version).tar.gz
po/R-$(package)-es.po: $(R) po/R-$(package).pot
	poedit po/R-$(package)-es.po
po/R-$(package).pot: $(R)
	# This target builds the pot file and update any R-$(package)-lang.po file under po dir
	R -e "library('tools'); update_pkg_po('.')"
	touch po/R-$(package).pot
~/R_LIBS/$(package): $(package)_$(version).tar.gz
	R CMD INSTALL $(package)_$(version).tar.gz
vignettes: $(R) $(Rmd)
	R -e "devtools::build_vignettes('.', clean = FALSE, install = FALSE)"
	mv --force doc/* inst/doc/
	rmdir doc
