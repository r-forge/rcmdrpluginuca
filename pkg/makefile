package := RcmdrPlugin.UCA
version := 6.9-5
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
	echo "d <- data.frame(bi1 = rbinom(10, 2, .3), bi2 = rbinom(10, 3, .3), bi3 = rbinom(10, 4, .3), bi4 = rbinom(10, 1, .3), be1 = rbinom(10, 1, .3), be2 = rbinom(10, 1, .3), be3 = rbinom(10, 1, .3), be4 = rbinom(10, 1, .3))" >>.Rprofile
	echo "d\$$biTotal <- d\$$bi1 + d\$$bi2 + d\$$bi3 + d\$$bi4" >>.Rprofile
	echo "d\$$beTotal <- d\$$be1 + d\$$be2 + d\$$be3 + d\$$be4" >>.Rprofile
	echo "activeDataSet('d')" >>.Rprofile
	echo "PsyScoreMenu()" >>.Rprofile
	echo "LANGUAGE=en" >.Renviron
	echo "R_LIBS_USER=\"~/R_LIBS/\"" >>.Renviron
	R --interactive --no-save
	echo "R_LIBS_USER=\"~/R_LIBS/\"" >.Renviron
	R --interactive --no-save
	rm .Rprofile
	rm .Renviron
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
	echo "d <- data.frame(bi1 = rbinom(10, 2, .3), bi2 = rbinom(10, 3, .3), bi3 = rbinom(10, 4, .3), bi4 = rbinom(10, 5, .3), be1 = rbinom(10, 1, .3), be2 = rbinom(10, 1, .3), be3 = rbinom(10, 1, .3), be4 = rbinom(10, 1, .3))" >>.Rprofile
	echo "d\$$biTotal <- d\$$bi1 + d\$$bi2 + d\$$bi3 + d\$$bi4" >>.Rprofile
	echo "d\$$beTotal <- d\$$be1 + d\$$be2 + d\$$be3 + d\$$be4" >>.Rprofile
	echo "activeDataSet('d')" >>.Rprofile
	echo "LANGUAGE=en" >.Renviron
	echo "R_LIBS_USER=\"~/R_LIBS/\"" >>.Renviron
	R --interactive --no-save
	echo "R_LIBS_USER=\"~/R_LIBS/\"" >.Renviron
	R --interactive --no-save
	rm .Rprofile
	rm .Renviron
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
