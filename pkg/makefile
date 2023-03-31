package := RcmdrPlugin.UCA
version := 6.2-1
R := $(wildcard R/*.R)
Rd := $(wildcard man/*.Rd)
Rmd := $(wildcard vignettes/*.Rmd)
html := $(patsubst vignettes/%.Rmd,inst/doc/%.html,$(Rmd))

0: $(package).log
	@echo Test done

inst/doc/%.html: $(R) $(Rmd)
	make vignettes
inst/po/es/LC_MESSAGES/R-$(package).mo: po/R-$(package)-es.po
	msgfmt -c -o inst/po/es/LC_MESSAGES/R-$(package).mo po/R-$(package)-es.po
$(package).log: makefile ~/R_LIBS/$(package)
	date > $(package).log
	echo "library(RcmdrPlugin.UCA)" >.Rprofile
	echo "data(Chile)" >>.Rprofile
	echo "activeDataSet('Chile')" >>.Rprofile
	## echo "CDIMenu()" >>.Rprofile
	R --interactive --no-save
	rm .Rprofile
	reset
$(package).Rcheck: $(package)_$(version).tar.gz
	R CMD check --as-cran $(package)_$(version).tar.gz
## Final version
$(package)_$(version).tar.gz: $(R) $(Rd) $(Rmd) $(html) ChangeLog DESCRIPTION inst/etc/menus.txt inst/po/es/LC_MESSAGES/R-$(package).mo NAMESPACE po/R-$(package)-es.po po/R-$(package).pot
## Development version
##$(package)_$(version).tar.gz: $(R) inst/etc/menus.txt NAMESPACE
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
