#!/bin/bash

Rscript -e 'rmarkdown::render("crossv.Rmd", "html_document", "Uganda_male", params = list(country = "Uganda", sex = 1))'

exit 0
