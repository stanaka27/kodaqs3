#!/bin/bash  
install.packages(c("tidyverse", "dplyr", "readxl", "rmarkdown"))
Rscript -e "rmarkdown::render('kodaqs_assignment_D2.1.Rmd')"