#!/bin/bash  

# Install dependencies
while read pkg; do
    Rscript -e "if (!requireNamespace('$pkg', quietly = TRUE)) install.packages('$pkg', repos='https://cloud.r-project.org')"
done < requirements.txt

# Run the script
Rscript -e "rmarkdown::render('kodaqs_assignment_D2.1.Rmd')"