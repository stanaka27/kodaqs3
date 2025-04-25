#!/bin/bash  

# Install R packages listed in requirements.txt
while read pkg; do
    Rscript -e "if (!requireNamespace('$pkg', quietly = TRUE)) install.packages('$pkg', repos='https://cloud.r-project.org')"
done < requirements.txt

./kodaqs_assignment_D2.1.sh