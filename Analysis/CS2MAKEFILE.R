## MakeFile

#Libraries for all anlayses
library(knitr)
library(xml2)
library(rvest)
#getwd()

#INPUT: Procrastination.csv
source('~/CaseStudy2/Analysis/procrastination_data.R', echo=TRUE)
#OUTPUT: procrastination

source('~/CaseStudy2/Analysis/hdi_data.R', echo=TRUE)
#OUTPUT: hdi_total

#INPUT: procrastination; hdi_total;
source('~/CaseStudy2/Analysis/merge_data.R', echo=TRUE)
#OUTPUT: procrast_hdi