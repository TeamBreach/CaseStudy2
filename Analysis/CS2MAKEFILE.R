## MakeFile

#Libraries for all anlayses
library(knitr)
library(xml2)
library(rvest)

#INPUT: Procrastination.csv
source('~/CaseStudy2/Analysis/procrastination_data.R', echo=FALSE)
#OUTPUT: procrastination

source(source('~/CaseStudy2/Analysis/hdi_data.R'), echo=FALSE)
#OUTPUT: hdi_total

#INPUT: procrastination; hdi_total;
source(source('~/CaseStudy2/Analysis/merge_data.R'), echo=FALSE)
source(source('~/CaseStudy2/Analysis/hdi_data.R'), echo=FALSE)
#OUTPUT: procrast_hdi