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
#
#Uncommment below if you woud like to  export the scraped hdi data
#write.csv(hdi_total, "C:/Users/emccandless/Documents/SMU/CaseStudy2/Data/hdi.csv", row.names=FALSE)

#INPUT: procrastination; hdi_total;
source('~/CaseStudy2/Analysis/merge_data.R', echo=TRUE)
#OUTPUT: procrast_hdi
#
#Uncommment below if you woud like to  export the tidied output
#write.csv(procrast_hdi, file='~/CaseStudy2/Data/procrast_hdi.csv', row.names = FALSE, na=c(""," ","NA", "<NA>"))
