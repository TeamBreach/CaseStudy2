## MakeFile

#Libraries for all anlayses
library(knitr)
library(xml2)
library(rvest)
library(ggplot2)
#getwd()




###### IMPORT THE PROCASINTATION DATA 

#INPUT: Procrastination.csv
source('~/CaseStudy2/Analysis/procrastination_data.R', echo=TRUE)
#OUTPUT: procrastination



###### IMPORT/SCRAPE THE HUMAN DEVELOPMENT TABLES

source('~/CaseStudy2/Analysis/hdi_data.R', echo=TRUE)
#OUTPUT: hdi_total
#
#Uncommment below if you woud like to  export the scraped hdi data
#write.csv(hdi_total, "C:/Users/emccandless/Documents/SMU/CaseStudy2/Data/hdi.csv", row.names=FALSE)




###### MERGE THE PROCRASTINATION AND HDI TABLES

#INPUT: procrastination; hdi_total;
source('~/CaseStudy2/Analysis/merge_data.R', echo=TRUE)
#OUTPUT: procrast_hdi
#
#### Uncommment below if you woud like to  export the tidied outputs
#
## Clean original
#write.csv(procrast_hdi, file='~/CaseStudy2/Data/procrastination_clean.csv', row.names = FALSE, na=c(""," ","NA", "<NA>"))
#
## Clean with hdi
#
#write.csv(procrast_hdi, file='~/CaseStudy2/Data/procrast_hdi.csv', row.names = FALSE, na=c(""," ","NA", "<NA>"))



###### PRELIMINARY ANALYSIS AND FREQUENCY TABLES  #######

#INPUT: procrast_hdi
source('~/CaseStudy2/Analysis/preliminary_analysis.R', echo=TRUE)
#OUTPUT: EMTABLE: This is a function that allows us to obtain frequency tables in the format of our preference
      #  The function must be created for future codes
#OUTPUT: procrast_hdi1: a subset of the merged procrastination and HDI tables for those over 18
      #  All future analysis will involve procrast_hdi1