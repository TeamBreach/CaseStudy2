
############## MERGE THE TWO DATASESTS ################

#3.c.
#THis requires both 'procrasintation' and 'hdi_total' from previous codes

#Check Spellings
#sort(unique(procrastination$Country))
#sort(unique(hdi_total$Country))

#Use Spanish Spelling of COlumbia
levels(procrastination$Country)[match('Columbia', levels(procrastination$Country))]<-'Colombia'
#Use correct Spelling of Israel
levels(procrastination$Country)[match('Isreal', levels(procrastination$Country))]<-'Israel'

procrast_hdi<-merge(x=procrastination, y=hdi_total, by.x='Country', by.y='Country', all.x = TRUE)

##Check results
#summary(test$procrast_hdi)
#str(procrast_hdi)

#unique(procrast_hdi$Country[is.na(procrast_hdi$HDI)])
##No HDI for Antiqua, Bermuda, Guam, Macao, Puerto Rico, and former Yogoslavia

#Summary of missing values
#Ta<-table(procrast_hdi$Country[is.na(procrast_hdi$HDI)])
#Ta[Ta > 0]

#Export the tidied data
#write.csv(procrast_hdi, file='~/CaseStudy2/Data/procrast_hdi.csv', row.names = FALSE, na=c(""," ","NA", "<NA>"))
