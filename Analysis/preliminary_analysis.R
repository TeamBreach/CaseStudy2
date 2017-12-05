##### PRELIMINARY ANALYSIS #######


##### Create a function to present tables to our liking ######
## We find this more useful than table()
## A different analyst can use table() or a different function ahead
EMTABLE<-function(df, var1 = 'Variable', digits = 1){
  
 # df<-procrast_hdi1$Gender
 # digits = 1
 # var1 = 'Variable'

  procrast_hdi10 <- table(df)
  #Convert to data.frame
  procrast_hdi11 <- data.frame(cbind(procrast_hdi10, prop.table(procrast_hdi10)))
  #Tidy Count and Percentage
  names(procrast_hdi11) <- c("Count", "Percentage")
  procrast_hdi11$Percentage <- round(procrast_hdi11$Percentage * 100, digits=digits)
  #Descending order
  procrast_hdi11 <- procrast_hdi11[order(-procrast_hdi11$Percentage), ,drop = FALSE]
  #Row.Names as column
  procrast_hdi12 <- cbind(rownames(procrast_hdi11), procrast_hdi11)
  names(procrast_hdi12)[1] = var1
  rownames(procrast_hdi12) <- 1:nrow(procrast_hdi12)  
  
  #This outputs this table to our preferred format
  return(procrast_hdi12)

}


#4.a.
## Remove respondents age<18
## All future analysis will be done on this subset
procrast_hdi1 <- procrast_hdi[procrast_hdi$Age>18,]


#4.b.
## Summary stats for key variables.
#  This is a general summary for our key variables
procrast_hdi2 <- apply(procrast_hdi1[,c('Age','Income.Year', 'HDI', 'XGP.Mean', 'XDP.Mean', 'XAIP.Mean', 'SWLS.Mean')], 2, summary)
kable(procrast_hdi2)
# They will be cleaned in the RMarkdown


#4.c.
## These are frequency tables for three key variables

#First, table for gender
Gend.Table<-EMTABLE(procrast_hdi1$Gender, digits=1, var1 = 'Gender')
Gend.Table

#Then, table for work status
WorkStat.Table<-EMTABLE(procrast_hdi1$Work.Status, digits=1, var1 = 'Work Status')
kable(WorkStat.Table)

#Then, table for Job Table
JobTable<-EMTABLE(procrast_hdi1$Current.Job, digits=1, var1 = 'Current Occupation')
kable(JobTable[1:10,])

#histogram to show distribution of respondent age.
ggplot(procrast_hdi1, aes(procrast_hdi1$Age)) +
  geom_histogram(color="black",fill="blue") + theme_minimal() +
  ggtitle("Age Distribution") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Age") 

#histogram to show distribution of respondent income.
ggplot(procrast_hdi1, aes(procrast_hdi1$Income.Year)) +
  geom_histogram(color="black",fill="blue") + theme_minimal() +
  ggtitle("Income Distribution") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Income") 



#4.d.
#Top 15 countries among our respondents
Country.Table<-EMTABLE(procrast_hdi1$Country, digits=1, var1 = 'Country')
kable(Country.Table[1:15,])
#Note: NA or no response accounted for 231 participants, putting it in 4th place

#4.e.
#
Match.Assess <- data.frame('Match.Assess'<-paste(procrast_hdi1$Self.Assess , procrast_hdi1$Other.Assess, sep = "|"))
MatchTable<-EMTABLE(Match.Assess, digits=1, var1 = 'Assessment: Self | Others')
kable(MatchTable)