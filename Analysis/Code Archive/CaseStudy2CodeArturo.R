##################
#Import and Clean
##################

library(knitr)


#2.
#read the data
#2.a.
procrastination<-read.csv('~/CaseStudy2/Data/Procrastination.csv', header=TRUE)

#general info
str(procrastination)

#2.b.
#dimensions
dim(procrastination)

#spot check first rows
head(procrastination)

#2.c.
#names
names(procrastination)
lapply(names(procrastination), nchar)

#2.d.
#Check Character and Numeric
#apply(procrastination, 2, class)
catch=NA
for(i in 1:length(procrastination[1,])){catch<-c(catch, is.numeric(procrastination[,i]))}
#catch[-1]

#Numeric Summary
#apply(procrastination[,c(1,6,8:9, 14:59)], 2, summary)
apply(procrastination[,catch[-1]], 2, summary)

#Character Summary
#apply(procrastination[,-c(1,6,8:9, 14:59)], 2, unique)
apply(procrastination[,!catch[-1]], 2, unique)

aggregate(procrastination[,!catch[-1]], by= , FUN=is.na)

### Clean data by variable ###

#rename
names(procrastination)[1]<-'Age'

names(procrastination)[4]<-'Education'

#names(procrastination)[5]<-'Work.Status'
levels(procrastination$Work.Status)[match('0', levels(procrastination$Work.Status))]<-''

names(procrastination)[6]<-'Income.Year'
#tail(sort(procrastination$Income.Year), 150)
#Consider that the incomes are buckets not values
#unique(procrastination$Income.Year)

names(procrastination)[7]<-'Current.Job'
#unique(procrastination$Current.Job)
#check for other info about 's'
#procrastination[procrastination$Current.Job == 's',]
levels(procrastination$Current.Job)[match('na', levels(procrastination$Current.Job))]<-''
levels(procrastination$Current.Job)[match('0', levels(procrastination$Current.Job))]<-''

#Fix Misspellings
#ouh could be oxford university hospital was not changed
levels(procrastination$Current.Job)[match('s', levels(procrastination$Current.Job))]<-'student'
levels(procrastination$Current.Job)[match('Studey', levels(procrastination$Current.Job))]<-'student'
levels(procrastination$Current.Job)[match('psychologis', levels(procrastination$Current.Job))]<-'psychologist'
levels(procrastination$Current.Job)[match('mktg', levels(procrastination$Current.Job))]<-'Marketing'
levels(procrastination$Current.Job)[match('MD', levels(procrastination$Current.Job))]<-'Physician'
levels(procrastination$Current.Job)[match('Economy', levels(procrastination$Current.Job))]<-'Economist'
levels(procrastination$Current.Job)[match('vidoe', levels(procrastination$Current.Job))]<-'video'
levels(procrastination$Current.Job)[match('houswife', levels(procrastination$Current.Job))]<-'Housewife'
gsub('â???"', '', levels(procrastination$Current.Job))
levels(procrastination$Current.Job)[match('\'Utterly shiftless arts student\'... at p', levels(procrastination$Current.Job))]<-'student'
levels(procrastination$Current.Job)[match('asst', levels(procrastination$Current.Job))]<-'Assistant'

names(procrastination)[8]<-'Years.Empl.'
#tail(sort(procrastination$Years.Empl.), 50)
#999 years is missing since we don't know what it means
procrastination$Years.Empl.<-ifelse(procrastination$Years.Empl. == 999, NA, procrastination$Years.Empl.)

names(procrastination)[9]<-'Months.Empl.'

names(procrastination)[10]<-'Comm.Size'
#CHeck if 8 or 0 corresponds to a category if numeric
#procrastination$Comm.Size[as.numeric(procrastination$Comm.Size) == 8]
#procrastination$Comm.Size[as.numeric(procrastination$Comm.Size) == 0]
#Change to 'Small City based on other responses
levels(procrastination$Comm.Size)[match('8', levels(procrastination$Comm.Size))]<-'Small City'
levels(procrastination$Comm.Size)[match('0', levels(procrastination$Comm.Size))]<-''

names(procrastination)[11]<-'Country'
levels(procrastination$Country)[match('0', levels(procrastination$Country))]<-''

names(procrastination)[12]<-'Marital.Stat'
levels(procrastination$Marital.Stat)[match('0', levels(procrastination$Marital.Stat))]<-''

names(procrastination)[13]<-'Sons'
levels(procrastination$Sons)[match('Male', levels(procrastination$Sons))]<-'1'
levels(procrastination$Sons)[match('Female', levels(procrastination$Sons))]<-'2'
#Convert to numeric
procrastination$Sons = as.numeric(as.character(procrastination$Sons))

names(procrastination)[14]<-'Daughters'
#procrastination$Daughters = as.numeric(procrastination$Daughters)

names(procrastination)[15]<-'XDP1'
names(procrastination)[16]<-'XDP2'
names(procrastination)[17]<-'XDP3'
names(procrastination)[18]<-'XDP4'
names(procrastination)[19]<-'XDP5'
names(procrastination)[20]<-'XAIP1'
names(procrastination)[21]<-'XAIP2'
names(procrastination)[22]<-'XAIP3'
names(procrastination)[23]<-'XAIP4'
names(procrastination)[24]<-'XAIP5'
names(procrastination)[25]<-'XAIP6'
names(procrastination)[26]<-'XAIP7'
names(procrastination)[27]<-'XAIP8'
names(procrastination)[28]<-'XAIP9'
names(procrastination)[29]<-'XAIP10'
names(procrastination)[30]<-'XAIP11'
names(procrastination)[31]<-'XAIP12'
names(procrastination)[32]<-'XAIP13'
names(procrastination)[33]<-'XAIP14'
names(procrastination)[34]<-'XAIP15'
names(procrastination)[35]<-'XGP1'
names(procrastination)[36]<-'XGP2'
names(procrastination)[37]<-'XGP3'
names(procrastination)[38]<-'XGP4'
names(procrastination)[39]<-'XGP5'
names(procrastination)[40]<-'XGP6'
names(procrastination)[41]<-'XGP7'
names(procrastination)[42]<-'XGP8'
names(procrastination)[43]<-'XGP9'
names(procrastination)[44]<-'XGP10'
names(procrastination)[45]<-'XGP11'
names(procrastination)[46]<-'XGP12'
names(procrastination)[47]<-'XGP13'
names(procrastination)[48]<-'XGP14'
names(procrastination)[49]<-'XGP15'
names(procrastination)[50]<-'XGP16'
names(procrastination)[51]<-'XGP17'
names(procrastination)[52]<-'XGP18'
names(procrastination)[53]<-'XGP19'
names(procrastination)[54]<-'XGP20'
names(procrastination)[55]<-'SWLS1'
names(procrastination)[56]<-'SWLS2'
names(procrastination)[57]<-'SWLS3'
names(procrastination)[58]<-'SWLS4'
names(procrastination)[59]<-'SWLS5'

names(procrastination)[60]<-'Self.Assess'

names(procrastination)[61]<-'Other.Assess'
#Check if '4' or '0' correspond to a categor under numeric coding
#procrastination$Other.Assess[as.numeric(procrastination$Other.Assess) == 4]
#procrastination$Other.Assess[as.numeric(procrastination$Other.Assess) == 0]
levels(procrastination$Other.Assess)[match('4', levels(procrastination$Other.Assess))]<-'no'
levels(procrastination$Other.Assess)[match('0', levels(procrastination$Other.Assess))]<-''

#2.e.
#Create means for category of survey questions
#First, find the indexes of the survey category in question
#Then, use apply() to find the row mean only for those categories

XGP<-grep( 'XGP', names(procrastination))
procrastination$XGP.Mean<-apply(procrastination[,XGP], 1, mean, na.rm=TRUE)

XDP<-grep( 'XDP', names(procrastination))
procrastination$XDP.Mean<-apply(procrastination[,XDP], 1, mean, na.rm=TRUE)

XAIP<-grep( 'XAIP', names(procrastination))
procrastination$XAIP.Mean<-apply(procrastination[,XAIP], 1, mean, na.rm=TRUE)

SWLS<-grep( 'SWLS', names(procrastination))
procrastination$SWLS.Mean<-apply(procrastination[,SWLS], 1, mean, na.rm=TRUE)


#2.d.
#Double Check Character and Numeric
str(procrastination)

#apply(procrastination, 2, class)
catch=NA
for(i in 1:length(procrastination[1,])){catch<-c(catch, is.numeric(procrastination[,i]))}
#catch[-1]

#Numeric Summary
#apply(procrastination[,c(1,6,8:9, 14:59)], 2, summary)
apply(procrastination[,catch[-1]], 2, summary)

#Character Summary
#apply(procrastination[,-c(1,6,8:9, 14:59)], 2, unique)
apply(procrastination[,!catch[-1]], 2, unique)

str(procrastination)

procrastination[procrastination$Kids == 'No Kids', c(3,13)]
procrastination[procrastination$Kids == 'No Kids', ]
procrastination$Sons[procrastination$Kids == 'No Kids']
as.numeric(procrastination$Sons[procrastination$Kids == 'No Kids'])



######## MERGE THE TWO DATASESTS ###############

#3.c.
#THis requires both 'procrasintation' and 'hdi_total' from previous codes

sort(unique(procrastination$Country))
#Use Spanish Spelling of COlumbia
levels(procrastination$Country)[match('Columbia', levels(procrastination$Country))]<-'Colombia'
#Use correct Spelling of Israel
levels(procrastination$Country)[match('Isreal', levels(procrastination$Country))]<-'Israel'

sort(unique(hdi_total$Country))

procrast_hdi<-merge(x=procrastination, y=hdi_total, by.x='Country', by.y='Country', all.x = TRUE)
summary(test$procrast_hdi)
str(procrast_hdi)

unique(procrast_hdi$Country[is.na(procrast_hdi$HDI)])
#No HDI for Antiqua, Bermuda, Guam, Macao, Puerto Rico, and former Yogoslavia
Ta<-table(procrast_hdi$Country[is.na(procrast_hdi$HDI)])
Ta[Ta > 0]


unique(procrast_hdi1$Self.Assess )
unique( procrast_hdi1$Other.Assess)
unique( procrast_hdi$Country)

Vector <- data.frame('Match.Assess'<-paste(procrast_hdi1$Self.Assess , procrast_hdi1$Other.Assess, sep = "|"))
Vector <- Vector[Vector == 'yes|yes' | Vector == 'no|no']
table(Vector)

procrast_hdi1$Match.Assess <- procrast_hdi1$Self.Assess == procrast_hdi1$Other.Assess

procrast_hdi1$Country



ggplot(df.shooter_graph, aes(x=reorder(Name,`FG%`),y=`FG%`)) +  geom_bar(stat="identity", aes(fill=Position)) +  
                  ggtitle("Field Goal Percentage by Player") +  
                  theme(plot.title = element_text(hjust = 0.5)) + 
                  xlab("Name") +  ylab("Field Goal Percentage") +  
                  geom_text(aes(label = `FG%`, hjust=-.25)) +  
                  ylim(0,.8) +  coord_flip()

p<-ggplot(data=stateFREQ, aes(x=State, y=Freq)) + geom_bar(stat="identity", fill=c1[1:48]) + ggtitle('Chart') 
p<- p +   labs(title="Barplot of VAMC Facilities per State") + xlab("State") + ylab("Number of Veterans Administration Health Facilities")
p<- p  + theme_minimal() + theme(plot.title = element_text(hjust = 0.5), axis.text.y = element_text(size=6.5)) + coord_flip()
#p<- p  + theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) + coord_flip()
p

Ta<-sort(table(procrast_hdi1$Country), decreasing=TRUE)
Ta[1:15]
summary(procrast_hdi1$Country)
sum(Ta)
str(procrast_hdi1)
  
temp<-data.frame(procrast_hdi1[order(-procrast_hdi1, na.last = TRUE), ])
temp[c(1:3, 4:16),]

ggplot(data=temp, aes(x=))


procrastination$Current.Job[grepl('â???"', procrastination$Current.Job)]
procrastination$Current.Job[grepl('&', procrastination$Current.Job)]
sum(grepl('â???"', procrastination$Current.Job))

procrastination$Current.Job[!is.na(procrastination$Current.Job)]

unique(procrast_hdi$Self.Assess)
unique(procrast_hdi$Other.Assess)
unique(procrast_hdi$Gender)
unique(procrast_hdi$Age)
unique(procrast_hdi$Income.Year)

summary(procrast_hdi$Gender)


HDI.level.table<-aggregate(procrast_hdi1$SWLS.Mean, by=list(procrast_hdi1$Development_Level), FUN=mean)
names(HDI.level.table)<-c("HDI Level", 'SWLS.Mean')
HDI.level.table

display.brewer.all()

table(procrast_hdi1[, c("Age", "Income.Year", "Gender")])
test<-aggregate( Gender ~ Age + Income.Year, data=procrast_hdi1, FUN=is.na)
dim(test)

library(plyr)
#get frequencies
q5d<-count(procrast_hdi1, c("Age", "Income.Year", "Gender"))
#str(q5d)

#merge frequencies to procrast_hdi1 to read into ggplot
freqAIG<-merge(procrast_hdi1[, c("Age", "Income.Year", "Gender")], q5d, by=c("Age", "Income.Year", "Gender"), all.x = TRUE)
#dim(freqAIG)
