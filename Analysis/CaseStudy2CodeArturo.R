##################
#Import and Clean
##################

#2.
#read the data
#2.a.
procrastination<-read.csv('C:/users/acasi/Downloads/Procrastination.csv', header=TRUE)

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

#apply(procrastination, 2, class)
catch=NA
for(i in 1:length(procrastination[1,])){catch<-c(catch, is.numeric(procrastination[,i]))}
catch[-1]

#Numeric Summary
#apply(procrastination[,c(1,6,8:9, 14:59)], 2, summary)
apply(procrastination[,catch[-1]], 2, summary)

#Character Summary
#apply(procrastination[,-c(1,6,8:9, 14:59)], 2, unique)
apply(procrastination[,!catch[-1]], 2, unique)

### Clean data by variable ###

#rename
names(procrastination)[1]<-'Age'
#procrastination$Age <- as.numeric(procrastination$Age)

names(procrastination)[1]<-'Education'
procrastination$Education<-ifelse(procrastination$Education == '0', '', procrastination$Education)

names(procrastination)[6]<-'Income.Year'
#tail(sort(procrastination$Income.Year), 150)
#Consider that the incomes are buckets not values
unique(procrastination$Income.Year)

names(procrastination)[7]<-'Current.Job'
procrastination$Current.Job<-ifelse(procrastination$Current.Job == 's', '', procrastination$Current.Job)
levels(procrastination$Current.Job)[match('0', levels(procrastination$Current.Job))]<-''
#ouh could be oxford university hospital

names(procrastination)[8]<-'Years.Empl.'
#tail(sort(procrastination$Years.Empl.), 50)
procrastination$Years.Empl.<-ifelse(procrastination$Years.Empl. == 999, NA, procrastination$Years.Empl.)

names(procrastination)[9]<-'Months.Empl.'

names(procrastination)[10]<-'Comm.Size'
procrastination$Comm.Size[as.numeric(procrastination$Comm.Size) == 8]
#procrastination$Comm.Size[procrastination$Comm.Size == '8']
levels(procrastination$Comm.Size)[match('8', levels(procrastination$Comm.Size))]<-'Small City'

procrastination$Comm.Size[as.numeric(procrastination$Comm.Size) == 0]
#procrastination[procrastination$Comm.Size == '0',]
levels(procrastination$Comm.Size)[match('0', levels(procrastination$Comm.Size))]<-''

names(procrastination)[11]<-'Country'
procrastination$Country<-ifelse(procrastination$Country == '0', '', procrastination$Country)
levels(procrastination$Country)[match('0', levels(procrastination$Country))]<-''


names(procrastination)[12]<-'Marital.Stat'
procrastination$Marital.Stat<-ifelse(procrastination$Marital.Stat == '0', '', procrastination$Marital.Stat)

names(procrastination)[13]<-'Sons'
levels(procrastination$Country)[match('Male', levels(procrastination$Country))]<-'1'
levels(procrastination$Country)[match('Female', levels(procrastination$Country))]<-'2'
procrastination$Sons = as.numeric(procrastination$Sons)

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
procrastination$Other.Assess<-ifelse(procrastination$Other.Assess == '4', '', procrastination$Other.Assess)
procrastination$Other.Assess<-ifelse(procrastination$Other.Assess == '0', '', procrastination$Other.Assess)
as.numeric(procrastination$Other.Assess)
procrastination$Other.Assess[133] = 'no'

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

#Set missing values
str(procrastination)

#apply(procrastination, 2, class)
catch=NA
for(i in 1:length(procrastination[1,])){catch<-c(catch, is.numeric(procrastination[,i]))}
catch[-1]

#Numeric Summary
#apply(procrastination[,c(1,6,8:9, 14:59)], 2, summary)
apply(procrastination[,catch[-1]], 2, summary)

#Character Summary
#apply(procrastination[,-c(1,6,8:9, 14:59)], 2, unique)
apply(procrastination[,!catch[-1]], 2, unique)
