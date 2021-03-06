##################
#Import and Clean
##################

#2.a.
#read the data
procrastination<-read.csv('./CaseStudy2/Data/Procrastination.csv', header=TRUE, na.strings=c(""," ","NA"))

#check dimensions
dim(procrastination)

### Clean data by variable ###
#2.b.-2.c.

#rename
names(procrastination)[1]<-'Age'

names(procrastination)[4]<-'Education'

#names(procrastination)[5]<-'Work.Status'
levels(procrastination$Work.Status)[match('0', levels(procrastination$Work.Status))]<-NA #''

names(procrastination)[6]<-'Income.Year'
#tail(sort(procrastination$Income.Year), 150)
#Consider that the incomes are buckets not values
#unique(procrastination$Income.Year)

names(procrastination)[7]<-'Current.Job'
#unique(procrastination$Current.Job)
#define missing
levels(procrastination$Current.Job)[match('na', levels(procrastination$Current.Job))]<-NA
levels(procrastination$Current.Job)[match('0', levels(procrastination$Current.Job))]<-NA
levels(procrastination$Current.Job)[match('please specify', levels(procrastination$Current.Job))]<-NA

#Fix Misspellings
#ouh could be oxford university hospital was not changed
#check for other info about 's'
#procrastination[procrastination$Current.Job == 's',]
levels(procrastination$Current.Job)[match('s', levels(procrastination$Current.Job))]<-'student'
levels(procrastination$Current.Job)[match('Studey', levels(procrastination$Current.Job))]<-'student'
levels(procrastination$Current.Job)[match('psychologis', levels(procrastination$Current.Job))]<-'psychologist'
levels(procrastination$Current.Job)[match('mktg', levels(procrastination$Current.Job))]<-'Marketing'
levels(procrastination$Current.Job)[match('MD', levels(procrastination$Current.Job))]<-'Physician'
levels(procrastination$Current.Job)[match('Economy', levels(procrastination$Current.Job))]<-'Economist'
levels(procrastination$Current.Job)[match('vidoe', levels(procrastination$Current.Job))]<-'video'
levels(procrastination$Current.Job)[match(' houswife', levels(procrastination$Current.Job))]<-'Housewife'
#gsub('�???"', '', levels(procrastination$Current.Job))
levels(procrastination$Current.Job)[match('\'Utterly shiftless arts student\'... at p', levels(procrastination$Current.Job))]<-'student'
levels(procrastination$Current.Job)[match('asst', levels(procrastination$Current.Job))]<-'Assistant'
#Consolidate Attrorneys
levels(procrastination$Current.Job)[match('attorney', levels(procrastination$Current.Job))]<-'Attorney'
levels(procrastination$Current.Job)[match('Attorney �???" Associate', levels(procrastination$Current.Job))]<-'Attorney'
levels(procrastination$Current.Job)[match('Attorney - self employed for 2 years �???" f', levels(procrastination$Current.Job))]<-'Attorney'
levels(procrastination$Current.Job)[match('Editor Attorney', levels(procrastination$Current.Job))]<-'Attorney'
levels(procrastination$Current.Job)[match('Assistant District Attorney', levels(procrastination$Current.Job))]<-'Attorney'
levels(procrastination$Current.Job)[match(' Attorney-self employed', levels(procrastination$Current.Job))]<-'Attorney'

names(procrastination)[8]<-'Years.Empl.'
#tail(sort(procrastination$Years.Empl.), 50)
#999 years is missing since we don't know what it means
procrastination$Years.Empl.<-ifelse(procrastination$Years.Empl. == 999, NA, procrastination$Years.Empl.)
procrastination$Years.Empl.<-round(procrastination$Years.Empl)

names(procrastination)[9]<-'Months.Empl.'

names(procrastination)[10]<-'Comm.Size'
#CHeck if 8 or 0 corresponds to a category if numeric
#procrastination$Comm.Size[as.numeric(procrastination$Comm.Size) == 8]
#procrastination$Comm.Size[as.numeric(procrastination$Comm.Size) == 0]
#Change to 'Small City based on other responses
levels(procrastination$Comm.Size)[match('8', levels(procrastination$Comm.Size))]<-'Small City'
levels(procrastination$Comm.Size)[match('0', levels(procrastination$Comm.Size))]<-NA

names(procrastination)[11]<-'Country'
levels(procrastination$Country)[match('0', levels(procrastination$Country))]<-NA
levels(procrastination$Country)[match('', levels(procrastination$Country))]<-NA
#levels(procrastination$Country)[match(' ', levels(procrastination$Country))]<-'<NA>'

names(procrastination)[12]<-'Marital.Stat'
levels(procrastination$Marital.Stat)[match('0', levels(procrastination$Marital.Stat))]<-NA

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
levels(procrastination$Other.Assess)[match('0', levels(procrastination$Other.Assess))]<-NA

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
