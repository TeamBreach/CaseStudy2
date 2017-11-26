# Procrastination
Arturo Casillas & Eric McCandless  
November 26, 2017  


## Introduction

Background: 

##Sources/Data

Data files: Raw and URL/Scraped


```r
library(knitr)
#opts_knit$set(root.dir = "~/Documents/")
#getwd()
```


```r
#2.
#read the data
#This assumes that CaseStudy2 is your working directory
#2.a.
procrastination<-read.csv('./Data/Procrastination.csv', header=TRUE)
```

##Additional Information

Additional information about this assessment can be found in the README file in the repository.

##Analysis/Findings

Explore Data:

Dimensions:


```r
#2.b.
#dimensions
dim(procrastination)
```

```
## [1] 4264   61
```

First Look:


```r
#general info
str(procrastination)

#Create 'catch', a vector of boolean if-numeric
catch=NA
for(i in 1:length(procrastination[1,])){catch<-c(catch, is.numeric(procrastination[,i]))}
#catch[-1]

#Numeric Summary
apply(procrastination[,catch[-1]], 2, summary)

#Character Summary
apply(procrastination[,!catch[-1]], 2, unique)
```

Clean data variable by variable:


```r
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
#Set 's' to student based on employment status
levels(procrastination$Current.Job)[match('s', levels(procrastination$Current.Job))]<-'student'
levels(procrastination$Current.Job)[match('0', levels(procrastination$Current.Job))]<-''
#ouh could be oxford university hospital

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
```

Double Check


```r
#Double Check
str(procrastination)
```

```
## 'data.frame':	4264 obs. of  61 variables:
##  $ Age         : num  67.5 45 19 37.5 28 23 67.5 37.5 24 45 ...
##  $ Gender      : Factor w/ 3 levels "","Female","Male": 3 3 2 3 2 2 2 3 2 3 ...
##  $ Kids        : Factor w/ 3 levels "","No Kids","Yes Kids": 3 3 2 3 2 2 2 2 2 3 ...
##  $ Education   : Factor w/ 9 levels "","deg","dip",..: 8 2 3 8 2 2 8 4 8 8 ...
##  $ Work.Status : Factor w/ 6 levels "","full-time",..: 4 3 5 2 2 2 3 3 2 2 ...
##  $ Income.Year : int  25000 35000 NA 45000 35000 15000 NA 10000 250000 87500 ...
##  $ Current.Job : Factor w/ 674 levels "","'Utterly shiftless arts student'... at p",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ Years.Empl. : num  9.0 1.5e-19 0.0 1.4e+01 1.0 ...
##  $ Months.Empl.: int  0 0 0 0 0 0 0 0 0 0 ...
##  $ Comm.Size   : Factor w/ 8 levels "","Small City",..: 3 8 4 4 8 7 4 8 8 8 ...
##  $ Country     : Factor w/ 93 levels "","Afghanistan",..: 30 14 24 25 25 25 59 87 87 72 ...
##  $ Marital.Stat: Factor w/ 6 levels "","Divorced",..: 2 3 5 3 5 5 2 5 5 3 ...
##  $ Sons        : num  0 1 0 0 0 0 0 0 0 2 ...
##  $ Daughters   : int  5 1 0 1 0 0 0 0 0 0 ...
##  $ XDP1        : int  3 3 5 3 3 3 3 4 2 5 ...
##  $ XDP2        : int  1 4 5 3 3 4 4 3 2 5 ...
##  $ XDP3        : int  1 3 2 3 2 3 3 4 4 5 ...
##  $ XDP4        : int  1 3 3 3 1 2 2 4 4 5 ...
##  $ XDP5        : int  1 3 3 3 1 2 2 3 4 5 ...
##  $ XAIP1       : int  1 3 5 2 1 2 3 4 3 3 ...
##  $ XAIP2       : int  1 1 4 1 1 5 1 4 3 3 ...
##  $ XAIP3       : int  1 4 4 4 3 5 1 4 3 5 ...
##  $ XAIP4       : int  1 3 5 3 3 5 2 4 4 3 ...
##  $ XAIP5       : int  1 3 5 5 2 5 3 4 4 5 ...
##  $ XAIP6       : int  1 4 5 3 2 3 3 2 2 1 ...
##  $ XAIP7       : int  1 3 5 4 2 5 1 5 2 5 ...
##  $ XAIP8       : int  1 3 4 5 2 4 4 2 4 5 ...
##  $ XAIP9       : int  5 3 5 4 1 4 5 4 2 5 ...
##  $ XAIP10      : int  1 3 5 5 1 5 5 3 2 5 ...
##  $ XAIP11      : int  1 4 4 4 2 3 3 2 4 4 ...
##  $ XAIP12      : int  1 2 3 3 1 5 2 4 4 4 ...
##  $ XAIP13      : int  1 2 5 4 2 4 3 3 4 4 ...
##  $ XAIP14      : int  1 2 4 2 1 5 1 4 3 4 ...
##  $ XAIP15      : int  3 4 3 1 2 5 4 5 3 5 ...
##  $ XGP1        : int  1 4 5 4 4 5 4 5 3 5 ...
##  $ XGP2        : int  1 2 2 1 1 5 1 1 4 3 ...
##  $ XGP3        : int  1 2 2 3 2 2 1 3 3 3 ...
##  $ XGP4        : int  1 2 4 3 4 5 1 4 4 1 ...
##  $ XGP5        : int  1 2 3 2 5 4 1 3 2 5 ...
##  $ XGP6        : int  1 2 1 3 2 4 2 3 2 5 ...
##  $ XGP7        : int  1 4 3 4 4 5 3 4 4 5 ...
##  $ XGP8        : int  1 2 2 5 2 4 2 3 2 4 ...
##  $ XGP9        : int  1 4 5 4 4 4 4 4 4 5 ...
##  $ XGP10       : int  1 2 4 1 1 3 1 4 1 3 ...
##  $ XGP11       : int  5 3 5 3 2 4 4 3 5 5 ...
##  $ XGP12       : int  1 4 5 4 3 4 2 5 2 5 ...
##  $ XGP13       : int  1 2 3 3 2 3 3 2 3 4 ...
##  $ XGP14       : int  1 2 4 3 4 4 2 2 2 4 ...
##  $ XGP15       : int  1 3 5 4 3 4 4 4 1 4 ...
##  $ XGP16       : int  1 4 2 4 2 4 3 4 5 5 ...
##  $ XGP17       : int  1 3 3 3 3 4 1 3 5 3 ...
##  $ XGP18       : int  5 3 5 4 2 4 4 4 1 5 ...
##  $ XGP19       : int  1 4 5 5 3 4 4 4 1 5 ...
##  $ XGP20       : int  5 4 4 1 4 4 2 4 3 5 ...
##  $ SWLS1       : int  5 3 2 2 4 3 3 3 4 1 ...
##  $ SWLS2       : int  5 4 2 4 4 2 4 3 4 4 ...
##  $ SWLS3       : int  5 4 2 2 4 4 3 3 5 2 ...
##  $ SWLS4       : int  5 4 3 2 3 4 3 2 4 4 ...
##  $ SWLS5       : int  5 3 4 2 4 3 2 3 4 1 ...
##  $ Self.Assess : Factor w/ 3 levels "","no","yes": 2 3 3 3 2 3 3 3 2 3 ...
##  $ Other.Assess: Factor w/ 3 levels "","no","yes": 2 3 3 3 2 3 3 3 2 3 ...
```

```r
#apply(procrastination, 2, class)
catch=NA
for(i in 1:length(procrastination[1,])){catch<-c(catch, is.numeric(procrastination[,i]))}
#catch[-1]

#Numeric Summary
#apply(procrastination[,c(1,6,8:9, 14:59)], 2, summary)
apply(procrastination[,catch[-1]], 2, summary)
```

```
## $Age
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    7.50   28.00   32.50   37.43   45.00   80.00      71 
## 
## $Income.Year
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##       0   15000   45000   58916   67500  250000     548 
## 
## $Years.Empl.
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   0.000   2.000   4.134   6.000  30.000     136 
## 
## $Months.Empl.
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   0.000   0.000   1.793   3.000  11.000       6 
## 
## $Sons
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.3552  0.0000 10.0000       4 
## 
## $Daughters
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.3538  0.0000 10.0000       4 
## 
## $XDP1
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   3.000   3.000   3.392   4.000   5.000 
## 
## $XDP2
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   3.000   3.000   3.354   4.000   5.000 
## 
## $XDP3
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.000   3.000   3.198   4.000   5.000 
## 
## $XDP4
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.000   3.000   2.688   3.000   5.000 
## 
## $XDP5
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.000   3.000   2.678   3.000   5.000 
## 
## $XAIP1
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   1.000   2.000   2.054   3.000   5.000 
## 
## $XAIP2
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   1.000   2.000   2.161   3.000   5.000 
## 
## $XAIP3
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.000   3.000   3.371   5.000   5.000 
## 
## $XAIP4
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.000   3.000   3.206   4.000   5.000 
## 
## $XAIP5
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.000   3.000   3.068   4.000   5.000 
## 
## $XAIP6
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.000   3.000   2.843   4.000   5.000 
## 
## $XAIP7
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.000   3.000   3.218   4.000   5.000 
## 
## $XAIP8
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   3.000   3.000   3.421   4.000   5.000 
## 
## $XAIP9
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.000   3.000   2.757   4.000   5.000 
## 
## $XAIP10
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   3.000   4.000   3.468   4.000   5.000 
## 
## $XAIP11
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.000   3.000   3.045   4.000   5.000 
## 
## $XAIP12
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.000   3.000   2.981   4.000   5.000 
## 
## $XAIP13
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.000   3.000   3.118   4.000   5.000 
## 
## $XAIP14
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.000   2.000   2.658   4.000   5.000 
## 
## $XAIP15
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.000   3.000   3.193   4.000   5.000 
## 
## $XGP1
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   3.000   4.000   3.671   5.000   5.000 
## 
## $XGP2
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   1.000   2.000   2.307   3.000   5.000 
## 
## $XGP3
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.000   3.000   2.774   4.000   5.000 
## 
## $XGP4
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.000   3.000   3.321   5.000   5.000 
## 
## $XGP5
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.000   3.000   3.022   4.000   5.000 
## 
## $XGP6
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.000   3.000   2.802   4.000   5.000 
## 
## $XGP7
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   3.000   3.000   3.382   4.000   5.000 
## 
## $XGP8
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    1.00    3.00    3.00    3.27    4.00    5.00 
## 
## $XGP9
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   3.000   4.000   3.749   5.000   5.000 
## 
## $XGP10
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     1.0     2.0     3.0     2.7     4.0     5.0 
## 
## $XGP11
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.000   3.000   3.257   4.000   5.000 
## 
## $XGP12
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   3.000   4.000   3.682   4.000   5.000 
## 
## $XGP13
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.000   3.000   2.687   4.000   5.000 
## 
## $XGP14
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.000   3.000   3.095   4.000   5.000 
## 
## $XGP15
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   3.000   4.000   3.567   4.000   5.000 
## 
## $XGP16
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   3.000   4.000   3.663   5.000   5.000 
## 
## $XGP17
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.000   3.000   3.178   4.000   5.000 
## 
## $XGP18
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   3.000   4.000   3.634   4.000   5.000 
## 
## $XGP19
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   3.000   4.000   3.589   5.000   5.000 
## 
## $XGP20
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   3.000   4.000   3.531   4.000   5.000 
## 
## $SWLS1
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.000   3.000   2.943   4.000   5.000 
## 
## $SWLS2
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   3.000   3.000   3.199   4.000   5.000 
## 
## $SWLS3
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    1.00    2.00    3.00    3.11    4.00    5.00 
## 
## $SWLS4
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.000   3.000   3.244   4.000   5.000 
## 
## $SWLS5
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.000   3.000   2.689   4.000   5.000
```

```r
#Character Summary
#apply(procrastination[,-c(1,6,8:9, 14:59)], 2, unique)
apply(procrastination[,!catch[-1]], 2, unique)
```

```
## $Gender
## [1] "Male"   "Female" ""      
## 
## $Kids
## [1] "Yes Kids" "No Kids"  ""        
## 
## $Education
## [1] "ma"     "deg"    "dip"    "grade"  "phd"    "ltuni"  "high"   "lthigh"
## [9] ""      
## 
## $Work.Status
## [1] "retired"    "part-time"  "student"    "full-time"  "unemployed"
## [6] ""          
## 
## $Current.Job
##   [1] ""                                          
##   [2] "financial analyst"                         
##   [3] "language trainer"                          
##   [4] "Editor"                                    
##   [5] "ouh"                                       
##   [6] "retired"                                   
##   [7] "Software Sales"                            
##   [8] "Civil servant"                             
##   [9] "please specify"                            
##  [10] "Economy"                                   
##  [11] "sales manager"                             
##  [12] "manager"                                   
##  [13] "BUSINESS CONSULTA"                         
##  [14] "Copy Writer"                               
##  [15] "Gender/Public Health Consultant"           
##  [16] "chairman of the board"                     
##  [17] "Media Consultant"                          
##  [18] "Social Policy Analyst"                     
##  [19] "Finance"                                   
##  [20] "Translator"                                
##  [21] "Technical director"                        
##  [22] "audio engineer"                            
##  [23] "Research Associate"                        
##  [24] "Controller"                                
##  [25] "businesswoman"                             
##  [26] "Financial Advisor"                         
##  [27] "IT"                                        
##  [28] "Programmer"                                
##  [29] "airport ground handler"                    
##  [30] "Computer Programmer"                       
##  [31] " teacher"                                  
##  [32] "Computer Science"                          
##  [33] "Unemployed"                                
##  [34] "EFL Teacher/ Professional Researcher"      
##  [35] "Doctor; Physician"                         
##  [36] "Software Developer"                        
##  [37] "Attorney"                                  
##  [38] "secretary"                                 
##  [39] "journalist (freelance)"                    
##  [40] "nursing home"                              
##  [41] "Developer"                                 
##  [42] "Director of a language program"            
##  [43] "Graphic Designer"                          
##  [44] "system manager"                            
##  [45] "ICT Director"                              
##  [46] "Chiefe Development Engineer"               
##  [47] " consultant"                               
##  [48] "Assoc. Governmental Program Analyst"       
##  [49] " houswife"                                 
##  [50] "Administrator"                             
##  [51] "IT systems administrator"                  
##  [52] "Computer Consultant"                       
##  [53] "student fysiotherapy /home care / massage" 
##  [54] "Astrohysicist"                             
##  [55] "Student and part time secretary"           
##  [56] "Journalist"                                
##  [57] "lecturer"                                  
##  [58] " Teaching Assistant/Graduate student"      
##  [59] "Business Owner"                            
##  [60] "entrepreneur"                              
##  [61] "free professionist"                        
##  [62] "Agronomist"                                
##  [63] "college professor"                         
##  [64] "dentist"                                   
##  [65] "director"                                  
##  [66] "Financial Controller"                      
##  [67] "attorney"                                  
##  [68] "President Nongovernmental organization"    
##  [69] "Dance teacher"                             
##  [70] "coordinatore operativo"                    
##  [71] "interpreter"                               
##  [72] "self-employed translator"                  
##  [73] "Research Scientist"                        
##  [74] "Disability Allowance"                      
##  [75] "Sales Expert"                              
##  [76] "Postdoc"                                   
##  [77] "Physicist"                                 
##  [78] "Sales"                                     
##  [79] "engineer"                                  
##  [80] "Musician"                                  
##  [81] "System Analyst"                            
##  [82] "trainee"                                   
##  [83] "Web Designer"                              
##  [84] "Dish Washer"                               
##  [85] "ESL Teacher/Biologist"                     
##  [86] "Sales executive"                           
##  [87] "Freelance ESL Teacher"                     
##  [88] "Information Developer"                     
##  [89] "IT admin"                                  
##  [90] "Network Engineer"                          
##  [91] "student childhood and youth studies"       
##  [92] "Quality Manager"                           
##  [93] "Town Planner"                              
##  [94] "assistant professor"                       
##  [95] "business"                                  
##  [96] "civil servant"                             
##  [97] "Technical support rep"                     
##  [98] "Programmer/Software Analyst"               
##  [99] "Project Manager"                           
## [100] "company director"                          
## [101] "Customer Service"                          
## [102] "cataloguer /  freelance artist"            
## [103] "deputy practice manager"                   
## [104] "Architect"                                 
## [105] "Analyst"                                   
## [106] "IT Support"                                
## [107] "Receptionist"                              
## [108] "treatment support co-ordinator"            
## [109] "producer"                                  
## [110] "Academic"                                  
## [111] "Postdoctoral Researcher"                   
## [112] "student/barmaid"                           
## [113] "airline"                                   
## [114] "Creative Consultant"                       
## [115] "Business / Test Analyst"                   
## [116] "IT security consultant"                    
## [117] "Executive officer"                         
## [118] "IT consultant"                             
## [119] "IT Support Engineer"                       
## [120] "Doctor Research"                           
## [121] "information assisstant"                    
## [122] "Television Producer"                       
## [123] "Press Officer"                             
## [124] "writer"                                    
## [125] "Procrastinator"                            
## [126] "Professional Soccer Player"                
## [127] "fdsdf"                                     
## [128] " Accountant"                               
## [129] "Pharmaceutical Merchandiser"               
## [130] "Education (at a university)"               
## [131] "IT Consultant"                             
## [132] "energy therapist"                          
## [133] "psychologis"                               
## [134] "HR generalist"                             
## [135] "Physiotherapst"                            
## [136] "Technical Writer"                          
## [137] "Market Analyst"                            
## [138] "Office Manager / Accountant"               
## [139] "Administration Assistant"                  
## [140] "IT Manager"                                
## [141] "Internet & media consultant"               
## [142] "Information Technology Consultant"         
## [143] "Housekeeping"                              
## [144] "Web Communications"                        
## [145] "Medical Practitioner"                      
## [146] " veterinarian"                             
## [147] "Management Consultant & Entrepreneur"      
## [148] "Technical officer"                         
## [149] "Service co-ordinator"                      
## [150] "Clutter clearer,  video editor, caterer"   
## [151] "entertainer"                               
## [152] "Scientist"                                 
## [153] "clerk"                                     
## [154] "Self Employed"                             
## [155] "President"                                 
## [156] "pathology"                                 
## [157] "Divisional Manager of a large cosmetics"   
## [158] "Marketing"                                 
## [159] "Driver"                                    
## [160] "Director Operations"                       
## [161] "Nurse"                                     
## [162] "Sales Rep"                                 
## [163] "Fitness Instructor"                        
## [164] "Residence Don"                             
## [165] "Regional Sales Manager"                    
## [166] "Country Style Employee"                    
## [167] "Systems Analyst"                           
## [168] "Facilitator"                               
## [169] "self-employed Photographer"                
## [170] "Research Assistant"                        
## [171] " Military"                                 
## [172] "policy analyst"                            
## [173] "Geologist"                                 
## [174] "Instructional Assistant Online"            
## [175] "Server"                                    
## [176] " innkeeper"                                
## [177] "Freelance Project Manager"                 
## [178] "Deputy Chieif Information Officer"         
## [179] "selfemplyed renovator"                     
## [180] "Accounting"                                
## [181] "Systems Programmer/Analyst"                
## [182] "Manager - Analytical and Environmental S"  
## [183] "Grease Monkey"                             
## [184] "Director of Software Company"              
## [185] "Speaker Author Consultant"                 
## [186] "Janitor"                                   
## [187] "vice-president"                            
## [188] "Senior Human Resources Consultant"         
## [189] "Communications & Publishing"               
## [190] "Artist/ designer/builder"                  
## [191] "Director,social Dvelopment"                
## [192] "Senior Systems Analyst"                    
## [193] "Software engineer"                         
## [194] "Training Coordinator"                      
## [195] "Statistician"                              
## [196] "Student / working part-time"               
## [197] "Computer Instructor (Continuing Educatio"  
## [198] "Counselor"                                 
## [199] "na"                                        
## [200] "Program Manager and Acting Director"       
## [201] "Biologist"                                 
## [202] "pro poker player /   website owner"        
## [203] "VP Scientific Affairs / pharmaceutical c"  
## [204] "Deputy Director"                           
## [205] "operations manager"                        
## [206] "C E O/ M D"                                
## [207] "Head - Operations & QA"                    
## [208] "Software Pro"                              
## [209] "tax consultant"                            
## [210] "Research Scholar"                          
## [211] "student"                                   
## [212] "Collection management specialist"          
## [213] "project manager"                           
## [214] "Lift Ops"                                  
## [215] "Self-employed Family Therapist"            
## [216] "special education teacher"                 
## [217] "Camera Coordinator"                        
## [218] "Designer"                                  
## [219] "Produce Associate"                         
## [220] "supervisor shelderd workshop for handcap"  
## [221] "Braillist"                                 
## [222] "election services"                         
## [223] "Corrections"                               
## [224] "Probation Supervisor"                      
## [225] "Bank Teller"                               
## [226] "Computer Operator"                         
## [227] "Case Manager"                              
## [228] "LPN"                                       
## [229] "Massage Therapist"                         
## [230] "vice president"                            
## [231] "Traffic Reporter-Radio"                    
## [232] "Residential Services Supervisor"           
## [233] "Language Service Provider"                 
## [234] "Executive"                                 
## [235] "Lab Services Assistant"                    
## [236] "ceo"                                       
## [237] "Film Industry/Miscelanious"                
## [238] " acupuncturist"                            
## [239] "Technology Curriculum Developer Science"   
## [240] "home maker"                                
## [241] "Freelance"                                 
## [242] "Insurance Agent"                           
## [243] "real estate"                               
## [244] "RN"                                        
## [245] "associate"                                 
## [246] "Restaurant operations manager"             
## [247] "Labor Relations Specialist"                
## [248] "TV News Executive Producer"                
## [249] "Librarian"                                 
## [250] "First VP & Associate General Counsel"      
## [251] "Coordinator of International Programs"     
## [252] "Chief of Staff"                            
## [253] "yoga teacher"                              
## [254] "Volunteer Director"                        
## [255] "Pastor ; Life coach  clergy"               
## [256] "Research Analyst"                          
## [257] "electronic technician"                     
## [258] "Associate Producer"                        
## [259] "VMD"                                       
## [260] "abc"                                       
## [261] "media relations/science writing"           
## [262] "buyer"                                     
## [263] "Owner"                                     
## [264] "University Staff"                          
## [265] "landscape designer"                        
## [266] "Network Services Engineer"                 
## [267] "Special Projects Editor"                   
## [268] "free lance bookkeeper"                     
## [269] "Urban Planner/Economic Development Plann"  
## [270] "Executive Director"                        
## [271] "marketing copywriter"                      
## [272] "Economist"                                 
## [273] "Studey"                                    
## [274] " bookseller"                               
## [275] " Assistant"                                
## [276] "Professional Organizer"                    
## [277] "Outdoor Recreation Coordinator"            
## [278] "Attorney â<U+0080><U+0093> Associate"                    
## [279] "Co-Proprietor"                             
## [280] "Antique Dealer"                            
## [281] "Market Research Analyst"                   
## [282] "law enforcement"                           
## [283] "insurance agent"                           
## [284] "Parent Educator/Supervisor"                
## [285] "Legal Assistant / Office Manager"          
## [286] "museum docent"                             
## [287] "Publishing"                                
## [288] "maintenance tech."                         
## [289] "Research intern"                           
## [290] "employed by a church"                      
## [291] "RN - Medical Sales"                        
## [292] "Client Relationship Assistant"             
## [293] " Communications"                           
## [294] "copy supervisor"                           
## [295] "Data Warehouse Engineer"                   
## [296] "Dental & Disability Coordinator"           
## [297] "writer/editor"                             
## [298] "Nanny and student"                         
## [299] "Program Coordinator"                       
## [300] "Social Worker"                             
## [301] "training Coordinator"                      
## [302] "EOD"                                       
## [303] "bookkeeper"                                
## [304] "Tutor"                                     
## [305] "host"                                      
## [306] "Human Resource Manager"                    
## [307] "owner - private practice physical therap"  
## [308] "advocate"                                  
## [309] "Aviation Specialist"                       
## [310] "CRNA"                                      
## [311] "education administration"                  
## [312] "Customer Service at Domino's Pizza"        
## [313] "Writer / eductor"                          
## [314] "Accounting Assistant"                      
## [315] "Program Director"                          
## [316] "Tech Analyst/GIS"                          
## [317] "jewelry artist"                            
## [318] "real estate agent"                         
## [319] "IT director"                               
## [320] "Investment Counsel"                        
## [321] "Food Service Supervisor"                   
## [322] "Environmental Senior Specialist"           
## [323] "research coordinator"                      
## [324] " Diplomat"                                 
## [325] "Stay-at-home dad"                          
## [326] "Theater General Manager"                   
## [327] "Graduate student/University instructor"    
## [328] "Field Coordinator"                         
## [329] "laboratory technician"                     
## [330] "Box Office Representative"                 
## [331] "Facilities Management"                     
## [332] "Freelance musician / part time EMT / pri"  
## [333] "Dietitian"                                 
## [334] "realtor"                                   
## [335] "Information Management"                    
## [336] "Office Services Manager"                   
## [337] "Juvenile Corrections Officer"              
## [338] "stocker"                                   
## [339] "Account Service Rep"                       
## [340] "intern"                                    
## [341] "set designer"                              
## [342] "Groundskeeper"                             
## [343] "library paraprofessional"                  
## [344] "Ecology technician"                        
## [345] "Artist"                                    
## [346] "Consultant and entrepreneur (small busin"  
## [347] "Food Department Director"                  
## [348] "Graduate Research Assistant"               
## [349] "psychotherapist"                           
## [350] "In-house Legal Counsel"                    
## [351] "Writer and management consultant"          
## [352] "Senior Consultant Programmer/Analyst"      
## [353] "EMT"                                       
## [354] "Legislation Analyst"                       
## [355] "public relations"                          
## [356] "sales insurance"                           
## [357] "Writer/editor/musician"                    
## [358] " designer"                                 
## [359] "Legal Assistant"                           
## [360] "Proposal Director"                         
## [361] "real estate broker"                        
## [362] "Vice President / program office"           
## [363] "regulatory affairs"                        
## [364] "Associate Director"                        
## [365] "wig designer"                              
## [366] "Campus Planner"                            
## [367] "Administrative Officer"                    
## [368] "phd student researcher"                    
## [369] "Teacher assistant"                         
## [370] "Senior Records Analyst"                    
## [371] "musician/student/teacher"                  
## [372] "Teacher and Full Time Doctoral Student"    
## [373] "health care"                               
## [374] "Multimedia Developer"                      
## [375] "Executive Assistant"                       
## [376] "Insurance Coordinator"                     
## [377] "Speaker/Actor"                             
## [378] "Retail / artist /writer"                   
## [379] "Artist/administrator"                      
## [380] "Social Media consultant"                   
## [381] "Pharmacist"                                
## [382] "Program Specialist"                        
## [383] "Science writing intern"                    
## [384] " school"                                   
## [385] "research specialist"                       
## [386] "manager IT"                                
## [387] "Nanny"                                     
## [388] " Epidemiologist"                           
## [389] "HVAC Tech"                                 
## [390] "Accounting Manager"                        
## [391] "Self-employed writer/editor"               
## [392] "Healthcare Consultant"                     
## [393] "researcher"                                
## [394] "chiropractor"                              
## [395] "college faculty"                           
## [396] "Information technology"                    
## [397] "P-T College Faculty & P-T Self-Employed"   
## [398] "Respiratory Therapist"                     
## [399] "Insurance"                                 
## [400] "Office Manager"                            
## [401] " admin assist"                             
## [402] "Warehouse Supervisor"                      
## [403] "Director of non-profit organization"       
## [404] "Sr. Drug Safety Associate"                 
## [405] "Electrical Technician"                     
## [406] "Senior Staff Writer"                       
## [407] "Writing Consultant"                        
## [408] "federal excise tax auditor"                
## [409] "retired/adjunct"                           
## [410] "television director"                       
## [411] "quad racer"                                
## [412] " university faculty"                       
## [413] "IT Assistant"                              
## [414] " anthropologist"                           
## [415] "Environmental Analyst"                     
## [416] "Post Grad Physician"                       
## [417] "writer/musician"                           
## [418] "Staff Writer at a magazine"                
## [419] "Research / GIS analyst"                    
## [420] "environmental education non profit direc"  
## [421] "technical writer"                          
## [422] "IT analyst"                                
## [423] "Full-Time Mother / Part-Time Editor"       
## [424] "Casting Director"                          
## [425] "Financial Consultant"                      
## [426] "Real estate developer"                     
## [427] "Tech Support"                              
## [428] "pr and communications firm owner"          
## [429] "Library technician"                        
## [430] "Surgical Resident"                         
## [431] "Grants Administrator"                      
## [432] "Senior Grant Officer"                      
## [433] "Student part-time and sales full-time"     
## [434] "Psychiatrist in Private Practice"          
## [435] "Office Admin"                              
## [436] "School Counselor"                          
## [437] "new realtor"                               
## [438] "Medical / Public Health Educator"          
## [439] "self-employed freelance writer/author"     
## [440] "self employed"                             
## [441] "student/imvestor"                          
## [442] "Social Work Intern"                        
## [443] "Administrative Asistant for Jewelry Stor"  
## [444] "Computers"                                 
## [445] "Bar & Restaurant Owner"                    
## [446] "Licensed Professional Counselor"           
## [447] "supervising program development speciali"  
## [448] "Surgeon"                                   
## [449] "Programmer/Developer"                      
## [450] "Master Control Operator"                   
## [451] "Test Item Writer (Self-employed)"          
## [452] "Secretary"                                 
## [453] "Hotel Desk Clerk"                          
## [454] "Actress"                                   
## [455] "Accounts Payable"                          
## [456] "medical transcriptionist"                  
## [457] "Self-Employed / personal trainer / stren"  
## [458] "Director / information technology"         
## [459] "Retail"                                    
## [460] "rocket scientist"                          
## [461] "manufacturing"                             
## [462] "Paralegal"                                 
## [463] "Capstone Golf Course"                      
## [464] "laborer (construction)"                    
## [465] "temporary office"                          
## [466] "Student and Private Curator"               
## [467] "Graduate Assistant"                        
## [468] "Programmer Analyst"                        
## [469] "Town Clerk"                                
## [470] "Consulting Manager"                        
## [471] "Investigative Specialist"                  
## [472] "writer / lecturer / consultant"            
## [473] "md"                                        
## [474] "Asst. Prof."                               
## [475] "EHS Manager"                               
## [476] "Program Manager"                           
## [477] "Ornithology Graduate Student and Teachin"  
## [478] " Research/Teaching Assistant"              
## [479] "Dealer"                                    
## [480] "Mentor/Special Events intern"              
## [481] "IT Technician"                             
## [482] "Real Estate Appraiser"                     
## [483] "Attorney - self employed for 2 years â<U+0080><U+0093> f"
## [484] "Tour Guide"                                
## [485] "Graduate student--research and teaching"   
## [486] "Technical Trainer"                         
## [487] "catholic priest/ full timestudent"         
## [488] "Geophysicist"                              
## [489] "ISTraining Coordinator"                    
## [490] "set lighting technician"                   
## [491] "senior project manager"                    
## [492] "enologist"                                 
## [493] "Registered Respiratory Therapist"          
## [494] "Framer/Sales Associate"                    
## [495] "Fitness Assistant / wellness mentor / ca"  
## [496] "visual artist"                             
## [497] "newspaper carrier"                         
## [498] "Consumer Case Coordinator"                 
## [499] "business consultant"                       
## [500] "Medical"                                   
## [501] "Manager,Interacitve Media"                 
## [502] "Art Director"                              
## [503] "Telemarketer"                              
## [504] "Partner"                                   
## [505] "Associate / investment banking"            
## [506] "trader"                                    
## [507] "Technical Coordinator"                     
## [508] "financial officer / small career-trainin"  
## [509] "artist/designer/homemaker"                 
## [510] "Special Education Administrative Assista"  
## [511] "assistant general counsel"                 
## [512] "Private Equity Principal"                  
## [513] "Creative Director"                         
## [514] "Law clerk"                                 
## [515] "president/CEO"                             
## [516] "IT Director"                               
## [517] "Program director at a non-profit organiz"  
## [518] "Tax Examiner"                              
## [519] "IT Engineer"                               
## [520] "senior consultant"                         
## [521] "Physical Science Technician"               
## [522] "letter carrier"                            
## [523] "teacher's assistant/afterschool leader"    
## [524] "film editor"                               
## [525] "insurance broker's assistant"              
## [526] "Software trainer"                          
## [527] "self employeed"                            
## [528] "Shipping/receiving/warehouse mgnt"         
## [529] "Senior Policy Advisor"                     
## [530] "medical sonographer"                       
## [531] "Certified Nurse's Assistant"               
## [532] "Asst. Pre-school Teacher"                  
## [533] "Director of Contract Management"           
## [534] "analyst"                                   
## [535] "financial risk manager"                    
## [536] "office"                                    
## [537] "Self employed Public Relations"            
## [538] " Attorney-self employed"                   
## [539] "Technology (CTO)"                          
## [540] "associate at law firm"                     
## [541] "Please specify title Manager for Regulat"  
## [542] "Account Manager"                           
## [543] "Dept. Director (Non-profit)"               
## [544] "Mechanical Engineer"                       
## [545] "Film maker"                                
## [546] "Supervisor"                                
## [547] "Foreign Affairs Specialist"                
## [548] "bookkeeper/ actor"                         
## [549] "Investment Assistant"                      
## [550] "restaurant mgr / student / and looking f"  
## [551] "Life Guard"                                
## [552] "detail checker"                            
## [553] "Student/Teacher"                           
## [554] "Graduate Researcher"                       
## [555] "PCA for a quadrapilegic and a PCA for a"   
## [556] "PCA"                                       
## [557] "College Administrator"                     
## [558] "IT Specialist"                             
## [559] "Academic Assistant"                        
## [560] "'Utterly shiftless arts student'... at p"  
## [561] "Human Resource Manger"                     
## [562] "volunteer mental health worker"            
## [563] "Media Relations Manager"                   
## [564] "Research manager"                          
## [565] "Medical Laboratory"                        
## [566] "Financial Analyst"                         
## [567] "Budget analyst"                            
## [568] "resident physician"                        
## [569] "Program Assistant"                         
## [570] "Program officer"                           
## [571] "Deputy Chief of Public Information for t"  
## [572] "Public Health"                             
## [573] "Page Designer for a newspaper"             
## [574] "flight surgeon"                            
## [575] "Accounts Payable / Fleet Manager"          
## [576] "Theater artist/ Teacher"                   
## [577] "Clinical Dietitian"                        
## [578] "Entrepreneur & Consultant"                 
## [579] "Process Engineer"                          
## [580] "Clinical Research Assistant"               
## [581] "Non-profit Consultant"                     
## [582] "student/waiter"                            
## [583] "Early Childhood Education Student/ Nanny"  
## [584] "Writer & Director of Content Solutions"    
## [585] "Recreational Staff"                        
## [586] "Organic Grocery Store Cashier/shift lead"  
## [587] "banker"                                    
## [588] "Senate Page"                               
## [589] "Online Media Buyer"                        
## [590] "Free lance editor and tutor--in theory"    
## [591] "instructor / coach"                        
## [592] "land use planner"                          
## [593] "Contsuruction Management"                  
## [594] "Bartender"                                 
## [595] "full time student and part time bartende"  
## [596] "Sales/ daycare worker"                     
## [597] "education"                                 
## [598] "CAD Technician"                            
## [599] "Internship"                                
## [600] "Paraprofessional"                          
## [601] "Vetrans Representative"                    
## [602] "Researcher - Physician"                    
## [603] "Environmental Engineer"                    
## [604] "Writer / web designer/ web-master"         
## [605] "hostess"                                   
## [606] "business manager"                          
## [607] "Mover"                                     
## [608] "Library Assistant"                         
## [609] "Product Field Test Manager"                
## [610] "TV BROADCAST TECHNICIAN"                   
## [611] "fulltime office assistant"                 
## [612] "Night Dispatch Supervisor  (it's just a"   
## [613] "Corporate instructor"                      
## [614] "Editor Attorney"                           
## [615] "asst"                                      
## [616] "Reasearch assistant"                       
## [617] "investment banker"                         
## [618] "physician (internist)"                     
## [619] "Assistant District Attorney"               
## [620] "clinical psychologist"                     
## [621] "Service Registrar/English Instructor"      
## [622] "Management consultant"                     
## [623] "Webmaster / Print Designer"                
## [624] "account planner"                           
## [625] "acounting analyst"                         
## [626] "Legal Secretary"                           
## [627] "pjublic relations director"                
## [628] "research technician"                       
## [629] "Associate director/ marketing communicat"  
## [630] "Director of business development"          
## [631] "mktg"                                      
## [632] "furniture maker, home restorer"            
## [633] "Chief Financial Officer"                   
## [634] "Activities Leader"                         
## [635] "Quotations specialist"                     
## [636] "Early child hood teacher"                  
## [637] "pharmacy tech."                            
## [638] "Speech and language Assistant"             
## [639] "Assistant Professor"                       
## [640] "adjunct faculty / University + communit"   
## [641] "student/retail"                            
## [642] "Career Placement Associate"                
## [643] "steamship agent"                           
## [644] "Education Specialist"                      
## [645] "Clinical Trial Assistant"                  
## [646] "Computer Systems Analyst"                  
## [647] "IT Administrator"                          
## [648] "Executive Vice President / Senior Lender"  
## [649] "Director of Academic Affairs"              
## [650] "Lab Director/Archeologist"                 
## [651] "Temp"                                      
## [652] "Production Operations Support Analyst"     
## [653] "Educator/Student"                          
## [654] "Student and Administrative Assistant"      
## [655] "CAD operator"                              
## [656] "Plant Engineering Supervisor"              
## [657] "Insurance Claims Supervisor"               
## [658] "chauffeur"                                 
## [659] "Corporation President"                     
## [660] "academic/career coach & admin assistant"   
## [661] " specialist"                               
## [662] "Proofreader"                               
## [663] "Business Systems Analyst"                  
## [664] "Corporate Trainer"                         
## [665] "Farm Manager"                              
## [666] "adult care"                                
## [667] "Software analyst"                          
## [668] "teacher / Administrator"                   
## [669] "Gove service"                              
## [670] "warehouse"                                 
## [671] "vidoe"                                     
## [672] "Grocery Store Salesman"                    
## [673] "Doctoral Candidate!!!  no wonder i'm doi"  
## [674] "photo profucer"                            
## 
## $Comm.Size
## [1] "Large-City"    "Village"       "Large Town"    "Small Town"   
## [5] "Rural/Country" "Medium-Sized"  "Small City"    ""             
## 
## $Country
##  [1] "El Salvador"        "Bolivia"            "Cyprus"            
##  [4] "Czech Republic"     "Morocco"            "Ukraine"           
##  [7] "Qatar"              "Antigua"            "Vietnam"           
## [10] "Lithuania"          "Saudi Arabia"       "Bulgaria"          
## [13] "Macedonia"          "Sri Lanka"          "Ecuador"           
## [16] "Afghanistan"        "Panama"             "Guam"              
## [19] "Pakistan"           "Estonia"            "Kenya"             
## [22] "Guyana"             "Puerto Rico"        "Bermuda"           
## [25] "Croatia"            "Taiwan"             "Hungary"           
## [28] "Austria"            "Brunei"             "Kazakhstan"        
## [31] "Malta"              "Luxembourg"         "Lebanon"           
## [34] "Singapore"          "Venezuela"          "Uruguay"           
## [37] "Argentina"          "Egypt"              "Nicaragua"         
## [40] "Myanmar"            "Peru"               "Albania"           
## [43] "Dominican Republic" "Andorra"            "Slovenia"          
## [46] "Russia"             "Barbados"           "Philippines"       
## [49] "Malaysia"           "Greece"             "Denmark"           
## [52] "Spain"              "Chile"              "Finland"           
## [55] "Macao"              "New Zealand"        "Yugoslavia"        
## [58] "Romania"            "Iceland"            "Portugal"          
## [61] "Thailand"           "Columbia"           "China"             
## [64] "Ghana"              "Jamaica"            "Norway"            
## [67] "Algeria"            "Netherlands"        "Belgium"           
## [70] "Japan"              "Isreal"             "Turkey"            
## [73] "Sweden"             "Italy"              "Hong Kong"         
## [76] "Ireland"            "France"             "Brazil"            
## [79] "Iran"               "Mexico"             "Germany"           
## [82] "United Kingdom"     "Switzerland"        "Bahamas"           
## [85] "South Africa"       "Poland"             "Australia"         
## [88] "South Korea"        "Botswana"           "Canada"            
## [91] "India"              "United States"      ""                  
## 
## $Marital.Stat
## [1] "Divorced"  "Married"   "Single"    "Separated" "Widowed"   ""         
## 
## $Self.Assess
## [1] "no"  "yes" ""   
## 
## $Other.Assess
## [1] "no"  "yes" ""
```

Highlights:




###Modifications/Merges to Data

###Data Summary

##Conclusions



