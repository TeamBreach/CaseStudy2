####### DEEPER ANALYSIS WITH GRAPHS AND TABLES #######

###5.b.
### Below we plot a bargraph of the top 15 Nations with the highest Decisional Procrastination Score
#table that summarizes DP means by country.
DP_Mean <- procrast_hdi1[,c("Country", "XDP.Mean")]
DP_Mean <- aggregate(DP_Mean[, c("XDP.Mean")], list(DP_Mean$Country), mean)
colnames(DP_Mean) <- c("Country", "XDP.Mean")

#second table that shows development level by country.
DP_Mean1 <- procrast_hdi1[,c("Country","Development_Level", "HDI")]
DP_Mean1 <- unique(DP_Mean1)

#merging 2 tables to get DP and development category together in df.
DP_Meanfinal <- merge(DP_Mean, DP_Mean1, by=c("Country"))
DP_Meanfinal = DP_Meanfinal[-1,]
DP_Meanfinal <- DP_Meanfinal[order(-DP_Meanfinal$XDP.Mean), ,drop = FALSE]
DP_Meanfinal <- head(DP_Meanfinal,15)
DP_Meanfinal$XDP.Mean <- round(DP_Meanfinal$XDP.Mean, digits=1)
DP_Meanfinal$Development_Level <- as.character(DP_Meanfinal$Development_Level)
DP_Meanfinal$Development_Level[is.na(DP_Meanfinal$Development_Level)] <- "No Development Level Reported"


#creating bar plot.
ggplot(DP_Meanfinal, aes(x=reorder(Country,XDP.Mean),y=XDP.Mean)) +
  geom_bar(stat="identity", aes(fill=Development_Level)) +
  theme_minimal() + 
  ggtitle("Top 15 Nations in Average DP Procrastination Scores") +
  theme(plot.title = element_text(hjust = 0.5, size=20), axis.text=element_text(size=15), axis.title=element_text(size=20), legend.title=element_text(size=20), legend.text=element_text(size=15)) +
  xlab("Country of Residence") +
  ylab("Mean DP Response") +
  geom_text(aes(label = XDP.Mean), hjust=-.25, size=5) +
  scale_fill_manual(values = c('#39ac73', '#c6ecd9', '#206040')) +  
  coord_flip()

###

#5.c.
### Below we plot a bargraph of the top 15 Nations with the highest General Procrastination Score
#table that summarizes DP means by country.
#merging 2 tables to get GP and development category together in df.
GP_Mean <- procrast_hdi1[,c("Country", "XGP.Mean")]
GP_Mean <- aggregate(GP_Mean[, c("XGP.Mean")], list(GP_Mean$Country), mean)
colnames(GP_Mean) <- c("Country", "XGP.Mean")
#second table that shows development level by country.
GP_Mean1 <- procrast_hdi1[,c("Country","Development_Level", "HDI")]
GP_Mean1 <- unique(GP_Mean1)
#merging 2 tables to get DP and development category together in df.
GP_Meanfinal <- merge(GP_Mean, GP_Mean1, by=c("Country"))
GP_Meanfinal = GP_Meanfinal[-1,]
GP_Meanfinal <- GP_Meanfinal[order(-GP_Meanfinal$XGP.Mean), ,drop = FALSE]
GP_Meanfinal <- head(GP_Meanfinal,15)
GP_Meanfinal$XGP.Mean <- round(GP_Meanfinal$XGP.Mean, digits=1)
GP_Meanfinal$Development_Level <- as.character(GP_Meanfinal$Development_Level)
GP_Meanfinal$Development_Level[is.na(GP_Meanfinal$Development_Level)] <- "No Development Level Reported"
#creating bar plot.
ggplot(GP_Meanfinal, aes(x=reorder(Country,XGP.Mean),y=XGP.Mean)) +
  geom_bar(stat="identity", aes(fill=Development_Level)) +
  theme_minimal() +
  ggtitle("Top 15 Nations in Average GP Procrastination Scores") +
  theme(plot.title = element_text(hjust = 0.5, size=20), axis.text=element_text(size=15), axis.title=element_text(size=20), legend.title=element_text(size=20), legend.text=element_text(size=15)) +
  xlab("Country of Residence") +
  ylab("Mean GP Response") +
  geom_text(aes(label = XGP.Mean), hjust=-.25, size=5) +
  #scale_fill_manual(values = c('royalblue3','steelblue1', 'lightcyan', 'darkblue')) +  
  scale_fill_manual(values = c('#0047b3','#4da6ff', '#e6f2ff', '#001a4d')) + 
  #scale_fill_brewer(palette="Dark2") +
  coord_flip()

###



#5.d.
#Relationship between Age and Income
#get frequencies
q5d<-count(procrast_hdi1, c("Age", "Income.Year", "Gender"))
#str(q5d)

#merge frequencies to procrast_hdi1 to read into ggplot
freqAIG<-merge(procrast_hdi1, q5d, by=c("Age", "Income.Year", "Gender"), all.x = TRUE)
#dim(freqAIG)

#creating scatterplot.
ggplot(na.omit(procrast_hdi1), aes(x=Age, y=Income.Year, color=Gender, fill=Gender, shape=Gender)) + 
  theme_minimal() +
  geom_point() + geom_smooth(method=lm) +  #Make scatterplot and add linea
  stat_sum() + scale_size(guide = "none") + #size by counts
  scale_shape_manual(values=c(19, 18, 20))+
  scale_colour_hue(l=50) +
  scale_fill_manual(values = c('orange','lightblue2')) +                 
  #scale_size_manual(values=4)+
  ggtitle("Age Versus Income") +
  theme(plot.title = element_text(hjust = 0.5, size=20), axis.text=element_text(size=15), axis.title=element_text(size=20), legend.title=element_text(size=20), legend.text=element_text(size=20)) +
  xlab("Age") +
  ylab("Income") 


#5.e.
#Relationship between Life Satisfaction and Income
#creating scatterplot.
ggplot(procrast_hdi1, aes(x=SWLS.Mean, y=HDI)) + theme_minimal() +
  geom_point() + geom_smooth(method=lm) +
  scale_colour_hue(l=50)+
  ggtitle("Life Satisfaction Versus HDI") +
  theme(plot.title = element_text(hjust = 0.5, size=20), axis.text=element_text(size=15), axis.title=element_text(size=20), legend.title=element_text(size=20), legend.text=element_text(size=20)) +
  xlab("Life Satisfaction") +
  ylab("HDI") 

#creating Barplot using Development Level
HDI.level.table<-aggregate(procrast_hdi1$SWLS.Mean, by=list(procrast_hdi1$Development_Level), FUN=mean)
names(HDI.level.table)<-c("Development_Level", 'SWLS.Mean')
HDI.level.table$SWLS.Mean <- round(HDI.level.table$SWLS.Mean, digits=1)
#barchart to show SWLM mean by Development Level.
ggplot(HDI.level.table, aes(y=SWLS.Mean, x=Development_Level, fill=Development_Level)) +
  geom_bar(stat="identity") + 
  scale_fill_brewer(palette="YlOrRd") +
  ggtitle("Life Satisfaction and HDI (Development Level)") +
  theme(plot.title = element_text(hjust = 0.5, size=20), axis.text=element_text(size=15), axis.title=element_text(size=20),legend.position="none") +
  xlab("Development Level") +
  ylab("SWLS Mean") +
  geom_text(aes(label = SWLS.Mean), vjust=-.25, size=6) +
  ylim(0,4) 

#Investigate Low Development Level
EMTABLE(procrast_hdi1$Development_Level, var1='Development Level', digits=1)
na.omit(procrast_hdi1[procrast_hdi1$Development_Level=='Low Human Development', c('Age', 'Country', 'HDI', 'SWLS.Mean')])
