#21.09.22 FK
#Dataframes Strats for Brainboost

library(RColorBrewer)
library(ggplot2)
library(scales)

#set working directory
setwd("W:/group_psm/AG-Paret/Projects/BrainBoost/data_analysis/Fragebogen")

##loading directory CAVE: access to //hobbes needed
de_raw_NF_1_2 <- read.csv("//hobbes/daten/PSM/Brainboost/evasys/NF_1_2_en.csv",sep=";",fileEncoding="latin1")
de_raw_NF_3 <- read.csv("//hobbes/daten/PSM/Brainboost/evasys/NF_3_en.csv",sep=";",fileEncoding="latin1")



#make sheet no. unique
de_raw_NF_1_2$Bogen <- c(paste("1-",1:length(de_raw_NF_1_2$Bogen),sep=""))
de_raw_NF_3$Bogen <- c(paste("2-",1:length(de_raw_NF_3$Bogen),sep=""))


#for strategies: combine information from both files via intersection;
df_strategies <- merge(de_raw_NF_1_2,de_raw_NF_3,all=TRUE)
df_strategies <- subset(df_strategies,select=intersect(colnames(de_raw_NF_1_2),colnames(de_raw_NF_3)))


#remove last column since it contains irrelevant data
df_strategies[19] <- NULL


#rename columns for better handling and independence of language
colnames(df_strategies) <- c("sheet","ID",paste("Q",1:12,sep=""),"OpenQ1","OpenQ2","OpenQ3","date")



#format date as class"POSIXct" to enable sorting
#CAVE: only works in german because of the word "um" in the middle of the string below
df_strategies$date<-as.POSIXct(df_strategies$date,format="%d.%m.%Y um %H:%M:%S")

#create numeric ID to enable sorting
ID_sort <- as.numeric(substr(df_strategies$ID,start = 4, stop = length(df_strategies$ID)))

#sort primarily by ID and within ID by time
df_strategies <- df_strategies[order(ID_sort,df_strategies$date),]


#Create a new column: Session no.
#Using ID_sort from above, create a new data frame with the unique ID's and their frequency
df_ID_freq<- as.data.frame(table(ID_sort))

#then count from 1 to n for each ID using the 'lapply' function, where n equals the amount of Sessions
#Since lapply returns a list, force it back to an integer vector using the 'unlist' function
Session<-unlist(lapply(c(df_ID_freq$Freq),seq))

#now add 'Session' in the main data frame and rearrange the columns
df_strategies<-cbind(df_strategies,Session)
df_strategies<-df_strategies[,c(1,2,19,3:18)]


#replace "[BILD]" with NA for Open Questions
df_strategies$OpenQ1[df_strategies$OpenQ1=="[BILD]"]<-NA
df_strategies$OpenQ3[df_strategies$OpenQ3=="[BILD]"]<-NA

#convert answer on OpenQ2 to string
df_strategies$OpenQ2[df_strategies$OpenQ2==1]<-"Yes"
df_strategies$OpenQ2[df_strategies$OpenQ2==2]<-"No"

#export
#write.csv(df_strategies,"W:/group_psm/AG-Paret/Projects/BrainBoost/data_analysis/Fragebogen/clean_strategies.csv", row.names = FALSE)

remove(de_raw_NF_1_2,de_raw_NF_3,df_ID_freq,ID_sort,Session)

##strategies analysis
df_strategies<-mutate(df_strategies, reappraisal=rowMeans(df_strategies[,c('Q1','Q2','Q8','Q9','Q10','Q11')], na.rm=TRUE), distraction=rowMeans(df_strategies[,c('Q3','Q7')],na.rm=TRUE), relaxation=Q4,mindfulness=Q5, attention_to_neutral=Q6, supression=Q12)
strateg<-df_strategies[,c("ID","Session","reappraisal","supression","mindfulness","attention_to_neutral","relaxation","distraction")]
strateg<-reshape::melt(strateg, id=c('ID','Session'))

colnames(strateg)<- c("subject","session","strategy","frequency")

#exclude subjects that dropped out
all<-unique(strateg$subject)
complete3<-unique(strateg$subject[which(strateg$session=='3')])
missing3<-setdiff(all,complete3)
complete2<-unique(strateg$subject[which(strateg$session=='3')])
missing2<-setdiff(all,complete2)
missing<-unique(c(missing2,missing3))
strateg<-strateg[!strateg$subject %in% missing,]

ggplot(strateg,aes(strategy,frequency, fill=strategy))+stat_summary(fun=mean, geom='bar')+stat_summary(fun.data=mean_cl_normal, geom='errorbar', width=0.2)+ylab('Mean Frequency')+ggtitle('Strategies used during neurofeedback')+theme(axis.title.x=element_blank(),axis.text.x=element_blank())+scale_fill_brewer(palette='Set1')


#correlate with PES
PES <- read.csv("PES_realtimeData.csv",sep=",",fileEncoding="latin1")
PES<-group_by(PES, subject,session)
sesPES<- summarise(PES, PES=mean(blockES))

#prepare strateg for merging with sesPES
strateg<-mutate(strateg, subject=sub('b','sub-',substr(subject,3,5)))

#merge IMI & subPES
strPES<-merge(sesPES,strateg)

ggplot(strPES,aes(0-PES,frequency, color=strategy))+geom_point()+geom_smooth(method='lm')+labs(x='downregulation success')+ggtitle('Correlation of strategies with downregulation success')+ scale_color_brewer(palette='Set1')+facet_wrap(~strategy)+theme(legend.position = 'none')

#between sub corr
strPES<-group_by(strPES,subject, strategy)
strPESsub<-summarise(strPES, frequency=mean(frequency), PES=mean(PES))

ggplot(strPESsub,aes(0-PES,frequency, color=strategy))+geom_point()+geom_smooth(method='lm')+labs(x='downregulation success')+ggtitle('Correlation of strategies with downregulation success')+ scale_color_brewer(palette='Set1')+facet_wrap(~strategy)+theme(legend.position = 'none')
