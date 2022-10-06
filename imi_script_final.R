#21.09.22 FK
#Dataframes IMI for Brainboost

library(dplyr)
library(reshape)

#set working directory
setwd("W:/group_psm/AG-Paret/Projects/BrainBoost/data_analysis/Fragebogen")

#loading directory CAVE: access to //hobbes needed
de_raw_NF_3 <- read.csv("//hobbes/daten/PSM/Brainboost/evasys/NF_3_en.csv",sep=";",fileEncoding="latin1")

#CAVE: the file below is required here since it pulls information about the reverse scoring and subscales from it!
Items <- read.csv("W:/group_psm/AG-Paret/Projects/BrainBoost/data_analysis/Fragebogen/IMI_scales_and_items.csv",sep=";",fileEncoding="latin1",header = FALSE)

#prepare "Items" for column names
dims<-substr(Items[3,2:length(Items)],start=1,stop=3)
no<-Items[1,2:length(Items)]

#create data frame with only the relevant columns
df_imi<-de_raw_NF_3[,c(1,2,18:47)]


#rename columns for better handling and independence of language
colnames(df_imi) <- c("sheet","ID",paste(dims,"_",no,sep = ""),"date")
  #"Item_",c(1:6,"7R",8:13,"14R",15,"16R",17:20,"21R","22R",23:28,"29R"),sep=""),"date")


#rename sheet no., so following the convention of the "strats"-file
df_imi$sheet <- c(paste("2-",1:length(df_imi$sheet),sep=""))


#format date as class"POSIXct" if needed for analysis. else delete the next step
#CAVE: only works in german because of the word "um" in the middle of the string below
df_imi$date<-as.POSIXct(df_imi$date,format="%d.%m.%Y um %H:%M:%S")

#recode reversed items
df_imi[grepl("R",colnames(df_imi),fixed = TRUE)]<-8-df_imi[grepl("R",colnames(df_imi),fixed = TRUE)]

#estimate means for each subscale; ignoring missing values
value <- rowMeans(df_imi[grepl("val",colnames(df_imi),fixed = TRUE)],na.rm = TRUE)
interest <- rowMeans(df_imi[grepl("int",colnames(df_imi),fixed = TRUE)],na.rm = TRUE)
effort <- rowMeans(df_imi[grepl("eff",colnames(df_imi),fixed = TRUE)],na.rm = TRUE)
pressure <- rowMeans(df_imi[grepl("pre",colnames(df_imi),fixed = TRUE)],na.rm = TRUE)
competence <- rowMeans(df_imi[grepl("com",colnames(df_imi),fixed = TRUE)],na.rm = TRUE)

#add as coloumns
df_imi<-cbind(df_imi,value,interest,effort,pressure,competence)

#create data frame with means only
df_imi_means<-df_imi[,c(2,33,34,35,36,37,32)]

#export
#write.csv(df_imi_means,"W:/group_psm/AG-Paret/Projects/BrainBoost/data_analysis/Fragebogen/clean_imi.csv", row.names = FALSE)

remove(de_raw_NF_3,df_imi_means,Items,no,dims,competence,effort,interest,pressure, value)

df_imi<-df_imi[,c(2,33:37)]
df_imi<-reshape::melt(df_imi, id='ID')
colnames(df_imi)<- c("subject","subscale","score")

ggplot(df_imi,aes(subscale,score, fill=subscale))+geom_boxplot()+ggtitle('Intristic Motivation Inventory')+theme(axis.title.x=element_blank(),axis.text.x=element_blank())+scale_fill_brewer(palette='Set1')

#correlate with PES
PES <- read.csv("PES_realtimeData.csv",sep=",",fileEncoding="latin1")
PES<-group_by(PES, subject)
subPES<- summarise(PES, PES=mean(blockES))
#prepare IMI for merging with subPES
df_imi<-mutate(df_imi, subject=sub('b','sub-',substr(subject,3,5)))

#merge IMI & subPES
imiPES<-merge(subPES,df_imi)

ggplot(imiPES,aes(0-PES,score))+geom_point()+geom_smooth(method='lm')+labs(x='downregulation success')+ggtitle('Correlation of motivation subscales with downregulation success')+ scale_color_brewer(palette='Set1')+facet_row(~subscale)+theme(legend.position = 'none')
