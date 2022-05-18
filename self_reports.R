#BrainBoost self-reports
#Miroslava Jindrova, ZI Mannheim, 2021

library(dplyr)

#read the data
pre<-read.csv('//hobbes/daten/PSM/Brainboost/evasys/BBpre_raw.csv', sep = ';')
post<-read.csv('//hobbes/daten/PSM/Brainboost/evasys/BBpost_raw.csv', sep = ';')
fu<-read.csv('//hobbes/daten/PSM/Brainboost/evasys/BBfu_raw.csv', sep = ';')

##CLEANING DATA

#rename column with subject ID and make the subject ID uniform
colnames(pre)[2] <- "subject"
pre<-mutate(pre, subject=sub('b','sub-',substr(subject,3,5)))

colnames(post)[2] <- "subject"
post<-mutate(post, subject=sub('b','sub-',substr(subject,3,5)))

colnames(fu)[2] <- "subject"
fu<-mutate(fu, subject=sub('b','sub-',substr(subject,3,5)))

#add timepoint info to each dataset
pre<-mutate(pre, timepoint=rep('pre',length(subject)))
post<-mutate(post, timepoint=rep('post',length(subject)))
fu<-mutate(fu, timepoint=rep('fu',length(subject)))

ders<-rbind(select(pre,c(2,'timepoint',57:92)),select(post,c(2,'timepoint',57:92)),select(fu,c(2,'timepoint',92:127)))
