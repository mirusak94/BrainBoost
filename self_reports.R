#BrainBoost self-reports
#Miroslava Jindrova, ZI Mannheim, 2021

library(dplyr)
library(readxl)

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

#add group info and remove drop outs
#add info about randomization and drop the participants that dropped out before post 
demographics<-read_xlsx('Y:/Projects/BrainBoost/data_analysis/demographics.xlsx')
group_info<-select(filter(demographics, is.na(Comments)|!Comments=='dropped out'), subject=Subject, group=Group)
group_info<-mutate(group_info, subject=sub('sub','sub-',subject))
pre<-merge(pre, group_info, by='subject')
post<-merge(post, group_info, by='subject')
fu<-merge(fu, group_info, by='subject')

#extract ders questionnaire
ders<-rbind(select(pre,c('subject','timepoint','group',57:92)),select(post,c('subject','timepoint','group',57:92)),select(fu,c('subject','timepoint','group',92:127)))
#calculate the score (items 1,3,6,7,8,10,17,20,22,24 and 34 are reverse score items)

colnames(ders)[4:39]<-c(paste(rep('i',36),as.character(c(1:36)),collapse = '')) #this stil doesn't work, I want to have i1 to i36
ders<-mutate(ders, total=-'1'+'2')

ders<-mutate(ders, total=-.[[4]]+.[[5]]-.[[6]]+.[[7]]+.[[8]]-.[[9]]-.[[10]]-.[[11]]+.[[12]]-.[[13]]+.[[14]]+.[[15]]+.[[16]]+.[[17]]+.[[18]]+.[[19]]-.[[20]]+.[[21]]+.[[22]]-.[[23]]+.[[24]]-.[[25]]+.[[26]]-.[[27]]+.[[28]]+.[[29]]+.[[30]]+.[[31]]+.[[32]]+.[[33]]+.[[34]]+.[[35]]+.[[36]]-.[[37]]+.[[38]]+.[[39]])
