#BrainBoost self-reports
#Miroslava Jindrova, ZI Mannheim, 2021-2022

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

#calculate total score (items 1,3,6,7,8,10,17,20,22,24 and 34 are reverse score items)
colnames(ders)[4:39]<-paste0(rep('i',36),1:36) #rename the items i1 to i36 for easier indexing
ders<-mutate(ders, total=-i1+i2-i3+i4+i5-i6-i7-i8+i9-i10+i11+i12+i13+i14+i15+i16-i17+i18+i19-i20+i21-i22+i23-i24+i25+i26+i27+i28+i29+i30+i31+i32+i33-i34+i35+i36)
ders<-na.omit(ders) #get rid of data w/NAs

#plot ders
ggplot(ders,aes(x=factor(timepoint,levels = c("pre", "post","fu")),y = total), group=group)+stat_summary(fun= mean,geom = 'point',size=3,aes(color=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_normal,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('DERS score')+ggtitle('Difficulties in Emotion Regulation Scale')

#extract BDI questionnaire
bdi<-rbind(select(pre,c('subject','timepoint','group',6:26)),select(post,c('subject','timepoint','group',6:26)),select(fu,c('subject','timepoint','group',41:61)))

#change scoring of change sleeping pattern and change in appetite item (since we are not interested in the direction of the change) 
#IT IS IMPORTANT TO KEEP THE ORDER OF THE CODE LINES HERE (otherwise you rewrite the data incorrectly)
bdi$Veränderungen.der.Schlafgewohnheiten[bdi$Veränderungen.der.Schlafgewohnheiten>=2&bdi$Veränderungen.der.Schlafgewohnheiten<=3]<-2
bdi$Veränderungen.der.Schlafgewohnheiten[bdi$Veränderungen.der.Schlafgewohnheiten>=4&bdi$Veränderungen.der.Schlafgewohnheiten<=5]<-3
bdi$Veränderungen.der.Schlafgewohnheiten[bdi$Veränderungen.der.Schlafgewohnheiten>=6&bdi$Veränderungen.der.Schlafgewohnheiten<=7]<-4

bdi$Veränderung.des.Appetits[bdi$Veränderung.des.Appetits>=2&bdi$Veränderung.des.Appetits<=3]<-2
bdi$Veränderung.des.Appetits[bdi$Veränderung.des.Appetits>=4&bdi$Veränderung.des.Appetits<=5]<-3
bdi$Veränderung.des.Appetits[bdi$Veränderung.des.Appetits>=6&bdi$Veränderung.des.Appetits<=7]<-4

#calculate total score
bdi<-mutate(bdi, total=sum(bdi[,4:24]))
bdi$total<-rowSums(bdi[,4:24])
bdi<-na.omit(bdi) #get rid of data w/NAs

#plot bdi
ggplot(bdi,aes(x=factor(timepoint,levels = c("pre", "post","fu")),y = total), group=group)+stat_summary(fun= mean,geom = 'point',size=3,aes(color=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_normal,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('DERS score')+ggtitle('Difficulties in Emotion Regulation Scale')
