#BrainBoost self-reports
#Miroslava Jindrova, ZI Mannheim, 2021-2022

library(dplyr)
library(readxl)
library(ggplot2)

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
#fu includes both - 3m and 6m follow up, this has to be separated
fu<-mutate(fu, Date=as.Date(gsub('\\.','/',substr(Zeitstempel,1,10)),format='%d/%m/%Y'))
fu<-group_by(fu,subject)
fu1<-slice(fu, which.min(Date))
fu1<-mutate(fu1, timepoint=rep('fu1',length(subject)))
fu2<-slice(fu, -which.min(Date))
fu2<-mutate(fu2, timepoint=rep('fu2',length(subject)))

#add group info and remove drop outs
#add info about randomization and drop the participants that dropped out before post 
demographics<-read_xlsx('Y:/Projects/BrainBoost/data_analysis/demographics.xlsx')
group_info<-select(filter(demographics, is.na(Comments)|!Comments=='dropped out'), subject=Subject, group=Group)
group_info<-mutate(group_info, subject=sub('sub','sub-',subject))
pre<-merge(pre, group_info, by='subject')
post<-merge(post, group_info, by='subject')
fu1<-merge(fu1, group_info, by='subject')
fu2<-merge(fu2, group_info, by='subject')

#extract ders questionnaire
ders<-rbind(select(pre,c('subject','timepoint','group',57:92)),select(post,c('subject','timepoint','group',57:92)),select(fu1,c('subject','timepoint','group',92:127)),select(fu2,c('subject','timepoint','group',92:127)))

#calculate total score (items 1,3,6,7,8,10,17,20,22,24 and 34 are reverse score items)
colnames(ders)[4:39]<-paste0(rep('i',36),1:36) #rename the items i1 to i36 for easier indexing
ders<-mutate(ders, total=-i1+i2-i3+i4+i5-i6-i7-i8+i9-i10+i11+i12+i13+i14+i15+i16-i17+i18+i19-i20+i21-i22+i23-i24+i25+i26+i27+i28+i29+i30+i31+i32+i33-i34+i35+i36)
ders<-na.omit(ders) #get rid of data w/NAs

#plot ders
ggplot(ders,aes(x=factor(timepoint,levels = c("pre", "post","fu1","fu2")),y = total), group=group)+stat_summary(fun= mean,geom = 'point',size=3,aes(color=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_normal,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('DERS score')+ggtitle('Difficulties in Emotion Regulation Scale')+scale_color_manual(values=c("#D55E00", "#009E73"))

#calculate sample size
ders_nf_sample<-length(unique(ders$subject[which(ders$group=='E')]))
ders_ctrl_sample<-length(unique(ders$subject[which(ders$group=='C')]))
ders_nf_sample_pre<-length(unique(ders$subject[which(ders$group=='E'&ders$timepoint=='pre')]))
ders_nf_sample_post<-length(unique(ders$subject[which(ders$group=='E'&ders$timepoint=='post')]))
ders_nf_sample_fu1<-length(unique(ders$subject[which(ders$group=='E'&ders$timepoint=='fu1')]))
ders_nf_sample_fu2<-length(unique(ders$subject[which(ders$group=='E'&ders$timepoint=='fu2')]))
ders_ctrl_sample_pre<-length(unique(ders$subject[which(ders$group=='C'&ders$timepoint=='pre')]))
ders_ctrl_sample_post<-length(unique(ders$subject[which(ders$group=='C'&ders$timepoint=='post')]))
ders_ctrl_sample_fu1<-length(unique(ders$subject[which(ders$group=='C'&ders$timepoint=='fu1')]))
ders_ctrl_sample_fu2<-length(unique(ders$subject[which(ders$group=='C'&ders$timepoint=='fu2')]))

#extract BDI questionnaire
bdi<-rbind(select(pre,c('subject','timepoint','group',6:26)),select(post,c('subject','timepoint','group',6:26)),select(fu1,c('subject','timepoint','group',41:61)),select(fu2,c('subject','timepoint','group',41:61)))

#change scoring of change sleeping pattern and change in appetite item (since we are not interested in the direction of the change) 
#IT IS IMPORTANT TO KEEP THE ORDER OF THE CODE LINES HERE (otherwise you rewrite the data incorrectly)
bdi$Veränderungen.der.Schlafgewohnheiten[bdi$Veränderungen.der.Schlafgewohnheiten>=2&bdi$Veränderungen.der.Schlafgewohnheiten<=3]<-2
bdi$Veränderungen.der.Schlafgewohnheiten[bdi$Veränderungen.der.Schlafgewohnheiten>=4&bdi$Veränderungen.der.Schlafgewohnheiten<=5]<-3
bdi$Veränderungen.der.Schlafgewohnheiten[bdi$Veränderungen.der.Schlafgewohnheiten>=6&bdi$Veränderungen.der.Schlafgewohnheiten<=7]<-4

bdi$Veränderung.des.Appetits[bdi$Veränderung.des.Appetits>=2&bdi$Veränderung.des.Appetits<=3]<-2
bdi$Veränderung.des.Appetits[bdi$Veränderung.des.Appetits>=4&bdi$Veränderung.des.Appetits<=5]<-3
bdi$Veränderung.des.Appetits[bdi$Veränderung.des.Appetits>=6&bdi$Veränderung.des.Appetits<=7]<-4

#calculate total score
bdi$total<-rowSums(bdi[,4:24])
bdi<-na.omit(bdi) #get rid of data w/NAs

#plot bdi
ggplot(bdi,aes(x=factor(timepoint,levels = c("pre", "post","fu1","fu2")),y = total), group=group)+stat_summary(fun= mean,geom = 'point',size=3,aes(color=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_normal,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('BDI-II score')+ggtitle('Besk Depression Inventory')+scale_color_manual(values=c("#D55E00", "#009E73"))

#calculate sample size
bdi_nf_sample<-length(unique(bdi$subject[which(bdi$group=='E')]))
bdi_ctrl_sample<-length(unique(bdi$subject[which(bdi$group=='C')]))
bdi_nf_sample_pre<-length(unique(bdi$subject[which(bdi$group=='E'&bdi$timepoint=='pre')]))
bdi_nf_sample_post<-length(unique(bdi$subject[which(bdi$group=='E'&bdi$timepoint=='post')]))
bdi_nf_sample_fu1<-length(unique(bdi$subject[which(bdi$group=='E'&bdi$timepoint=='fu1')]))
bdi_nf_sample_fu2<-length(unique(bdi$subject[which(bdi$group=='E'&bdi$timepoint=='fu2')]))
bdi_ctrl_sample_pre<-length(unique(bdi$subject[which(bdi$group=='C'&bdi$timepoint=='pre')]))
bdi_ctrl_sample_post<-length(unique(bdi$subject[which(bdi$group=='C'&bdi$timepoint=='post')]))
bdi_ctrl_sample_fu1<-length(unique(bdi$subject[which(bdi$group=='C'&bdi$timepoint=='fu1')]))
bdi_ctrl_sample_fu2<-length(unique(bdi$subject[which(bdi$group=='C'&bdi$timepoint=='fu2')]))

#extract PCL-5 questionnaire
pcl5<-rbind(select(pre,c('subject','timepoint','group',312:331)),select(post,c('subject','timepoint','group',174:193)),select(fu1,c('subject','timepoint','group',209:228)),select(fu2,c('subject','timepoint','group',209:228)))

#calculate total score
pcl5$total<-rowSums(pcl5[,4:23])
pcl5<-na.omit(pcl5) #get rid of data w/NAs

#plot pcl-5
ggplot(pcl5,aes(x=factor(timepoint,levels = c("pre", "post","fu1","fu2")),y = total), group=group)+stat_summary(fun= mean,geom = 'point',size=3,aes(color=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_normal,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('PCL-5 score')+ggtitle('PTSD Checklist for DSM-5')+scale_color_manual(values=c("#D55E00", "#009E73"))

#calculate sample size
pcl5_nf_sample<-length(unique(pcl5$subject[which(pcl5$group=='E')]))
pcl5_ctrl_sample<-length(unique(pcl5$subject[which(pcl5$group=='C')]))
pcl5_nf_sample_pre<-length(unique(pcl5$subject[which(pcl5$group=='E'&pcl5$timepoint=='pre')]))
pcl5_nf_sample_post<-length(unique(pcl5$subject[which(pcl5$group=='E'&pcl5$timepoint=='post')]))
pcl5_nf_sample_fu1<-length(unique(pcl5$subject[which(pcl5$group=='E'&pcl5$timepoint=='fu1')]))
pcl5_nf_sample_fu2<-length(unique(pcl5$subject[which(pcl5$group=='E'&pcl5$timepoint=='fu2')]))
pcl5_ctrl_sample_pre<-length(unique(pcl5$subject[which(pcl5$group=='C'&pcl5$timepoint=='pre')]))
pcl5_ctrl_sample_post<-length(unique(pcl5$subject[which(pcl5$group=='C'&pcl5$timepoint=='post')]))
pcl5_ctrl_sample_fu1<-length(unique(pcl5$subject[which(pcl5$group=='C'&pcl5$timepoint=='fu1')]))
pcl5_ctrl_sample_fu2<-length(unique(pcl5$subject[which(pcl5$group=='C'&pcl5$timepoint=='fu2')]))
