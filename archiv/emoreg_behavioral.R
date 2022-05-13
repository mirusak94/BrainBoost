#emoreg behavioral data

library(dplyr)
library(ggplot2)

#load the data
s4pre<-read.delim('//hobbes/daten/PSM/Brainboost/sourcedata/SUB04/ses-pre/sub-04_ses-pre_rating-emoreg.txt')
s5pre<-read.delim('//hobbes/daten/PSM/Brainboost/sourcedata/SUB05/ses-pre/sub-05_ses-pre_rating-emoreg.txt')
s6pre<-read.delim('//hobbes/daten/PSM/Brainboost/sourcedata/SUB06/ses-pre/sub-06_ses-pre_rating-emoreg.txt')
s7pre<-read.delim('//hobbes/daten/PSM/Brainboost/sourcedata/SUB07/ses-pre/sub-07_ses-pre_rating-emoreg.txt')
s8pre<-read.delim('//hobbes/daten/PSM/Brainboost/sourcedata/SUB08/ses-pre/sub-08_ses-pre_rating-emoreg.txt')
s9pre<-read.delim('//hobbes/daten/PSM/Brainboost/sourcedata/SUB09/ses-pre/sub-09_ses-pre_rating-emoreg.txt')
s10pre<-read.delim('//hobbes/daten/PSM/Brainboost/sourcedata/SUB10/ses-pre/sub-10_ses-pre_rating-emoreg.txt')
s11pre<-read.delim('//hobbes/daten/PSM/Brainboost/sourcedata/SUB11/ses-pre/sub-11_ses-pre_rating-emoreg.txt')
s12pre<-read.delim('//hobbes/daten/PSM/Brainboost/sourcedata/SUB12/ses-pre/sub-12_ses-pre_rating-emoreg.txt')
s4post<-read.delim('//hobbes/daten/PSM/Brainboost/sourcedata/SUB04/ses-post/sub-04_ses-post_rating-emoreg.txt')
s5post<-read.delim('//hobbes/daten/PSM/Brainboost/sourcedata/SUB05/ses-post/sub-05_ses-post_rating-emoreg.txt')
s6post<-read.delim('//hobbes/daten/PSM/Brainboost/sourcedata/SUB06/ses-post/sub-06_ses-post_rating-emoreg.txt')
s7post<-read.delim('//hobbes/daten/PSM/Brainboost/sourcedata/SUB07/ses-post/sub-07_ses-post_rating-emoreg.txt')
s8post<-read.delim('//hobbes/daten/PSM/Brainboost/sourcedata/SUB08/ses-post/sub-08_ses-post_rating-emoreg.txt')
s9post<-read.delim('//hobbes/daten/PSM/Brainboost/sourcedata/SUB09/ses-post/sub-09_ses-post_rating-emoreg.txt')
s10post<-read.delim('//hobbes/daten/PSM/Brainboost/sourcedata/SUB10/ses-post/sub-10_ses-post_rating-emoreg.txt')
s11post<-read.delim('//hobbes/daten/PSM/Brainboost/sourcedata/SUB11/ses-post/sub-11_ses-post_rating-emoreg.txt')
s12post<-read.delim('//hobbes/daten/PSM/Brainboost/sourcedata/SUB12/ses-post/sub-12_ses-post_rating-emoreg.txt')
s4fu<-read.delim('//hobbes/daten/PSM/Brainboost/sourcedata/SUB04/ses-fu/sub-04_ses-fu_rating-emoreg.txt')
s5fu<-read.delim('//hobbes/daten/PSM/Brainboost/sourcedata/SUB05/ses-fu/sub-05_ses-fu_rating-emoreg.txt')
s7fu<-read.delim('//hobbes/daten/PSM/Brainboost/sourcedata/SUB07/ses-fu/sub-07_ses-fu_rating-emoreg.txt')

#add columns about the subjects and timepoints
s4pre<-s4pre%>%mutate(timepoint = "pre",sub = 's04', group = 'control')
s5pre<-s5pre%>%mutate(timepoint = "pre",sub = 's05', group = 'nf')
s6pre<-s6pre%>%mutate(timepoint = "pre",sub = 's06', group = 'control')
s7pre<-s7pre%>%mutate(timepoint = "pre",sub = 's07', group = 'nf')
s8pre<-s8pre%>%mutate(timepoint = "pre",sub = 's08', group = 'control')
s9pre<-s9pre%>%mutate(timepoint = "pre",sub = 's09', group = 'control')
s10pre<-s10pre%>%mutate(timepoint = "pre",sub = 's10', group = 'control')
s11pre<-s11pre%>%mutate(timepoint = "pre",sub = 's11', group = 'nf')
s12pre<-s12pre%>%mutate(timepoint = "pre",sub = 's12', group = 'nf')
s4post<-s4post%>%mutate(timepoint = "post",sub = 's04', group = 'control')
s5post<-s5post%>%mutate(timepoint = "post",sub = 's05', group = 'nf')
s6post<-s6post%>%mutate(timepoint = "post",sub = 's06', group = 'control')
s7post<-s7post%>%mutate(timepoint = "post",sub = 's07', group = 'nf')
s8post<-s8post%>%mutate(timepoint = "post",sub = 's08', group = 'control')
s9post<-s9post%>%mutate(timepoint = "post",sub = 's09', group = 'control')
s10post<-s10post%>%mutate(timepoint = "post",sub = 's10', group = 'control')
s11post<-s11post%>%mutate(timepoint = "post",sub = 's11', group = 'nf')
s12post<-s12post%>%mutate(timepoint = "post",sub = 's12', group = 'nf')
s4fu<-s4fu%>%mutate(timepoint = "fu",sub = 's04', group = 'control')
s5fu<-s5fu%>%mutate(timepoint = "fu",sub = 's05', group = 'nf')
s7fu<-s7fu%>%mutate(timepoint = "fu",sub = 's07', group = 'nf')

#bind all into one table and delete the initial tables
emoreg_behav<-rbind(s4pre,s5pre,s6pre,s7pre,s8pre,s9pre,s10pre,s11pre,s12pre,s4post,s5post,s6post,s7post,s8post,s9post,s10post,s11post,s12post,s4fu,s5fu,s7fu)
remove(s4pre,s5pre,s6pre,s7pre,s8pre,s9pre,s10pre,s11pre,s12pre,s4post,s5post,s6post,s7post,s8post,s9post,s10post,s11post,s12post,s4fu,s5fu,s7fu)

#delete the rows with not confirmed ratings
emoreg_behav$Arousal.confirmed<-replace(emoreg_behav$Arousal.confirmed, emoreg_behav$Arousal.confirmed==0,NA)
not_confirmed<-apply(emoreg_behav,1,function(x){any(is.na(x))})
emoreg_behav<-emoreg_behav[!not_confirmed,]

#create a new table with mean and standard deviation for each subject, timepoint and condition
emoreg_behav<-group_by(emoreg_behav, Condition, timepoint, sub, group)
emoreg_sub<-summarise(emoreg_behav,mean_sub_ar = mean(Arousal.rating), sd_sub_ar = sd(Arousal.rating))
emoreg_sub<-group_by(emoreg_sub, Condition, timepoint, group)
emoreg_summary<-summarise(emoreg_sub, mean_arousal=mean(mean_sub_ar),sd_mean_arousal=sd(mean_sub_ar),mean_sd_arousal=mean(sd_sub_ar))

#visualisation of the means
#neutral 
ggplot(data = filter(emoreg_summary, Condition == 'view_neutral'), aes(x=factor(timepoint,levels = c("pre", "post", "fu")),y =  mean_arousal, group = group ))+geom_point(size=3,aes(color=group))+geom_line(linetype="dotted", aes(color=group))+xlab('timepoint')+ylab('mean arousal rating')+ ggtitle('MEAN neutral')+geom_errorbar(width=.1, aes(color=group, ymin=mean_arousal-sd_mean_arousal, ymax=mean_arousal+sd_mean_arousal))
#regulate 
ggplot(data = filter(emoreg_summary, Condition == 'regulate_negative'), aes(x=factor(timepoint,levels = c("pre", "post", "fu")),y =  mean_arousal, group = group ))+geom_point(size=3,aes(color=group))+geom_line(linetype="dotted", aes(color=group))+xlab('timepoint')+ylab('mean arousal rating')+ ggtitle('MEAN regulate')+geom_errorbar(width=.1, aes(color=group, ymin=mean_arousal-sd_mean_arousal, ymax=mean_arousal+sd_mean_arousal))
#negative 
ggplot(data = filter(emoreg_summary, Condition == 'view_negative'), aes(x=factor(timepoint,levels = c("pre", "post", "fu")),y =  mean_arousal, group = group ))+geom_point(size=3,aes(color=group))+geom_line(linetype="dotted", aes(color=group))+xlab('timepoint')+ylab('mean arousal rating')+ ggtitle('MEAN negative')+geom_errorbar(width=.1, aes(color=group, ymin=mean_arousal-sd_mean_arousal, ymax=mean_arousal+sd_mean_arousal))

#visualisation variance
#neutral 
ggplot(data = filter(emoreg_summary, Condition == 'view_neutral'), aes(x=factor(timepoint,levels = c("pre", "post", "fu")),y =  mean_sd_arousal, group = group ))+geom_point(size=3,aes(color=group))+geom_line(linetype="dotted", aes(color=group))+xlab('timepoint')+ylab('sd arousal rating')+ ggtitle('SD neutral')
#regulate 
ggplot(data = filter(emoreg_summary, Condition == 'regulate_negative'), aes(x=factor(timepoint,levels = c("pre", "post", "fu")),y =  mean_sd_arousal, group = group ))+geom_point(size=3,aes(color=group))+geom_line(linetype="dotted", aes(color=group))+xlab('timepoint')+ylab('sd arousal rating')+ ggtitle('SD regulate')
#negative 
ggplot(data = filter(emoreg_summary, Condition == 'view_negative'), aes(x=factor(timepoint,levels = c("pre", "post", "fu")),y =  mean_sd_arousal, group = group ))+geom_point(size=3,aes(color=group))+geom_line(linetype="dotted", aes(color=group))+xlab('timepoint')+ylab('sd arousal rating')+ ggtitle('SD negative')
