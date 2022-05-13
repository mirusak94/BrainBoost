#preliminary graphs of the fmri data (contrasts)

library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)

#load the tables containing pre data
pre_contrasts1<-read.delim('//zi.local/flstorage/dep_psm/AG-Paret/Projects/BrainBoost/data_analysis/spm_analysis/firstlevel/ses-pre/task-emoreg/taskrelated_activity/trimmed_original/ROI-cons_mask-Right-Amygdala_23_-3_-18_roi.txt')
pre_contrasts2<-read.delim('//zi.local/flstorage/dep_psm/AG-Paret/Projects/BrainBoost/data_analysis/spm_analysis/firstlevel/ses-pre/task-emoreg/taskrelated_activity/original/ROI-cons_mask-Right-Amygdala_23_-3_-18_roi.txt')
#merge them into the one table
pre_contrasts<-merge(pre_contrasts1,pre_contrasts2,all = TRUE)
#delete the initial incomplete tables from the R workspace
remove(pre_contrasts1,pre_contrasts2)

#load post and fu data
post_contrasts<-read.delim('//zi.local/flstorage/dep_psm/AG-Paret/Projects/BrainBoost/data_analysis/spm_analysis/firstlevel/ses-post/task-emoreg/taskrelated_activity/original/ROI-cons_mask-Right-Amygdala_23_-3_-18_roi.txt')
fu_contrasts<-read.delim('//zi.local/flstorage/dep_psm/AG-Paret/Projects/BrainBoost/data_analysis/spm_analysis/firstlevel/ses-fu/task-emoreg/taskrelated_activity/original/ROI-cons_mask-Right-Amygdala_23_-3_-18_roi.txt')

#merge all to one table (add a column with timepoint info) and remove the initial tables
pre_contrasts<-pre_contrasts%>%mutate(timepoint = "pre")
post_contrasts<-post_contrasts%>%mutate(timepoint = "post")
fu_contrasts<-fu_contrasts%>%mutate(timepoint = "fu")
contrasts<-rbind(pre_contrasts,post_contrasts,fu_contrasts)
remove(pre_contrasts,post_contrasts,fu_contrasts)

#change naming of subjects from sub-SUBxx to subxx
contrasts<-mutate(contrasts,subject= sub('sub-SUB','sub',subject))

#load demographics table to add info about randomization
demographics<-read_xlsx('//zi.local/flstorage/dep_psm/AG-Paret/Projects/BrainBoost/data_analysis/demographics.xlsx')
group_info<-select(demographics, subject=Subject, group=Group)
contrasts<-merge(contrasts, group_info, by='subject')

#create contrast column (tidying)
contrasts<-pivot_longer(contrasts, cols=2:7,names_to = 'contrast', values_to='contrast_value')

#summary stats
contrasts<-group_by(contrasts,contrast,timepoint, group)
contrasts_summary<-summarise(contrasts,mean_t=mean(contrast_value), sd_t=sd(contrast_value))

#visualisation:
#regulate
ggplot(data = filter(contrasts_summary, contrast=='regulate'), aes(x=factor(timepoint,levels = c("pre", "post","fu")),y = mean_t, group = group))+geom_point(size=3,aes(color=group))+geom_line(linetype="dotted",aes(color=group))+xlab('timepoint')+ylab('contrast')+ ggtitle('Right amygdala: regulate - implicit baseline')+geom_errorbar(width=.1, aes(color=group, ymin=mean_t-sd_t, ymax=mean_t+sd_t))
#negative
ggplot(data = filter(contrasts_summary, contrast=='view'), aes(x=factor(timepoint,levels = c("pre", "post","fu")),y = mean_t, group = group))+geom_point(size=3,aes(color=group))+geom_line(linetype="dotted",aes(color=group))+xlab('timepoint')+ylab('contrast')+ ggtitle('Right amygdala: negative - implicit baseline')+geom_errorbar(width=.1, aes(color=group, ymin=mean_t-sd_t, ymax=mean_t+sd_t))
#neutral
ggplot(data = filter(contrasts_summary, contrast=='neutral'), aes(x=factor(timepoint,levels = c("pre", "post","fu")),y = mean_t, group = group))+geom_point(size=3,aes(color=group))+geom_line(linetype="dotted",aes(color=group))+xlab('timepoint')+ylab('contrast')+ ggtitle('Right amygdala: neutral - implicit baseline')+geom_errorbar(width=.1, aes(color=group, ymin=mean_t-sd_t, ymax=mean_t+sd_t))
#regulate vs. negative
ggplot(data = filter(contrasts_summary, contrast=='regulate.vs.view'), aes(x=factor(timepoint,levels = c("pre", "post","fu")),y = mean_t, group = group))+geom_point(size=3,aes(color=group))+geom_line(linetype="dotted",aes(color=group))+xlab('timepoint')+ylab('contrast')+ ggtitle('Right amygdala: regulate - negative')+geom_errorbar(width=.1, aes(color=group, ymin=mean_t-sd_t, ymax=mean_t+sd_t))
#regulate vs. neutral
ggplot(data = filter(contrasts_summary, contrast=='regulate.vs.neutral'), aes(x=factor(timepoint,levels = c("pre", "post","fu")),y = mean_t, group = group))+geom_point(size=3,aes(color=group))+geom_line(linetype="dotted",aes(color=group))+xlab('timepoint')+ylab('contrast')+ ggtitle('Right amygdala: regulate - neutral')+geom_errorbar(width=.1, aes(color=group, ymin=mean_t-sd_t, ymax=mean_t+sd_t))
#negative vs.neutral
ggplot(data = filter(contrasts_summary, contrast=='view.vs.neutral'), aes(x=factor(timepoint,levels = c("pre", "post","fu")),y = mean_t, group = group))+geom_point(size=3,aes(color=group))+geom_line(linetype="dotted",aes(color=group))+xlab('timepoint')+ylab('contrast')+ ggtitle('Right amygdala: negative - neutral')+geom_errorbar(width=.1, aes(color=group, ymin=mean_t-sd_t, ymax=mean_t+sd_t))
