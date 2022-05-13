#preliminary graphs of the fmri data (% signal change)

library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)

#load the tables containing pre, post and fu data
pre_percent_signal_change<-read.delim('//zi.local/flstorage/dep_psm/AG-Paret/Projects/BrainBoost/data_analysis/spm_analysis/firstlevel/ses-pre/task-faces/taskrelated_activity/original/ROI-percent-BOLD-signal-change_mask-Left-Amygdala_-23_-5_-18_roi.txt')
post_percent_signal_change<-read.delim('//zi.local/flstorage/dep_psm/AG-Paret/Projects/BrainBoost/data_analysis/spm_analysis/firstlevel/ses-post/task-faces/taskrelated_activity/original/ROI-percent-BOLD-signal-change_mask-Left-Amygdala_-23_-5_-18_roi.txt')
fu_percent_signal_change<-read.delim('//zi.local/flstorage/dep_psm/AG-Paret/Projects/BrainBoost/data_analysis/spm_analysis/firstlevel/ses-fu/task-faces/taskrelated_activity/original/ROI-percent-BOLD-signal-change_mask-Left-Amygdala_-23_-5_-18_roi.txt')

#merge all to one table (add a column with timepoint info) and remove the initial tables
pre_percent_signal_change<-pre_percent_signal_change%>%mutate(timepoint = "pre")
post_percent_signal_change<-post_percent_signal_change%>%mutate(timepoint = "post")
fu_percent_signal_change<-fu_percent_signal_change%>%mutate(timepoint = "fu")
percent_signal_change<-rbind(pre_percent_signal_change,post_percent_signal_change,fu_percent_signal_change)
remove(pre_percent_signal_change,post_percent_signal_change,fu_percent_signal_change)

#change naming of subjects from sub-SUBxx to subxx
percent_signal_change<-mutate(percent_signal_change,subject= sub('sub-SUB','sub',subject))

#load demographics table to add info about randomization
demographics<-read_xlsx('//zi.local/flstorage/dep_psm/AG-Paret/Projects/BrainBoost/data_analysis/demographics.xlsx')
group_info<-select(demographics, subject=Subject, group=Group)
percent_signal_change<-merge(percent_signal_change, group_info, by='subject')

#create contrast column (tidying)
percent_signal_change<-pivot_longer(percent_signal_change, cols=2:3,names_to = 'condition', values_to='signal_change')

#summary stats
percent_signal_change<-group_by(percent_signal_change,condition,timepoint, group)
psc_summary<-summarise(percent_signal_change,mean_signal_change=mean(signal_change), sd_signal_change=sd(signal_change))

#visualisation:
#faces
ggplot(data = filter(psc_summary, condition=='face'), aes(x=factor(timepoint,levels = c("pre", "post","fu")),y = mean_signal_change, group = group))+geom_point(size=3,aes(color=group))+geom_line(linetype="dotted",aes(color=group))+xlab('timepoint')+ylab('% signal change')+ ggtitle('Left amygdala % signal change: faces')+geom_errorbar(width=.1, aes(color=group, ymin=mean_signal_change-sd_signal_change, ymax=mean_signal_change+sd_signal_change))
#scrambled
ggplot(data = filter(psc_summary, condition=='scrambled'), aes(x=factor(timepoint,levels = c("pre", "post","fu")),y = mean_signal_change, group = group))+geom_point(size=3,aes(color=group))+geom_line(linetype="dotted",aes(color=group))+xlab('timepoint')+ylab('% signal change')+ ggtitle('Left amygdala % signal change: scrambled')+geom_errorbar(width=.1, aes(color=group, ymin=mean_signal_change-sd_signal_change, ymax=mean_signal_change+sd_signal_change))
