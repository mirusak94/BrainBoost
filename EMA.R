#EMA BrainBoost
#Miroslava Jindrova, ZI Mannheim, 2021

library(readxl)
library(dplyr)
library(data.table)
library(tidyverse)
library(reshape)
library(psych)
library(Hmisc)

#load data into one file
folders<-list.files(path = "//hobbes/daten/PSM/Brainboost/moviesens/EMA", recursive = TRUE,pattern = "*xlsx", full.names = TRUE)
files<-sapply(folders,read_xlsx)
all<-rbindlist(files,idcol = "subject", fill = TRUE)
#create columns containing subject code and timepoint
all<- all %>% mutate(timepoint=substr(subject,56,57),subject=substr(subject,51,55)) %>%
  relocate(timepoint,.after=subject) %>%
  mutate(timepoint= gsub('pr','pre',timepoint),
         timepoint= gsub('po','post',timepoint))

#drop columns where all values are NA
all<-select_if(all,~any(!is.na(.)))

#add info about randomization and drop the participants that dropped out before post 
demographics<-read_xlsx('Y:/Projects/BrainBoost/data_analysis/demographics.xlsx')
group_info<-select(filter(demographics, is.na(Comments)|!Comments=='dropped out'), subject=Subject, group=Group)
all<-merge(all, group_info, by='subject')

##add info about the test day
#triggers<-select(all,subject, timepoint,Trigger_date, Form)
all<- all %>% mutate(Trigger_date=substr(Trigger_date,1,10),Trigger_time=substr(Trigger_time,12,19),Form_start_time=substr(Form_start_time,12,19))%>% 
  mutate(Trigger_date=as.Date(Trigger_date)) %>%
  group_by(subject, timepoint)%>%
  mutate(test_day=as.character (Trigger_date-min(Trigger_date)))

#sub07fu did the last test day separately, so we want to change the number of the test day there
all<-all[-which(all$subject=='sub07'&all$timepoint=='fu'&all$test_day==4),]
all$test_day[which(all$subject=='sub07'&all$timepoint=='fu'&all$test_day==6)]<-4
#drop out triggers later than the last test day 
all<-filter(all, test_day=='1'|test_day=='2'|test_day=='3'|test_day=='4')

#RESPONSE RETURN
all<-group_by(all, subject, timepoint, test_day, group)
return<-count(all, Form)
return<-ungroup(return)

#to avoid errors, count triggers where some data are missing
n_trigger_empty<-data.frame('Form'=c('Anspannung','Affekt','DSS-4','Erlebnisse'), 
                            'triggers_total'=c(sum(all$Form=='Anspannung'),
                                               sum(all$Form=='Affekt'),
                                               sum(all$Form=='DSS-4'),
                                               sum(all$Form=='Erlebnisse')),
                            'triggers_incomplete'=c(sum(all$Form=='Anspannung'&is.na(all$anspann_01)),
                                                    sum(all$Form=='Affekt'&(is.na(all$pana_glucklich)|is.na(all$pana_traurig)|is.na(all$pana_angstlich)|is.na(all$pana_entspannt)|is.na(all$pana_gereizt)|is.na(all$pana_zufrieden)|is.na(all$pana_wutend)|is.na(all$pana_frohlich)|is.na(all$pana_niedergeschlagen)|is.na(all$pana_enthusiastisch))),
                                                    sum(all$Form=='DSS-4'&(is.na(all$dss4_01)|is.na(all$dss4_02)|is.na(all$dss4_03)|is.na(all$dss4_04))),
                                                    sum(all$Form=='Erlebnisse'&(is.na(all$erleb__schlecht)|is.na(all$erleb__gut)))))

#plot the responses
#tension 
#control group
ggplot(data=filter(return, group == 'C', Form=='Anspannung'), aes(x=factor(timepoint,levels = c("pre", "post","fu")),y=n, fill=test_day))+geom_boxplot()+ylim(1,13) +xlab('timepoint')+ylab('triggers answered')+ ggtitle('control group - tension response')
#nf group
ggplot(data=filter(return, group == 'E', Form=='Anspannung'), aes(x=factor(timepoint,levels = c("pre", "post","fu")),y=n, fill=test_day))+geom_boxplot()+ylim(1,13) +xlab('timepoint')+ylab('triggers answered')+ ggtitle('NF group - tension response')

#PANAS 
#control group
ggplot(data=filter(return, group == 'C', Form=='Affekt'), aes(x=factor(timepoint,levels = c("pre", "post","fu")),y=n, fill=test_day))+geom_boxplot()+ylim(1,13) +xlab('timepoint')+ylab('triggers answered')+ ggtitle('control group - PANAS response')
#nf group
ggplot(data=filter(return, group == 'E', Form=='Affekt'), aes(x=factor(timepoint,levels = c("pre", "post","fu")),y=n, fill=test_day))+geom_boxplot()+ylim(1,13) +xlab('timepoint')+ylab('triggers answered')+ ggtitle('NF group - PANAS response')

#DSS-4 
#control group
ggplot(data=filter(return, group == 'C', Form=='DSS-4'), aes(x=factor(timepoint,levels = c("pre", "post","fu")),y=n, fill=test_day))+geom_boxplot()+ylim(1,13) +xlab('timepoint')+ylab('triggers answered')+ ggtitle('control group - DSS-4 response')
#nf group
ggplot(data=filter(return, group == 'E', Form=='DSS-4'), aes(x=factor(timepoint,levels = c("pre", "post","fu")),y=n, fill=test_day))+geom_boxplot()+ylim(1,13) +xlab('timepoint')+ylab('triggers answered')+ ggtitle('NF group - DSS-4 response')

#experiences 
#control group
ggplot(data=filter(return, group == 'C', Form=='Erlebnisse'), aes(x=factor(timepoint,levels = c("pre", "post","fu")),y=n, fill=test_day))+geom_boxplot()+ylim(1,13) +xlab('timepoint')+ylab('triggers answered')+ ggtitle('control group - experiences response')
#nf group
ggplot(data=filter(return, group == 'E', Form=='Erlebnisse'), aes(x=factor(timepoint,levels = c("pre", "post","fu")),y=n, fill=test_day))+geom_boxplot()+ylim(1,13) +xlab('timepoint')+ylab('triggers answered')+ ggtitle('NF group - experiences response')

##clean DATA 
#(sorted to negative affect, positive affect, dss-4, inner tension, experience)
neg_af<-na.omit(select(all,subject, timepoint,group, test_day, Form_start_time,pana_traurig,pana_gereizt, pana_wutend,pana_niedergeschlagen,pana_angstlich),cols=c(pana_traurig,pana_gereizt, pana_wutend,pana_niedergeschlagen,pana_angstlich))
pos_af<-na.omit(select(all,subject, timepoint,group, test_day, Form_start_time,pana_glucklich,pana_entspannt, pana_zufrieden,pana_frohlich,pana_enthusiastisch),cols=c(pana_glucklich,pana_entspannt, pana_zufrieden,pana_frohlich,pana_enthusiastisch))
DSS<-na.omit(select(all,subject, timepoint,group, test_day, Form_start_time,dss4_01, dss4_02,dss4_03,dss4_04),cols=c(dss4_01, dss4_02,dss4_03,dss4_04))
ansp<-na.omit(select(all,subject, timepoint,group, test_day, Form_start_time,anspann_01),cols=anspann_01)
exper<-na.omit(select(all,subject, timepoint,group, test_day, Form_start_time,erleb__schlecht, erleb__gut),cols=c(erleb__schlecht, erleb__gut))

#calculate final scores
neg_af<-mutate(neg_af, sum_neg=(pana_traurig+pana_gereizt+pana_wutend+pana_niedergeschlagen+pana_angstlich))
neg_af<-group_by(neg_af, subject, timepoint, group)
na<-summarise(neg_af, na=mean(sum_neg),mssd_na=mssd(sum_neg))

pos_af<-mutate(pos_af, sum_pos=(pana_glucklich+pana_entspannt+pana_zufrieden+pana_frohlich+pana_enthusiastisch))
pos_af<-group_by(pos_af, subject, timepoint, group)
pa<-summarise(pos_af, pa=mean(sum_pos),mssd_pa=mssd(sum_pos))

DSS<-mutate(DSS, dss4_m=(dss4_01+dss4_02+dss4_03+dss4_04)/4)
DSS<-group_by(DSS, subject, timepoint, group)
dss4<-summarise(DSS, dss4=mean(dss4_m),mssd_dss4=mssd(dss4_m))

ansp<-group_by(ansp, subject, timepoint, group)
inner_tens<-summarise(ansp, tens=mean(anspann_01),mssd_tens=mssd(anspann_01))

exper<-group_by(exper, subject, timepoint, group)
exp_gb<-summarise(exper, g_exp=mean(erleb__gut),b_exp=mean(erleb__schlecht),mssd_ge=mssd(erleb__gut),mssd_be=mssd(erleb__schlecht))
#merge all scores in one table
ema<-merge(merge(merge(merge(pa,na),dss4),inner_tens),exp_gb)

##plots
#means
ggplot(ema,aes(x=factor(timepoint,levels = c("pre", "post","fu")),y = na), group=group)+stat_summary(fun= mean,geom = 'point',size=3,aes(color=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_normal,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Negative Affect')+ggtitle('Change in negative affect')
ggplot(ema,aes(x=factor(timepoint,levels = c("pre", "post","fu")),y = pa), group=group)+stat_summary(fun= mean,geom = 'point',size=3,aes(color=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_normal,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Positive Affect')+ggtitle('Change in positive affect')
ggplot(ema,aes(x=factor(timepoint,levels = c("pre", "post","fu")),y = dss4), group=group)+stat_summary(fun= mean,geom = 'point',size=3,aes(color=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_normal,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('DSS-4')+ggtitle('Change in dissociative state')
ggplot(ema,aes(x=factor(timepoint,levels = c("pre", "post","fu")),y = tens), group=group)+stat_summary(fun= mean,geom = 'point',size=3,aes(color=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_normal,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Inner tension')+ggtitle('Change in inner tension')
ggplot(ema,aes(x=factor(timepoint,levels = c("pre", "post","fu")),y = g_exp), group=group)+stat_summary(fun= mean,geom = 'point',size=3,aes(color=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_normal,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Good experience intensity')+ggtitle('Change in reactivity to good experiences')
ggplot(ema,aes(x=factor(timepoint,levels = c("pre", "post","fu")),y = b_exp), group=group)+stat_summary(fun= mean,geom = 'point',size=3,aes(color=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_normal,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Bad experience intensity')+ggtitle('Change in reactivity to bad experiences')

#MSSD
ggplot(ema,aes(x=factor(timepoint,levels = c("pre", "post","fu")),y = mssd_na), group=group)+stat_summary(fun= mean,geom = 'point',size=3,aes(color=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_normal,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Negative Affect (MSSD)')+ggtitle('Instability of negative affect')
ggplot(ema,aes(x=factor(timepoint,levels = c("pre", "post","fu")),y = mssd_pa), group=group)+stat_summary(fun= mean,geom = 'point',size=3,aes(color=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_normal,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Positive Affect (MSSD)')+ggtitle('Instability of positive affect')
ggplot(ema,aes(x=factor(timepoint,levels = c("pre", "post","fu")),y = mssd_dss4), group=group)+stat_summary(fun= mean,geom = 'point',size=3,aes(color=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_normal,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('DSS-4 (MSSD)')+ggtitle('Instability of dissociative state')
ggplot(ema,aes(x=factor(timepoint,levels = c("pre", "post","fu")),y = mssd_tens), group=group)+stat_summary(fun= mean,geom = 'point',size=3,aes(color=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_normal,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Inner tension (MSSD)')+ggtitle('Instability of inner tension')
ggplot(ema,aes(x=factor(timepoint,levels = c("pre", "post","fu")),y = mssd_ge), group=group)+stat_summary(fun= mean,geom = 'point',size=3,aes(color=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_normal,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Good experience intensity (MSSD)')+ggtitle('Instability of reactivity to good experiences')
ggplot(ema,aes(x=factor(timepoint,levels = c("pre", "post","fu")),y = mssd_be), group=group)+stat_summary(fun= mean,geom = 'point',size=3,aes(color=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_normal,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Bad experience intensity (MSSD)')+ggtitle('Instability of reactivity to bad experiences')
