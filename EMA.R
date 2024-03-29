#EMA BrainBoost
#Miroslava Jindrova, ZI Mannheim, 2021

library(readxl)
library(dplyr)
library(data.table)
library(tidyverse)
library(reshape)
library(psych)
library(Hmisc)
library(chron)

#load data into one file
folders<-list.files(path = "W:/group_psm/share/MR_Backup/hobbes/Brainboost/moviesens/EMA/abgeschlossen", recursive = TRUE,pattern = "*xlsx", full.names = TRUE)
files<-sapply(folders,read_xlsx)
all<-rbindlist(files,idcol = "subject", fill = TRUE)
#create columns containing subject code and timepoint
all<- all %>% mutate(timepoint=substr(subject,87,88),subject=substr(subject,82,86)) %>%
  relocate(timepoint,.after=subject) %>%
  mutate(timepoint= gsub('pr','pre',timepoint),
         timepoint= gsub('po','post',timepoint))


#drop columns where all values are NA
all<-select_if(all,~any(!is.na(.)))

#add info about randomization and drop the participants that dropped out before post 
demographics<-read_xlsx('Y:/Projects/BrainBoost/data_analysis/demographics.xlsx')
group_info<-select(filter(demographics, is.na(Comments)|!str_detect(Comments,'dropped out')), subject=Subject, group=Group)
all<-merge(all, group_info, by='subject')

##add info about the test day
#triggers<-select(all,subject, timepoint,Trigger_date, Form)
all<- all %>% mutate(Trigger_date=substr(Trigger_date,1,10),Trigger_time=substr(Trigger_time,12,19),Form_start_time=times(substr(Form_start_time,12,19)))%>% 
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

#calculate % response rate
return<-group_by(return, subject, timepoint, group, Form)
return_sum<-summarise(return, return=mean(n)/13*100)
return_sum<-filter(return_sum,Form!='Missing')

#plot % return
ggplot(data=filter(return_sum, group =='E'), aes(x=factor(timepoint,levels = c("pre", "post","fu")),y=return, fill=Form))+geom_boxplot()+ylim(1,100)+xlab('timepoint')+ylab('% response rate')+ ggtitle('NF group - EMA response rate')+ scale_color_brewer(palette="Dark2")
ggplot(data=filter(return_sum, group =='C'), aes(x=factor(timepoint,levels = c("pre", "post","fu")),y=return, fill=Form))+geom_boxplot()+ylim(1,100)+xlab('timepoint')+ylab('% response rate')+ ggtitle('control group - EMA response rate')+ scale_color_brewer(palette="Dark2")

#descriptive stats of response rate
return_sum<-group_by(return_sum, timepoint, group)
desc_return<-summarise(return_sum, mean=mean(return), sd=sd(return),n=length(unique(subject)))

#plot % return (summary)
ggplot(data=return_sum, aes(x=factor(timepoint,levels = c("pre", "post","fu")),y=return))+geom_boxplot()+ylim(1,100)+xlab('timepoint')+ylab('% response rate')+ ggtitle('NF group - EMA response rate')+facet_grid(~group)


##clean DATA 
#(sorted to negative affect, positive affect, dss-4, inner tension, experience)
neg_af<-na.omit(select(all,subject, timepoint,group, test_day, Form_start_time,pana_traurig,pana_gereizt, pana_wutend,pana_niedergeschlagen,pana_angstlich),cols=c(pana_traurig,pana_gereizt, pana_wutend,pana_niedergeschlagen,pana_angstlich))
pos_af<-na.omit(select(all,subject, timepoint,group, test_day, Form_start_time,pana_glucklich,pana_entspannt, pana_zufrieden,pana_frohlich,pana_enthusiastisch),cols=c(pana_glucklich,pana_entspannt, pana_zufrieden,pana_frohlich,pana_enthusiastisch))
DSS<-na.omit(select(all,subject, timepoint,group, test_day, Form_start_time,dss4_01, dss4_02,dss4_03,dss4_04),cols=c(dss4_01, dss4_02,dss4_03,dss4_04))
ansp<-na.omit(select(all,subject, timepoint,group, test_day, Form_start_time,anspann_01),cols=anspann_01)
exper<-na.omit(select(all,subject, timepoint,group, test_day, Form_start_time,erleb__schlecht, erleb__gut),cols=c(erleb__schlecht, erleb__gut))

#calculate final scores
neg_af<-mutate(neg_af, sum_neg=(pana_traurig+pana_gereizt+pana_wutend+pana_niedergeschlagen+pana_angstlich))
neg_af<-group_by(neg_af, subject, timepoint, group, test_day)
na_day<-summarise(neg_af, na=mean(sum_neg),mssd_na=mssd(sum_neg))
na_day<-group_by(na_day, subject, timepoint, group)
na<-summarise(na_day, na=mean(na),mssd_na=mean(mssd_na))

pos_af<-mutate(pos_af, sum_pos=(pana_glucklich+pana_entspannt+pana_zufrieden+pana_frohlich+pana_enthusiastisch))
pos_af<-group_by(pos_af, subject, timepoint, group,test_day)
pa_day<-summarise(pos_af, pa=mean(sum_pos),mssd_pa=mssd(sum_pos))
pa_day<-group_by(pa_day, subject, timepoint, group)
pa<-summarise(pa_day, pa=mean(pa),mssd_pa=mean(mssd_pa))

DSS<-mutate(DSS, dss4_m=(dss4_01+dss4_02+dss4_03+dss4_04)/4)
DSS<-group_by(DSS, subject, timepoint, group,test_day)
dss4_day<-summarise(DSS, dss4=mean(dss4_m),mssd_dss4=mssd(dss4_m))
dss4<-summarise(dss4_day, dss4=mean(dss4),mssd_dss4=mean(mssd_dss4))

ansp<-group_by(ansp, subject, timepoint, group, test_day)
inner_tens_day<-summarise(ansp, tens=mean(anspann_01),mssd_tens=mssd(anspann_01))
inner_tens<-summarise(inner_tens_day, tens=mean(tens),mssd_tens=mean(mssd_tens))
  
exper<-group_by(exper, subject, timepoint, group,test_day)
exp_gb_day<-summarise(exper, g_exp=mean(erleb__gut),b_exp=mean(erleb__schlecht),mssd_ge=mssd(erleb__gut),mssd_be=mssd(erleb__schlecht))
exp_gb<-summarise(exp_gb_day, g_exp=mean(g_exp),b_exp=mean(b_exp),mssd_ge=mean(mssd_ge),mssd_be=mean(mssd_be))

#merge all scores in one table
ema<-merge(merge(merge(merge(pa,na),dss4),inner_tens),exp_gb)

##plots
#means
ggplot(ema,aes(x=factor(timepoint,levels = c("pre", "post","fu")),y = na), group=group)+stat_summary(fun= mean,geom = 'point',size=3,aes(color=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_normal,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Negative Affect')+ggtitle('Change in negative affect')+scale_color_manual(values=c("#D55E00", "#009E73"))
ggplot(ema,aes(x=factor(timepoint,levels = c("pre", "post","fu")),y = pa), group=group)+stat_summary(fun= mean,geom = 'point',size=3,aes(color=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_normal,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Positive Affect')+ggtitle('Change in positive affect')+scale_color_manual(values=c("#D55E00", "#009E73"))
ggplot(ema,aes(x=factor(timepoint,levels = c("pre", "post","fu")),y = dss4), group=group)+stat_summary(fun= mean,geom = 'point',size=3,aes(color=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_normal,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('DSS-4')+ggtitle('Change in dissociative state')+scale_color_manual(values=c("#D55E00", "#009E73"))
ggplot(ema,aes(x=factor(timepoint,levels = c("pre", "post","fu")),y = tens), group=group)+stat_summary(fun= mean,geom = 'point',size=3,aes(color=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_normal,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Inner tension')+ggtitle('Change in inner tension')+scale_color_manual(values=c("#D55E00", "#009E73"))
ggplot(ema,aes(x=factor(timepoint,levels = c("pre", "post","fu")),y = g_exp), group=group)+stat_summary(fun= mean,geom = 'point',size=3,aes(color=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_normal,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Good experience intensity')+ggtitle('Change in reactivity to good experiences')+scale_color_manual(values=c("#006960", "#C7361B"))
# ↑ this one looks good
ggplot(ema,aes(x=factor(timepoint,levels = c("pre", "post","fu")),y = b_exp), group=group)+stat_summary(fun= mean,geom = 'point',size=3,aes(color=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_normal,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Bad experience intensity')+ggtitle('Change in reactivity to bad experiences')+scale_color_manual(values=c("#D55E00", "#009E73"))

#MSSD
ggplot(ema,aes(x=factor(timepoint,levels = c("pre", "post","fu")),y = mssd_na), group=group)+stat_summary(fun= mean,geom = 'point',size=3,aes(color=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_normal,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Negative Affect (MSSD)')+ggtitle('Instability of negative affect')+scale_color_manual(values=c("#D55E00", "#009E73"))
ggplot(ema,aes(x=factor(timepoint,levels = c("pre", "post","fu")),y = mssd_pa), group=group)+stat_summary(fun= mean,geom = 'point',size=3,aes(color=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_normal,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Positive Affect (MSSD)')+ggtitle('Instability of positive affect')+scale_color_manual(values=c("#D55E00", "#009E73"))
ggplot(ema,aes(x=factor(timepoint,levels = c("pre", "post","fu")),y = mssd_dss4), group=group)+stat_summary(fun= mean,geom = 'point',size=3,aes(color=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_normal,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('DSS-4 (MSSD)')+ggtitle('Instability of dissociative state')+scale_color_manual(values=c("#D55E00", "#009E73"))
ggplot(ema,aes(x=factor(timepoint,levels = c("pre", "post","fu")),y = mssd_tens), group=group)+stat_summary(fun= mean,geom = 'point',size=3,aes(color=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_normal,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Inner tension (MSSD)')+ggtitle('Instability of inner tension')+scale_color_manual(values=c("#D55E00", "#009E73"))
ggplot(ema,aes(x=factor(timepoint,levels = c("pre", "post","fu")),y = mssd_ge), group=group)+stat_summary(fun= mean,geom = 'point',size=3,aes(color=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_normal,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Good experience intensity (MSSD)')+ggtitle('Instability of reactivity to good experiences')+scale_color_manual(values=c("#D55E00", "#009E73"))
ggplot(ema,aes(x=factor(timepoint,levels = c("pre", "post","fu")),y = mssd_be), group=group)+stat_summary(fun= mean,geom = 'point',size=3,aes(color=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_normal,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Bad experience intensity (MSSD)')+ggtitle('Instability of reactivity to bad experiences')+scale_color_manual(values=c("#D55E00", "#009E73"))

nf_sample<-length(unique(ema$subject[which(ema$group=='E')]))
ctrl_sample<-length(unique(ema$subject[which(ema$group=='C')]))
nf_sample_pre<-length(unique(ema$subject[which(ema$group=='E'&ema$timepoint=='pre')]))
nf_sample_post<-length(unique(ema$subject[which(ema$group=='E'&ema$timepoint=='post')]))
nf_sample_fu<-length(unique(ema$subject[which(ema$group=='E'&ema$timepoint=='fu')]))
ctrl_sample_pre<-length(unique(ema$subject[which(ema$group=='C'&ema$timepoint=='pre')]))
ctrl_sample_post<-length(unique(ema$subject[which(ema$group=='C'&ema$timepoint=='post')]))
ctrl_sample_fu<-length(unique(ema$subject[which(ema$group=='C'&ema$timepoint=='fu')]))

