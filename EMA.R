#EMA BrainBoost
#Miroslava Jindrova, ZI Mannheim, 2021

library(readxl)
library(dplyr)
library(data.table)
library(tidyverse)
library(reshape)

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
demographics<-read_xlsx('//zi.local/flstorage/dep_psm/group_psm/AG-Paret/Projects/BrainBoost/data_analysis/demographics.xlsx')
group_info<-select(filter(demographics, is.na(Comments)|!Comments=='dropped out'), subject=Subject, group=Group)
all<-merge(all, group_info, by='subject')

##add info about the test day
#triggers<-select(all,subject, timepoint,Trigger_date, Form)
all<- all %>% mutate(Trigger_date=substr(Trigger_date,1,10),Trigger_time=substr(Trigger_time,12,19))%>% 
  mutate(Trigger_date=as.Date(Trigger_date)) %>%
  group_by(subject, timepoint)%>%
  mutate(test_day=as.character (Trigger_date-min(Trigger_date)))

#sub07fu did the last test day separately, so we want to change the number of the test day there
all<-all[-which(all$subject=='sub07'&all$timepoint=='fu'&all$test_day==4),]
all$test_day[which(all$subject=='sub07'&all$timepoint=='fu'&all$test_day==6)]<-4
#drop out triggers later than the last test day 
all<-filter(all, test_day=='0'|test_day=='1'|test_day=='2'|test_day=='3'|test_day=='4')

#RESPONSE RETURN
all<-group_by(all, subject, timepoint, test_day, group)
return<-count(all, Form)
return<-return[-which(return$test_day=='0'),] #delete test day 0 
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
PANAS<-select(filter(all, Form=='Affekt'), !Trigger, !Trigger_date, !Trigger_time, !Trigger_counter, !where(~is.na(.x)))

