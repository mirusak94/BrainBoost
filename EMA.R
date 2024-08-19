#EMA BrainBoost
#Miroslava Jindrova, ZI Mannheim, 2021-2024

### packages & functions ###

library(readxl)
library(plyr)
library(dplyr)
library(data.table)
library(tidyverse)
library(reshape)
library(psych)
library(Hmisc)
library(chron)
library(pastecs)
library(WRS2)
library(nlme)
library(lme4)
library(emmeans)
library(car)
library(rstatix)
library(ggpubr)
library(ez)

source('functions/grouped_norm_hist.R') #function for histograms per group

################### load data and pre-clean ################### 

  #load data into one file
    folders<-list.files(path = "W:/group_psm/share/MR_Backup/Datengrab/Brainboost/moviesens/EMA", recursive = TRUE,pattern = "*xlsx", full.names = TRUE)
    files<-sapply(folders,read_xlsx)
    all<-rbindlist(files,idcol = "subject", fill = TRUE)
  #create columns containing subject code and timepoint
    all<- all %>% mutate(timepoint=substr(subject,76,77),subject=substr(subject,71,75)) %>%
      relocate(timepoint,.after=subject) %>%
      mutate(timepoint= gsub('pr','pre',timepoint),
             timepoint= gsub('po','post',timepoint),
             Form= gsub('DSS-4','DSS4',Form))
    
    
  #drop columns where all values are NA
    all<-select_if(all,~any(!is.na(.)))
    
  #add info about randomization and drop the participants that dropped out before post 
    demographics<-read_xlsx('//zi.local/flstorage/dep_psm/group_psm/AG-Paret/Projects/BrainBoost/data_analysis/demographics.xlsx')
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
  #also adjust trigger_counter on the last day
    all$Trigger_counter[which(all$subject=='sub07'&all$timepoint=='fu'&all$test_day==4)]<-all$Trigger_counter[which(all$subject=='sub07'&all$timepoint=='fu'&all$test_day==4)]+40
    
    
    
################################ RESPONSE RETURN ################################
  all<-dplyr::group_by(all, subject, timepoint, test_day, group)
  return<-count(all, Form) 
  return<-ungroup(return)
  
  #to avoid errors, count triggers where some data are missing
  n_trigger_empty<-data.frame('Form'=c('Anspannung','Affekt','DSS4','Erlebnisse'), 
                              'triggers_total'=c(sum(all$Form=='Anspannung'),
                                                 sum(all$Form=='Affekt'),
                                                 sum(all$Form=='DSS4'),
                                                 sum(all$Form=='Erlebnisse')),
                              'triggers_incomplete'=c(sum(all$Form=='Anspannung'&is.na(all$anspann_01)),
                                                      sum(all$Form=='Affekt'&(is.na(all$pana_glucklich)|is.na(all$pana_traurig)|is.na(all$pana_angstlich)|is.na(all$pana_entspannt)|is.na(all$pana_gereizt)|is.na(all$pana_zufrieden)|is.na(all$pana_wutend)|is.na(all$pana_frohlich)|is.na(all$pana_niedergeschlagen)|is.na(all$pana_enthusiastisch))),
                                                      sum(all$Form=='DSS4'&(is.na(all$dss4_01)|is.na(all$dss4_02)|is.na(all$dss4_03)|is.na(all$dss4_04))),
                                                      sum(all$Form=='Erlebnisse'&(is.na(all$erleb__schlecht)|is.na(all$erleb__gut)))))
  
  #calculate % response rate
  return<-group_by(return, subject, timepoint, group, Form)
  return_sum<-summarise(return, return=mean(n)/13*100)
  return_sum<-return_sum[!c(return_sum$Form=='Missing'| return_sum$Form=='Charge phone'),]
  
  #plot the responses
    #tension 
    #control group
    ggplot(data=filter(return, group == 'C', Form=='Anspannung'), aes(x=timepoint,y=n, fill=test_day))+geom_boxplot()+ylim(1,13) +xlab('timepoint')+ylab('triggers answered')+ ggtitle('control group - tension response')
    #nf group
    ggplot(data=filter(return, group == 'E', Form=='Anspannung'), aes(x=timepoint,y=n, fill=test_day))+geom_boxplot()+ylim(1,13) +xlab('timepoint')+ylab('triggers answered')+ ggtitle('NF group - tension response')
    
    #PANAS 
    #control group
    ggplot(data=filter(return, group == 'C', Form=='Affekt'), aes(x=timepoint,y=n, fill=test_day))+geom_boxplot()+ylim(1,13) +xlab('timepoint')+ylab('triggers answered')+ ggtitle('control group - PANAS response')
    #nf group
    ggplot(data=filter(return, group == 'E', Form=='Affekt'), aes(x=timepoint,y=n, fill=test_day))+geom_boxplot()+ylim(1,13) +xlab('timepoint')+ylab('triggers answered')+ ggtitle('NF group - PANAS response')
    
    #DSS4 
    #control group
    ggplot(data=filter(return, group == 'C', Form=='DSS4'), aes(x=timepoint,y=n, fill=test_day))+geom_boxplot()+ylim(1,13) +xlab('timepoint')+ylab('triggers answered')+ ggtitle('control group - DSS-4 response')
    #nf group
    ggplot(data=filter(return, group == 'E', Form=='DSS4'), aes(x=timepoint,y=n, fill=test_day))+geom_boxplot()+ylim(1,13) +xlab('timepoint')+ylab('triggers answered')+ ggtitle('NF group - DSS-4 response')
    
    #experiences 
    #control group
    ggplot(data=filter(return, group == 'C', Form=='Erlebnisse'), aes(x=timepoint,y=n, fill=test_day))+geom_boxplot()+ylim(1,13) +xlab('timepoint')+ylab('triggers answered')+ ggtitle('control group - experiences response')
    #nf group
    ggplot(data=filter(return, group == 'E', Form=='Erlebnisse'), aes(x=timepoint,y=n, fill=test_day))+geom_boxplot()+ylim(1,13) +xlab('timepoint')+ylab('triggers answered')+ ggtitle('NF group - experiences response')
    
    #plot % return
    ggplot(data=filter(return_sum, group =='E'), aes(x=timepoint,y=return, fill=Form))+geom_boxplot()+ylim(1,100)+xlab('timepoint')+ylab('% response rate')+ ggtitle('NF group - EMA response rate')+ scale_color_brewer(palette="Dark2")
    ggplot(data=filter(return_sum, group =='C'), aes(x=timepoint,y=return, fill=Form))+geom_boxplot()+ylim(1,100)+xlab('timepoint')+ylab('% response rate')+ ggtitle('control group - EMA response rate')+ scale_color_brewer(palette="Dark2")
    
  #descriptive stats of response rate
  return_sum<-group_by(return_sum, timepoint, group)
  desc_return<-summarise(return_sum, mean=mean(return), sd=sd(return),n=length(unique(subject)))
  
  #plot % return (summary)
  ggplot(data=return_sum, aes(x=timepoint,y=return))+geom_boxplot()+ylim(1,100)+xlab('timepoint')+ylab('% response rate')+ ggtitle('EMA response rate')+facet_grid(~group)
  
  #statistical comparison of response rate below in the statistics section (not significantly different)

  
  
  
############################## CLEAN DATA ############################## 
  
  #(sorted to negative affect, positive affect, dss-4, inner tension, experience)
    neg_af<-na.omit(all[ ,c(1:2,46:47,7, 18:19,22:23,26:27,30:31,34:35)])
    pos_af<-na.omit(all[ ,c(1:2,46:47,7, 16:17,20:21,24:25,28:29,32:33)])
    DSS<-na.omit(all[ ,c(1:2,46:47,7, 38:41)])
    ansp<-na.omit(all[ ,c(1:2,46:47,7, 36:37)])
    exper<-na.omit(all[ ,c(1:2,46:47,7, 42:45)])
  
  ####VARIANCE
  #calculate variance and omit invalid data
    all_clean<-merge(neg_af, merge(pos_af, merge(DSS, merge(ansp,exper,all = TRUE),all = TRUE),all = TRUE))
    times_r<-select(all_clean, starts_with('time_'))
    boxplot(times_r)
    all_clean<-all_clean%>% rowwise() %>%mutate(
        variance=var(c(pana_traurig,pana_gereizt, pana_angstlich, pana_wutend, pana_niedergeschlagen, pana_glucklich,pana_enthusiastisch, pana_entspannt, pana_zufrieden, pana_frohlich)),
        fastest=min(as.numeric(pick(starts_with('time_')))))
    not_valid<-na.omit(all_clean[all_clean$variance<0.1,])
    
  ############### calculate final scores ###############
    
    ### neg affect
      #mean
      neg_af<-mutate(neg_af, sum_neg=(pana_traurig+pana_gereizt+pana_wutend+pana_niedergeschlagen+pana_angstlich))
      
      # SD=successive difference (for MSSD)
      setDT(neg_af)[, neg_af_SD := sum_neg - lag(sum_neg, 1L), by = c("subject","timepoint", "test_day")] 
      neg_af$neg_af_SSD <- neg_af$neg_af_SD^2
      
      # PAC (probability of acute change)
      quantile(abs(neg_af$neg_af_SD), c(.885, .89, .90, .95), na.rm=T)
      neg_af$AC_neg_af <- ifelse(abs(neg_af$neg_af_SD)>=8, 1,0)
      nrow(subset(neg_af, AC_neg_af==1)) / nrow(subset(neg_af, !is.na(AC_neg_af))) 
      
      # summarise per day
      neg_af<-group_by(neg_af, subject, timepoint, group, test_day)#
      na_day<-summarise(neg_af, na=mean(sum_neg),mssd_na_day=mean(neg_af_SSD, na.rm=T), pac_na_day=mean(AC_neg_af, na.rm=T)) 
      na_day<-group_by(na_day, subject, timepoint, group)
      na<-summarise(na_day, na=mean(na),mssd_na=mean(mssd_na_day),pac_na=mean(pac_na_day)) #mean from day
      #neg_af<-group_by(neg_af, subject, timepoint, group)#
      #na<-summarise(neg_af, na=mean(sum_neg),mssd_na=mean(neg_af_SSD, na.rm=T), pac_na=mean(AC_neg_af, na.rm=T)) # mean per timepoint from raw values
      
    ### pos affect
      pos_af<-mutate(pos_af, sum_pos=(pana_glucklich+pana_entspannt+pana_zufrieden+pana_frohlich+pana_enthusiastisch))
      
      # SD=successive difference (for MSSD)
      setDT(pos_af)[, pos_af_SD := sum_pos - lag(sum_pos, 1L), by = c("subject","timepoint", "test_day")] #by tells the code to take time-day-person into account (don't calculate the diff)
      pos_af$pos_af_SSD <- pos_af$pos_af_SD^2
      
      # PAC (probability of acute change)
      quantile(abs(pos_af$pos_af_SD), c(.885, .89, .90, .95), na.rm=T) #%10 höchste veränderung spezifizieren
      pos_af$AC_pos_af <- ifelse(abs(pos_af$pos_af_SD)>=8, 1,0)
      nrow(subset(pos_af, AC_pos_af==1)) / nrow(subset(pos_af, !is.na(AC_pos_af))) #wie viel % über alle personen
      
      # summarise per day
      pos_af<-group_by(pos_af, subject, timepoint, group,test_day)
      pa_day<-summarise(pos_af, pa=mean(sum_pos),mssd_pa_day=mean(pos_af_SSD, na.rm=T), pac_pa_day=mean(AC_pos_af, na.rm=T)) 
      pa_day<-group_by(pa_day, subject, timepoint, group)
      pa<-summarise(pa_day, pa=mean(pa),mssd_pa=mean(mssd_pa_day),pac_pa=mean(pac_pa_day))
      #pos_af<-group_by(pos_af, subject, timepoint, group)
      #pa<-summarise(pos_af, pa=mean(sum_pos),mssd_pa=mean(pos_af_SSD, na.rm=T), pac_pa=mean(AC_pos_af, na.rm=T)) 
      
    ### DSS4
      DSS<-mutate(DSS, dss4_m=(dss4_01+dss4_02+dss4_03+dss4_04)/4)
      
      # SD=successive difference (for MSSD)
      setDT(DSS)[, DSS_SD := dss4_m - lag(dss4_m, 1L), by = c("subject","timepoint", "test_day")] 
      DSS$DSS_SSD <- DSS$DSS_SD^2
      
      # PAC (probability of acute change)
      quantile(abs(DSS$DSS_SD), c(.885, .89, .90, .95), na.rm=T)
      DSS$AC_DSS <- ifelse(abs(DSS$DSS_SD)>=2, 1,0)
      nrow(subset(DSS, AC_DSS==1)) / nrow(subset(DSS, !is.na(AC_DSS))) 
      
      # summarise per day
      DSS<-group_by(DSS, subject, timepoint, group,test_day)
      dss4_day<-summarise(DSS, dss4=mean(dss4_m),mssd_dss4_day=mean(DSS_SSD, na.rm=T), pac_dss4_day=mean(AC_DSS, na.rm=T)) 
      dss4<-summarise(dss4_day, dss4=mean(dss4),mssd_dss4=mean(mssd_dss4_day),pac_dss4=mean(pac_dss4_day))
      #DSS<-group_by(DSS, subject, timepoint, group)
      #dss4<-summarise(DSS, dss4=mean(dss4_m),mssd_dss4=mean(DSS_SSD, na.rm=T), pac_dss4=mean(AC_DSS, na.rm=T)) 
      
    ### tension
      # SD=successive difference (for MSSD)
      setDT(ansp)[, ansp_SD := anspann_01 - lag(anspann_01, 1L), by = c("subject","timepoint", "test_day")] 
      ansp$ansp_SSD <- ansp$ansp_SD^2

      # PAC (probability of acute change)
      quantile(abs(ansp$ansp_SD), c(.885, .89, .90, .95), na.rm=T)
      ansp$AC_ansp <- ifelse(abs(ansp$ansp_SD)>=4, 1,0)
      nrow(subset(ansp, AC_ansp==1)) / nrow(subset(ansp, !is.na(AC_ansp))) 
      
      # summarise per day
      ansp<-group_by(ansp, subject, timepoint, group, test_day)
      inner_tens_day<-summarise(ansp, tens=mean(anspann_01),mssd_tens_day=mean(ansp_SSD, na.rm=T), pac_tens_day=mean(AC_ansp, na.rm=T)) 
      inner_tens<-summarise(inner_tens_day, tens=mean(tens),mssd_tens=mean(mssd_tens_day),pac_tens=mean(pac_tens_day))
      #ansp<-group_by(ansp, subject, timepoint, group)
      #inner_tens<-summarise(ansp, tens=mean(anspann_01),mssd_tens=mean(ansp_SSD, na.rm=T), pac_tens=mean(AC_ansp, na.rm=T)) 
      
    ### experiences                                                       doplnit!
      # SD=successive difference (for MSSD)
      setDT(exper)[, goodE_SD := erleb__gut - lag(erleb__gut, 1L), by = c("subject","timepoint", "test_day")] 
      exper$goodE_SSD <- exper$goodE_SD^2

      setDT(exper)[, badE_SD := erleb__schlecht - lag(erleb__schlecht, 1L), by = c("subject","timepoint", "test_day")] 
      exper$badE_SSD <- exper$badE_SD^2
 
      
      # PAC (probability of acute change)
      quantile(abs(exper$goodE_SD), c(.885, .89, .90, .95), na.rm=T)
      exper$AC_goodE <- ifelse(abs(exper$goodE_SD)>=4, 1,0)
      nrow(subset(exper, AC_goodE==1)) / nrow(subset(exper, !is.na(AC_goodE)))
      quantile(abs(exper$badE_SD), c(.885, .89, .90, .95), na.rm=T)
      exper$AC_badE <- ifelse(abs(exper$badE_SD)>=4, 1,0)
      nrow(subset(exper, AC_badE==1)) / nrow(subset(exper, !is.na(AC_badE))) 
      
      # summarise per day
      exper<-group_by(exper, subject, timepoint, group,test_day)
      exp_gb_day<-summarise(exper, goodE=mean(erleb__gut),badE=mean(erleb__schlecht),mssd_good_day=mean(goodE_SSD, na.rm=T), pac_good_day=mean(AC_goodE, na.rm=T),mssd_bad_day=mean(badE_SSD, na.rm=T), pac_bad_day=mean(AC_badE, na.rm=T))
      exp_gb<-summarise(exp_gb_day, goodE=mean(goodE),badE=mean(badE),mssd_good=mean(mssd_good_day),mssd_bad=mean(mssd_bad_day),pac_good=mean(pac_good_day),pac_bad=mean(pac_bad_day))
      #exper<-group_by(exper, subject, timepoint, group)
      #exp_gb<-summarise(exper, goodE=mean(erleb__gut),badE=mean(erleb__schlecht),mssd_good=mean(goodE_SSD, na.rm=T), pac_good=mean(AC_goodE, na.rm=T),mssd_bad=mean(badE_SSD, na.rm=T), pac_bad=mean(AC_badE, na.rm=T))
      
  #merge all scores in one table
    ema<-merge(merge(merge(merge(pa,na),dss4),inner_tens),exp_gb)
    ema_days<-merge(merge(merge(merge(pa_day,na_day),dss4_day),inner_tens_day),exp_gb_day)
    ema_all<-merge(merge(merge(merge(pos_af[,c(1:5,16,18:19)],neg_af[,c(1:5,16,18:19)], all = TRUE),DSS[,c(1:5,10,12:13)], all = TRUE),ansp[,c(1:6,9:10)], all = TRUE),exper[,c(1:6,8,11,13:15)], all = TRUE)
    
    ema$timepoint<-factor(ema$timepoint,levels = c("pre", "post","fu"))
    
    remove(na, na_day, neg_af, pa, pa_day, pos_af, ansp, inner_tens, inner_tens_day, exper, exp_gb, exp_gb_day, dss4, dss4_day, DSS) #clean workspace
##### plots (main) #####
  #mean
    ggplot(ema,aes(x=timepoint,y = na), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Negative Affect (mean)')+ggtitle('Change in negative affect')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())
    ggplot(ema,aes(x=timepoint,y = pa), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Positive Affect (mean)')+ggtitle('Change in positive affect')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())
    ggplot(filter(ema, subject!='sub18'),aes(x=timepoint,y = pa), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Positive Affect (mean)')+ggtitle('Change in positive affect (sub18 excluded)')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())
    ggplot(ema,aes(x=timepoint,y = dss4), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('DSS-4 (mean)')+ggtitle('Change in dissociative state')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())
    ggplot(ema,aes(x=timepoint,y = tens), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Inner tension (mean)')+ggtitle('Change in inner tension')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())
    ggplot(filter(ema, subject!='sub18'),aes(x=timepoint,y = tens), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Inner tension (mean)')+ggtitle('Change in inner tension (sub18 excluded)')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())
    # good exp looks good
    ggplot(ema,aes(x=timepoint,y = goodE), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Good experience intensity (mean)')+ggtitle('Change in reactivity to good experiences')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())
    ggplot(filter(ema, subject!='sub18'),aes(x=timepoint,y = goodE), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Good experience intensity (mean)')+ggtitle('Change in reactivity to good experiences (sub18 excluded)')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())
    ggplot(ema,aes(x=timepoint,y = badE), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Bad experience intensity (mean)')+ggtitle('Change in reactivity to bad experiences')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())

  #MSSD
    ggplot(ema,aes(x=timepoint,y = mssd_na), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Negative Affect (MSSD)')+ggtitle('Instability of negative affect')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())
    ggplot(filter(ema, subject!='sub18'),aes(x=timepoint,y = mssd_na), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Negative Affect (MSSD)')+ggtitle('Instability of negative affect')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())
    ggplot(ema,aes(x=timepoint,y = mssd_pa), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Positive Affect (MSSD)')+ggtitle('Instability of positive affect')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())
    ggplot(filter(ema, subject!='sub29'),aes(x=timepoint,y = mssd_pa), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Positive Affect (MSSD)')+ggtitle('Instability of positive affect')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())
    ggplot(ema,aes(x=timepoint,y = mssd_dss4), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('DSS-4 (MSSD)')+ggtitle('Instability of dissociative state')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())
    ggplot(ema,aes(x=timepoint,y = mssd_tens), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Inner tension (MSSD)')+ggtitle('Instability of inner tension')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())
    ggplot(filter(ema, subject!='sub18'),aes(x=timepoint,y = mssd_tens), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Inner tension (MSSD)')+ggtitle('Instability of inner tension (sub18 excluded)')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())
    ggplot(ema,aes(x=timepoint,y = mssd_good), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Good experience intensity (MSSD)')+ggtitle('Instability of reactivity to good experiences')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())
    ggplot(ema,aes(x=timepoint,y = mssd_bad), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Bad experience intensity (MSSD)')+ggtitle('Instability of reactivity to bad experiences')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())

  #PAC
    ggplot(ema,aes(x=timepoint,y = pac_na), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Negative Affect (PAC)')+ggtitle('Instability of negative affect')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())
    ggplot(ema,aes(x=timepoint,y = pac_pa), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Positive Affect (PAC)')+ggtitle('Instability of positive affect')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())
    ggplot(ema,aes(x=timepoint,y = pac_dss4), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('DSS-4 (PAC)')+ggtitle('Instability of dissociative state')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())
    ggplot(ema,aes(x=timepoint,y = pac_tens), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Inner tension (PAC)')+ggtitle('Instability of inner tension')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())
    ggplot(filter(ema, subject!='sub18'),aes(x=timepoint,y = pac_tens), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Inner tension (PAC)')+ggtitle('Instability of inner tension')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())
    ggplot(ema,aes(x=timepoint,y = pac_good), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Good experience intensity (PAC)')+ggtitle('Instability of reactivity to good experiences')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())
    ggplot(ema,aes(x=timepoint,y = pac_bad), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Bad experience intensity (PAC)')+ggtitle('Instability of reactivity to bad experiences')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())
    
     #sample sizes for plots
    ema_sample_size<-setNames(data.frame(matrix(ncol = 4, nrow = 6)), c("group", "pre", "post", "fu"))
    ema_sample_size$group<-rep(c("E","C"),3)
    ema_sample_size$pre<-c(length(unique(ema$subject[which(ema$group=='E'&ema$timepoint=='pre')])),
                           length(unique(ema$subject[which(ema$group=='C'&ema$timepoint=='pre')])))
    ema_sample_size$post<-c(length(unique(ema$subject[which(ema$group=='E'&ema$timepoint=='post')])),
                            length(unique(ema$subject[which(ema$group=='C'&ema$timepoint=='post')])))
    ema_sample_size$fu<-c(length(unique(ema$subject[which(ema$group=='E'&ema$timepoint=='fu')])),
                          length(unique(ema$subject[which(ema$group=='C'&ema$timepoint=='fu')])))
    
    #plot everything at once
    ema_long<-pivot_longer(ema,cols=4:21, names_to = 'item', values_to='value')
    ema_long$item<-factor(ema_long$item, 
                             labels= c("positive affect (mean)","negative affect (mean)","tension (mean)","dissociative symptoms (mean)","reactivity to good experiences (mean)","reactivity to bad experiences (mean)",
                                       "instability of positive affect (MSSD)","instability of negative affect (MSSD)","instability of tension (MSSD)","instability of dissociative symptoms (MSSD)","instability of reactivity to good experiences (MSSD)","instability of reactivity to bad experiences (MSSD)",
                                       "instability of positive affect (PAC)","instability of negative affect (PAC)","instability of tension (PAC)","instability of dissociative symptoms (PAC)","instability of reactivity to good experiences (PAC)","instability of reactivity to bad experiences (PAC)"),
                             levels = c("pa","na","tens","dss4","goodE","badE","mssd_pa","mssd_na","mssd_tens","mssd_dss4","mssd_good","mssd_bad","pac_pa","pac_na","pac_tens","pac_dss4","pac_good","pac_bad"))
    
    ggplot(ema_long,aes(x=timepoint,y = value), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())+facet_wrap(~item, scales = 'free_y', nrow = 3)
    
    #poster plot /OHBM
    #poster_ema<-ema[,c(1:5,7:8,16:17)]
    #poster_ema<-pivot_longer(poster_ema,cols=4:9, names_to = 'item', values_to='value')
    #poster_ema$item0<-factor(poster_ema$item, labels= c("positive affect (mean)","negative affect (mean)","instability of positive affect (MSSD)","instability of negative affect (MSSD)","reactivity to good experiences (mean)","reactivity to bad experiences (mean)"),levels = c("pa","na","mssd_pa","mssd_na","goodE","badE"))
    #ggplot(poster_ema,aes(x=timepoint,y = value), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())+facet_wrap(~item0, scales = 'free_y', nrow = 3)
    
    
#######################           individual courses           #######################
    #means
    ggplot(ema,aes(x=timepoint,y = na, label=subject), group=group)+geom_point(size=3, aes(color=group, shape=group, group=group))+geom_line(aes(group=subject,color=group))+xlab('Timepoint')+ylab('Negative Affect (mean)')+ggtitle('Change in negative affect')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())
    ggplot(ema,aes(x=timepoint,y = pa,label=subject), group=group)+geom_point(size=3, aes(color=group, shape=group, group=group))+geom_line(aes(group=subject,color=group))+xlab('Timepoint')+ylab('Positive Affect (mean)')+ggtitle('Change in positive affect')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())+geom_text(data=subset(ema, pa > 22))
    ggplot(ema,aes(x=timepoint,y = dss4, label=subject), group=group)+geom_point(size=3, aes(color=group, shape=group, group=group))+geom_line(aes(group=subject,color=group))+xlab('Timepoint')+ylab('DSS-4 (mean)')+ggtitle('Change in dissociative state')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())
    ggplot(ema,aes(x=timepoint,y = tens,label=subject), group=group)+geom_point(size=3, aes(color=group, shape=group, group=group))+geom_line(aes(group=subject,color=group))+xlab('Timepoint')+ylab('Inner tension (mean)')+ggtitle('Change in inner tension')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())+geom_text(data=subset(ema, tens < 1.9))
    ggplot(ema,aes(x=timepoint,y = goodE,label=subject), group=group)+geom_point(size=3, aes(color=group, shape=group, group=group))+geom_line(aes(group=subject,color=group))+xlab('Timepoint')+ylab('Good experience intensity (mean)')+ggtitle('Change in reactivity to good experiences')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())+geom_text(data=filter(ema, group=='C'))
    ggplot(ema,aes(x=timepoint,y = badE,label=subject), group=group)+geom_point(size=3, aes(color=group, shape=group, group=group))+geom_line(aes(group=subject,color=group))+xlab('Timepoint')+ylab('Bad experience intensity (mean)')+ggtitle('Change in reactivity to bad experiences')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())+geom_text(data=subset(ema, badE >3))
    
    #MSSD
    ggplot(ema,aes(x=timepoint,y = mssd_na, label=subject), group=group)+geom_point(size=3, aes(color=group, shape=group, group=group))+geom_line(aes(group=subject,color=group))+xlab('Timepoint')+ylab('Negative Affect (MSSD)')+ggtitle('Instability of negative affect')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())+geom_text(data=subset(ema, mssd_na>60))
    ggplot(ema,aes(x=timepoint,y = mssd_pa, label=subject), group=group)+geom_point(size=3, aes(color=group, shape=group, group=group))+geom_line(aes(group=subject,color=group))+xlab('Timepoint')+ylab('Positive Affect (MSSD)')+ggtitle('Instability of positive affect')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())+geom_text(data=subset(ema, mssd_pa>40))
    ggplot(ema,aes(x=timepoint,y = mssd_dss4, label=subject), group=group)+geom_point(size=3, aes(color=group, shape=group, group=group))+geom_line(aes(group=subject,color=group))+xlab('Timepoint')+ylab('DSS-4 (MSSD)')+ggtitle('Instability of dissociative state')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())
    ggplot(ema,aes(x=timepoint,y = mssd_tens,label=subject), group=group)+geom_point(size=3, aes(color=group, shape=group, group=group))+geom_line(aes(group=subject,color=group))+xlab('Timepoint')+ylab('Inner tension (MSSD)')+ggtitle('Instability of inner tension')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())+geom_text(data=subset(ema, mssd_tens >8))
    ggplot(ema,aes(x=timepoint,y = mssd_good,label=subject), group=group)+geom_point(size=3, aes(color=group, shape=group, group=group))+geom_line(aes(group=subject,color=group))+xlab('Timepoint')+ylab('Good experience intensity (MSSD)')+ggtitle('Instability of reactivity to good experiences')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())+geom_text(data=subset(ema, mssd_good >7))
    ggplot(ema,aes(x=timepoint,y = mssd_bad,label=subject), group=group)+geom_point(size=3, aes(color=group, shape=group, group=group))+geom_line(aes(group=subject,color=group))+xlab('Timepoint')+ylab('Bad experience intensity (MSSD)')+ggtitle('Instability of reactivity to bad experiences')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())+geom_text(data=subset(ema, mssd_bad >7))
    
    #PAC
    ggplot(ema,aes(x=timepoint,y = pac_na, label=subject), group=group)+geom_point(size=3, aes(color=group, shape=group, group=group))+geom_line(aes(group=subject,color=group))+xlab('Timepoint')+ylab('Negative Affect (PAC)')+ggtitle('Instability of negative affect')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())+geom_text(data=subset(ema, pac_na>0.35))
    ggplot(ema,aes(x=timepoint,y = pac_pa, label=subject), group=group)+geom_point(size=3, aes(color=group, shape=group, group=group))+geom_line(aes(group=subject,color=group))+xlab('Timepoint')+ylab('Positive Affect (PAC)')+ggtitle('Instability of positive affect')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())+geom_text(data=subset(ema, pac_pa>0.28))
    ggplot(ema,aes(x=timepoint,y = pac_dss4, label=subject), group=group)+geom_point(size=3, aes(color=group, shape=group, group=group))+geom_line(aes(group=subject,color=group))+xlab('Timepoint')+ylab('DSS-4 (PAC)')+ggtitle('Instability of dissociative state')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())
    ggplot(ema,aes(x=timepoint,y = pac_tens, label=subject), group=group)+geom_point(size=3, aes(color=group, shape=group, group=group))+geom_line(aes(group=subject,color=group))+xlab('Timepoint')+ylab('Inner tension (PAC)')+ggtitle('Instability of inner tension')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())+geom_text(data=subset(ema, pac_tens >0.39))
    ggplot(ema,aes(x=timepoint,y = pac_good, label=subject), group=group)+geom_point(size=3, aes(color=group, shape=group, group=group))+geom_line(aes(group=subject,color=group))+xlab('Timepoint')+ylab('Good experience intensity (PAC)')+ggtitle('Instability of reactivity to good experiences')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())+geom_text(data=subset(ema, pac_good >0.33))
    ggplot(ema,aes(x=timepoint,y = pac_bad, label=subject), group=group)+geom_point(size=3, aes(color=group, shape=group, group=group))+geom_line(aes(group=subject,color=group))+xlab('Timepoint')+ylab('Bad experience intensity (PAC)')+ggtitle('Instability of reactivity to bad experiences')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())+geom_text(data=subset(ema, pac_bad >0.4))
    
    
#######################           day levels           #######################
    #means
    ggplot(ema_days,aes(x=test_day,y = na), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Negative Affect (mean)')+ggtitle('Change in negative affect')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())+facet_grid(~timepoint)
    ggplot(ema_days,aes(x=test_day,y = pa), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Positive Affect (mean)')+ggtitle('Change in positive affect')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())+facet_grid(~timepoint)
    ggplot(ema_days,aes(x=test_day,y = dss4), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('DSS-4 (mean)')+ggtitle('Change in dissociative state')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())+facet_grid(~timepoint)
    ggplot(ema_days,aes(x=test_day,y = tens), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Inner tension (mean)')+ggtitle('Change in inner tension')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())+facet_grid(~timepoint)
    # good exp looks good
    ggplot(ema_days,aes(x=test_day,y = goodE), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Good experience intensity (mean)')+ggtitle('Change in reactivity to good experiences')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())+facet_grid(~timepoint)
    ggplot(ema_days,aes(x=test_day,y = badE), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Bad experience intensity (mean)')+ggtitle('Change in reactivity to bad experiences')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())+facet_grid(~timepoint)
    
    #MSSD
    ggplot(ema_days,aes(x=test_day,y = mssd_na_day), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Negative Affect (MSSD)')+ggtitle('Instability of negative affect')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())+facet_grid(~timepoint)
    ggplot(ema_days,aes(x=test_day,y = mssd_pa_day), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Positive Affect (MSSD)')+ggtitle('Instability of positive affect')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())+facet_grid(~timepoint)
    ggplot(ema_days,aes(x=test_day,y = mssd_dss4_day), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('DSS-4 (MSSD)')+ggtitle('Instability of dissociative state')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())+facet_grid(~timepoint)
    ggplot(ema_days,aes(x=test_day,y = mssd_tens_day), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Inner tension (MSSD)')+ggtitle('Instability of inner tension')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())+facet_grid(~timepoint)
    ggplot(ema_days,aes(x=test_day,y = mssd_good_day), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Good experience intensity (MSSD)')+ggtitle('Instability of reactivity to good experiences')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())+facet_grid(~timepoint)
    ggplot(ema_days,aes(x=test_day,y = mssd_bad_day), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Bad experience intensity (MSSD)')+ggtitle('Instability of reactivity to bad experiences')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())+facet_grid(~timepoint)
    
    #PAC
    ggplot(ema_days,aes(x=test_day,y = pac_na_day), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Negative Affect (PAC)')+ggtitle('Instability of negative affect')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())+facet_grid(~timepoint)
    ggplot(ema_days,aes(x=test_day,y = pac_pa_day), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Positive Affect (PAC)')+ggtitle('Instability of positive affect')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())+facet_grid(~timepoint)
    ggplot(ema_days,aes(x=test_day,y = pac_dss4_day), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('DSS-4 (PAC)')+ggtitle('Instability of dissociative state')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())+facet_grid(~timepoint)
    ggplot(ema_days,aes(x=test_day,y = pac_tens_day), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Inner tension (PAC)')+ggtitle('Instability of inner tension')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())+facet_grid(~timepoint)
    ggplot(ema_days,aes(x=test_day,y = pac_good_day), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Good experience intensity (PAC)')+ggtitle('Instability of reactivity to good experiences')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())+facet_grid(~timepoint)
    ggplot(ema_days,aes(x=test_day,y = pac_bad_day), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+xlab('Timepoint')+ylab('Bad experience intensity (PAC)')+ggtitle('Instability of reactivity to bad experiences')+scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+ theme_bw()+theme(panel.grid.major.x = element_blank())+facet_grid(~timepoint)
    
    
    
########################## statistics - response return ################
    
    #statistically compare if there's a difference in RESPONSE RATE between the groups/timepoints 
    #test assumptions
    return_sum<-cast(return_sum, subject+timepoint+group~Form, value='return')
    
    formVars<-c(colnames(return_sum))
    formCols<-c (4:7)
    
    #outliers
    outliers<-data.frame()
    for (x in formCols) {
      outliers_temp<-return_sum%>%
        group_by(group, timepoint) %>% # you can reduce grouping here
        identify_outliers(formVars[x])
      outliers<-rbind(outliers,cbind(variable=rep(formVars[x],nrow(outliers_temp)),outliers_temp[,c(1:3,8:9)]))
    }
    extreme_outliers=outliers[outliers$is.extreme=='TRUE',] #no extreme outliers if we reduce grouping (suggesting they're caused by too little data)
    
    #Normality
    norm_test<-return_sum%>%
      group_by(group, timepoint) %>%
      shapiro_test(Affekt, Anspannung, DSS4, Erlebnisse)
    norm_test<-mutate(norm_test, NOTnorm=p<0.1)
    not_norm=norm_test[norm_test$NOTnorm=='TRUE',]
    
    #qq plots
    for (q in formCols) {
      qq_return<-ggqqplot(return_sum, formVars[q], ggtheme = theme_bw(), title=formVars[q]) +
        facet_grid(timepoint ~ group, labeller = "label_both")
      print(qq_return)
    }
    
    #histograms
    for (h in formCols) {
      b<-max(get(formVars[h],return_sum))/15
      hist_return <-grouped_norm_hist(return_sum, h, formVars[h],b, day=FALSE, hour=FALSE)
      print(hist_return)
    }
    
    #homogeneity  
    homog<-data.frame()
    for (x in formCols) {
      homog<-rbind(homog,
                   rbind(c(formVars[x],'group',leveneTest(return_sum[,x],return_sum$group)$P[1]),
                         c(formVars[x],'timepoint',leveneTest(return_sum[,x],return_sum$timepoint)$P[1]),
                         c(formVars[x],'timepointxgroup',leveneTest(return_sum[,x],interaction(return_sum$timepoint,return_sum$group))$P[1])))
    }
    colnames(homog)<-c('variable', 'grouping', 'leveneTest_p')
    homog<-mutate(homog, violated=leveneTest_p<0.05)
    
    #variables as factors
    return_sum$timepoint<-factor(return_sum$timepoint, levels = c('pre','post', 'fu'))
    return_sum$group<-factor(return_sum$group, levels=c('C','E'), labels = c('TAU','NF'))
    return_sum$subject<-as.factor(return_sum$subject)
    
    
    #robust anovas
    for (y in formCols) {
      name_anov<-paste(formVars[y],'_anov_robust', sep = "")
      assign(name_anov,bwtrim(data = return_sum,get(formVars[y])~group*timepoint,id=subject))
    }
    
    #now we can have a look on the individual anovas
    Affekt_anov_robust # no sig. effect
    Anspannung_anov_robust # no sig. effect
    DSS4_anov_robust # no sig. effect
    Erlebnisse_anov_robust # no sig. effect
    
    
    
    
    
    
    
    
############################################################### MAIN ANALYSIS ############################################################### 
    
    ########################################## test assumptions on raw data ####
    emaVars<-c(colnames(ema_all))
    emaCols<-c (6:23)
    
    #outliers
    outliers<-data.frame()
    for (x in emaCols) {
      outliers_temp<-ema_all%>%
        group_by(group, timepoint) %>% # you can reduce grouping here
        identify_outliers(emaVars[x])
      outliers<-rbind(outliers,cbind(variable=rep(emaVars[x],nrow(outliers_temp)),outliers_temp[,c(1:3,24:25)]))
    }
    extreme_outliers=outliers[outliers$is.extreme=='TRUE',] 
    boxplot(ema_all$sum_pos[ema_all$timepoint=='pre'&ema_all$group=='C'],ylab='positive affect', main='session: PRE, group: TAU')
    boxplot(ema_all$erleb__schlecht[ema_all$timepoint=='fu'&ema_all$group=='C'],ylab='positive affect', main='session: PRE, group: TAU')

    #Normality
    norm_test<-ema_all%>%
      group_by(group, timepoint) %>%
      shapiro_test(sum_pos, sum_neg, dss4_m, anspann_01, erleb__schlecht, erleb__gut)
    norm_test<-mutate(norm_test, NOTnorm=p<0.05)
    not_norm=norm_test[norm_test$NOTnorm=='TRUE',]
    
    #qq plots
    for (q in emaCols) {
      qq_ema<-ggqqplot(ema_all, emaVars[q], ggtheme = theme_bw(), title=emaVars[q]) +
        facet_grid(timepoint ~ group, labeller = "label_both")
      print(qq_ema)
    }
    
    #histograms
    for (h in emaCols) {
      b<-max(get(emaVars[h],ema_all), na.rm = TRUE)/12
      hist_ema <-grouped_norm_hist(ema_all, h, emaVars[h],b, day=TRUE, hour=TRUE)
      print(hist_ema)
    }
    
    #################################################### test assumptions for mssd&pac/days ####
    emaVars<-c(colnames(ema_days))
    emaCols<-c (6:7,9:10,12:13,15:16,19:22)
    
    #outliers
    outliers<-data.frame()
    for (x in emaCols) {
      outliers_temp<-ema_days%>%
        group_by(group, timepoint) %>% # you can reduce grouping here
        identify_outliers(emaVars[x])
      outliers<-rbind(outliers,cbind(variable=rep(emaVars[x],nrow(outliers_temp)),outliers_temp[,c(1:4,23:24)]))
    }
    extreme_outliers=outliers[outliers$is.extreme=='TRUE',] 
   
    #Normality
    norm_test<-ema_days%>%
      group_by(group, timepoint) %>%
      shapiro_test(mssd_pa_day, mssd_na_day, mssd_dss4_day, mssd_tens_day, mssd_good_day, mssd_bad_day,pac_pa_day, pac_na_day, pac_dss4_day, pac_tens_day, pac_good_day, pac_bad_day)
    norm_test<-mutate(norm_test, NOTnorm=p<0.05)
    not_norm=norm_test[norm_test$NOTnorm=='TRUE',]
    
    #qq plots
    for (q in emaCols) {
      qq_ema<-ggqqplot(ema_days, emaVars[q], ggtheme = theme_bw(), title=emaVars[q]) +
        facet_grid(timepoint ~ group, labeller = "label_both")
      print(qq_ema)
    }
    
    #histograms
    for (h in emaCols) {
      b<-max(get(emaVars[h],ema_days), na.rm = TRUE)/10
      hist_ema <-grouped_norm_hist(ema_days, h, emaVars[h],b, day=TRUE, hour=FALSE)
      print(hist_ema)
    }
    
    
    
    
    ##################################### test assumptions on means per timepoint ###
    emaVars<-c(colnames(ema))
    emaCols<-c (4:length(emaVars))
      
    #outliers
    outliers<-data.frame()
    for (x in emaCols) {
      outliers_temp<-ema%>%
        group_by(group, timepoint) %>% # you can reduce grouping here
        identify_outliers(emaVars[x])
      outliers<-rbind(outliers,cbind(variable=rep(emaVars[x],nrow(outliers_temp)),outliers_temp[,c(1:3,22:23)]))
    }
      extreme_outliers=outliers[outliers$is.extreme=='TRUE',] #no extreme outliers if we reduce grouping (suggesting they're caused by too little data)
      
      #plot groups of data with extreme outliers
      #boxplot(ema$na[ema$timepoint=='pre'&ema$group=='C'],ylab='negative affect (mean)', main='session: PRE, group: TAU')

    #Normality
    norm_test<-ema%>%
          group_by(group, timepoint) %>%
          shapiro_test(pa, mssd_pa, na, mssd_na, dss4, mssd_dss4, tens, mssd_tens, goodE, badE, mssd_good, mssd_bad,pac_pa, pac_na, pac_dss4, pac_tens, pac_good, pac_bad)
    norm_test<-mutate(norm_test, NOTnorm=p<0.1)
    not_norm=norm_test[norm_test$NOTnorm=='TRUE',]
    
    descript<-list(by(ema[,4:21],list(ema$timepoint,ema$group),stat.desc, norm=TRUE, basic=FALSE, desc=FALSE))
    
    #qq plots
    for (q in emaCols) {
      qq_ema<-ggqqplot(ema, emaVars[q], ggtheme = theme_bw(), title=emaVars[q]) +
      facet_grid(timepoint ~ group, labeller = "label_both")
      print(qq_ema)
    }
    
    #histograms
    for (h in emaCols) {
      b<-max(get(emaVars[h],ema))/10
      hist_ema <-grouped_norm_hist(ema, h, emaVars[h],b, day=FALSE, hour=FALSE)
      print(hist_ema)
    }
    
    #homogeneity  
    homog<-data.frame()
    for (x in emaCols) {
      homog<-rbind(homog,
          rbind(c(emaVars[x],'group',leveneTest(ema[,x],ema$group)$P[1]),
                c(emaVars[x],'timepoint',leveneTest(ema[,x],ema$timepoint)$P[1]),
                c(emaVars[x],'timepointxgroup',leveneTest(ema[,x],interaction(ema$timepoint,ema$group))$P[1])))
    }
    colnames(homog)<-c('variable', 'grouping', 'leveneTest_p')
    homog<-mutate(homog, violated=leveneTest_p<0.05)
    
    #violated assumptions summary
    violations<-merge(unique(extreme_outliers[,c(1:3,6)]),not_norm[,c(1:3,6)], all=TRUE)
    violations[is.na(violations)]<-FALSE
    violations$howmany<-violations$is.extreme+violations$NOTnorm
    violated_variables<-unique(violations$variable) #all except for pa & tens
    
    #####calculate diff between pre-post and test for outliers (boxplot) - sub18 out?
    ema_pre<-filter(ema, timepoint=='pre')
    ema_post<-filter(ema, timepoint=='post')
    ema_pre_post<-merge(ema_pre[,-2],ema_post[,-2],by=c('subject','group'), suffixes = c('_pre','_post'))
    ema_change<-ema_pre_post[, 3:(ncol(ema_pre_post)/2 + 1)] - ema_pre_post[, (ncol(ema_pre_post)/2 + 2):ncol(ema_pre_post)]
    ema_change <- data.frame(ema_pre_post[, c('subject','group')], ema_change)
    colnames(ema_change)[3:ncol(ema_change)] <- paste0("diff_", colnames(ema_pre)[4:ncol(ema_pre)])
    remove(ema_pre,ema_post,ema_pre_post) # clean the workspace
    
    
    ############### identify outliers in differences between pre-post #########
    #outliers
    diffVars<-c(colnames(ema_change))
    diffCols<-c (3:length(diffVars))
    
    outliers_diff<-data.frame()
    for (x in diffCols) {
      outliers_temp<-ema_change%>%
        group_by(group) %>%
        identify_outliers(diffVars[x])
      outliers_diff<-rbind(outliers_diff,cbind(variable=rep(diffVars[x],nrow(outliers_temp)),outliers_temp[,c(1:2,21:22)]))
    }
    extreme_outliers_diff=outliers_diff[outliers_diff$is.extreme=='TRUE',] 
    
    ema_change_long<-pivot_longer(ema_change,cols=3:20, names_to = 'item', values_to='value') #the original ema table contains columns for subject, timepoint, group and each ema item)
    ggplot(ema_change_long,aes(x=group,y=value))+geom_boxplot()+facet_wrap(~item, scales = 'free_y', nrow = 3)
    
    ############################################### ANOVAs ###########################################################
    
    #variables as factors
    ema$timepoint<-factor(ema$timepoint, levels = c('pre','post', 'fu'))
    ema$group<-factor(ema$group, levels=c('C','E'), labels = c('TAU','NF'))
    ema$subject<-as.factor(ema$subject)
    
    
    #parametric anovas 
    #paramCols<- c(4,6,10,13)
    
    ezDesign(ema,timepoint, subject) #check missing data

    #parametric anovas
    for (y in emaCols) {
      name_anov<-paste(emaVars[y],'_anov_parametric', sep = "")
      assign(name_anov,anova_test(data=ema, dv=emaVars[y], wid=subject, between=group, within=timepoint, type = 3, detailed = TRUE))
    }
    
    setdiff(emaVars,violated_variables)
    
    #now we can have a look on the individual anovas
    #pa_anov_parametric
    #na_anov_parametric 
    #mssd_pa_anov_parametric
    #mssd_na_anov_parametric
    #dss4_anov_parametric
    #mssd_dss4_anov_parametric
    tens_anov_parametric
    #mssd_tens_anov_parametric
    #badE_anov_parametric  
    #goodE_anov_parametric 
    #mssd_bad_anov_parametric 
    #mssd_good_anov_parametric
    #pac_pa_anov_parametric
    #pac_na_anov_parametric
    #pac_dss4_anov_parametric
    #pac_tens_anov_parametric 
    #pac_good_anov_parametric
    #pac_bad_anov_parametric 
    
    #robust anovas
    for (y in emaCols) {
      name_anov<-paste(emaVars[y],'_anov_robust', sep = "")
      assign(name_anov,bwtrim(data = ema,get(emaVars[y])~group*timepoint,id=subject))
      }
    
    #now we can have a look on the individual anovas
    pa_anov_robust #group effect
    na_anov_robust #group effect
    mssd_pa_anov_robust
    mssd_na_anov_robust
    dss4_anov_robust
    mssd_dss4_anov_robust
    #tens_anov_robust 
    mssd_tens_anov_robust
    badE_anov_robust
    goodE_anov_robust #group effect
    mssd_bad_anov_robust 
    mssd_good_anov_robust
    pac_pa_anov_robust
    pac_na_anov_robust
    pac_dss4_anov_robust
    pac_tens_anov_robust 
    pac_good_anov_robust
    pac_bad_anov_robust 
    

#posthoc parametric
#ema%>%pairwise_t_test(goodE~timepoint,p.adjust.method = "bonferroni")


#posthocrobust
#sppbb(pa~group*timepoint,id=subject,data = ema)
    
    
######################################################################### TRANSFORMATION ######################################################################### 
    
    #transform ema (log(x+1))
    #raw datapoints
    mylog<-function(x) (log(x+1))
    TRema_r<-ema_all
    TRema_r[,c(6:23)]<-mutate_all(TRema_r[,c(6:23)], mylog)  
    TRema_r<-group_by(TRema_r, test_day, subject, timepoint, group) ############################### mean by day and then timepoint or directly by timepoint?
    TRema_r<-summarise(TRema_r, pos=mean(sum_pos, na.rm=TRUE), neg=mean(sum_neg, na.rm=TRUE),dss=mean(dss4_m, na.rm=TRUE), tens=mean(anspann_01, na.rm=TRUE),good=mean(erleb__gut, na.rm=TRUE), bad=mean(erleb__schlecht, na.rm=TRUE))
    
    #mssd&pac/day
    TRema_m<-ema_days[,c(1:4,6:7,9:10,12:13,15:16,19:22)]
    TRema_m[,c(5:16)]<-mutate_all(TRema_m[,c(5:16)], mylog)  
    
    TRema<-merge(TRema_m,TRema_r,all = TRUE)
    TRema<-group_by(TRema, subject, timepoint, group)
    TRema<-summarise(TRema, pa=mean(pos), na=mean(neg), tens=mean(tens), dss4=mean(dss), goodE=mean(good), badE=mean(bad), 
                     mssd_pa=mean(mssd_pa_day), mssd_na=mean(mssd_na_day), mssd_tens=mean(mssd_tens_day), mssd_dss4=mean(mssd_dss4_day), mssd_good=mean(mssd_good_day), mssd_bad=mean(mssd_bad_day),
                     pac_pa=mean(pac_pa_day), pac_na=mean(pac_na_day), pac_tens=mean(pac_tens_day), pac_dss4=mean(pac_dss4_day), pac_good=mean(pac_good_day), pac_bad=mean(pac_bad_day))
    TRema<-data.frame(TRema)

 ##################################### test assumptions on means per timepoint (transformed data) ################
    emaVars<-c(colnames(TRema))
    emaCols<-c (4:length(emaVars))
    #outliers
    outliers<-data.frame()
    for (x in emaCols) {
      outliers_temp<-TRema%>%
        group_by(group, timepoint) %>% # you can reduce grouping here
        identify_outliers(emaVars[x])
      outliers<-rbind(outliers,cbind(variable=rep(emaVars[x],nrow(outliers_temp)),outliers_temp[,c(1:3,22:23)]))
    }
    extreme_outliers=outliers[outliers$is.extreme=='TRUE',] #no extreme outliers if we reduce grouping (suggesting they're caused by too little data)
    
    #plot groups of data with extreme outliers
    #boxplot(TRema$badE[TRema$timepoint=='fu'&TRema$group=='E'],ylab='bad exp (mean)', main='session: FU, group: NF')
    
    #Normality
    norm_test<-TRema%>%
      group_by(group, timepoint) %>%
      shapiro_test(pa, mssd_pa, na, mssd_na, dss4, mssd_dss4, tens, mssd_tens, goodE, badE, mssd_good, mssd_bad,pac_pa, pac_na, pac_dss4, pac_tens, pac_good, pac_bad)
    norm_test<-mutate(norm_test, NOTnorm=p<0.05)
    not_norm=norm_test[norm_test$NOTnorm=='TRUE',]
    
    descript<-list(by(TRema[,4:length(emaVars)],list(TRema$timepoint,TRema$group),stat.desc, norm=TRUE, basic=FALSE, desc=FALSE))
    
    #qq plots
    for (q in emaCols) {
      qq_TRema<-ggqqplot(TRema, emaVars[q], ggtheme = theme_bw(), title=emaVars[q]) +
        facet_grid(timepoint ~ group, labeller = "label_both")
      print(qq_TRema)
    }
    
    
    #histograms
    for (h in emaCols) {
      b<-max(get(emaVars[h],TRema))/15
      hist_ema <-grouped_norm_hist(TRema, h, emaVars[h],b, day=FALSE, hour=FALSE)
      print(hist_ema)
    }
    
    #homogeneity  
    homog<-data.frame()
    for (x in emaCols) {
      homog<-rbind(homog,
                   rbind(c(emaVars[x],'group',leveneTest(TRema[,x],TRema$group)$P[1]),
                         c(emaVars[x],'timepoint',leveneTest(TRema[,x],TRema$timepoint)$P[1]),
                         c(emaVars[x],'timepointxgroup',leveneTest(TRema[,x],interaction(TRema$timepoint,TRema$group))$P[1])))
    }
    colnames(homog)<-c('variable', 'grouping', 'leveneTest_p')
    homog<-mutate(homog, violated=leveneTest_p<0.05)
    
    #violated assumptions summary
    violations<-merge(unique(extreme_outliers[,c(1:3,6)]),not_norm[,c(1:3,6)], all=TRUE)
    violations[is.na(violations)]<-FALSE
    violations$howmany<-violations$is.extreme+violations$NOTnorm
    violated_variables<-unique(violations$variable) #all except for pa & tens
    
    ##analysis
    #variables as factors
    TRema$timepoint<-factor(TRema$timepoint, levels = c('pre','post', 'fu'))
    TRema$group<-factor(TRema$group, levels=c('C','E'), labels = c('TAU','NF'))
    TRema$subject<-as.factor(TRema$subject)
    

    #parametric anovas for PA, (NA), TENS, (badE)
    #paramCols<- c(4,6,10,13)
    
    ezDesign(TRema,timepoint, subject) #check missing data
    
    ################# ANOVAs on transformed data ##############
    #parametric anovas
    for (y in emaCols) {
      name_anov<-paste(emaVars[y],'_anov_parametric', sep = "")
      assign(name_anov,anova_test(data=TRema, dv=emaVars[y], wid=subject, between=group, within=timepoint, type = 3, detailed = TRUE))
    }
    
    setdiff(emaVars,violated_variables)
    
    #now we can have a look on the individual anovas
    pa_anov_parametric
    na_anov_parametric #time effect
    mssd_pa_anov_parametric
    mssd_na_anov_parametric
    #dss4_anov_parametric
    #mssd_dss4_anov_parametric
    #tens_anov_parametric
    #mssd_tens_anov_parametric
    #badE_anov_parametric  
    #goodE_anov_parametric
    mssd_bad_anov_parametric #time effect
    mssd_good_anov_parametric
    pac_pa_anov_parametric
    #pac_na_anov_parametric
    #pac_dss4_anov_parametric
    pac_tens_anov_parametric #group x time interaction
    pac_good_anov_parametric
    #pac_bad_anov_parametric   
    
    
    
    
##################################################### MULTILEVEL ANALYSIS ##################################################### 
     

    
    ### adjust this code to our data and do it in a loop (don't forget to set different distribution for means and mssd/pac)
    #factors
    ema_all$timepoint<-factor(ema_all$timepoint, levels = c('pre','post', 'fu'))
    ema_all$group<-factor(ema_all$group, levels=c('C','E'), labels = c('TAU','NF'))
    ema_all$subject<-as.factor(ema_all$subject)
    
    ############################## means


    emaVars<-c(colnames(ema_all))
    meanCols<-c (6,9,12,15,18,19)
    models_norm<-c()
    
    for (y in meanCols) {
      name_res<-paste(emaVars[y],'_res', sep = "")
      variable<-ema_all[,emaVars[y]]
      print(paste("Running lme for:", emaVars[y]))
      assign(name_res,lme(variable ~ group+timepoint+group*timepoint, random = ~ 1 | subject/timepoint, data=ema_all, na.action=na.omit, method='ML'))
      models_norm<-c(models_norm,name_res)
      }
    
    
    summary(sum_neg_res)
    summary(sum_pos_res)
    summary(dss4_m_res)
    summary(anspann_01_res)
    summary(erleb__gut_res)
    summary(erleb__schlecht_res)
    
    emmeans(sum_neg_res, list(pairwise ~ group*timepoint), adjust = "none") #change the first argument to see other models
  
    #residuals
    for (y in models_norm) {
      res<-get(y)
      hist(res$residuals,main=y) #hist resid
      qqnorm(resid(res), main=y) #qq plot
      qqline(resid(res))
    }
    
        
    ############################## ssds
    
    ssdCols<-c (7,10,13,16,20,21)

    
    for (y in ssdCols) {
      name_res<-paste(emaVars[y],'_res', sep = "")
      variable<-ema_all[,emaVars[y]]
      print(paste("Running glmer for:", emaVars[y]))
      assign(name_res,glmer(0.1+variable ~ group +timepoint+group*timepoint + (1 | subject/timepoint), data = ema_all,#+0.1 - damit null werte nicht rausfallen
                            family = Gamma(link=log),
                            control = glmerControl(optimizer = "bobyqa"), nAGQ = 1))
    }
    
    summary(pos_af_SSD_res)
    summary(neg_af_SSD_res)
    summary(DSS_SSD_res)
    summary(ansp_SSD_res)
    summary(goodE_SSD_res)
    summary(badE_SSD_res)
    
    emmeans(pos_af_SSD_res, list(pairwise ~ group*timepoint), adjust = "none") #change the first argument to see other models
        
    
    ############################# pacs

    
    
    pacCols<-c (8,11,14,17,22,23)
    
    
    for (y in pacCols) {
      name_res<-paste(emaVars[y],'_res', sep = "")
      variable<-ema_all[,emaVars[y]]
      print(paste("Running glmer for:", emaVars[y]))
      assign(name_res,glmer(variable ~ group +timepoint+group*timepoint + (1 | subject/timepoint), data = ema_all, 
                 family = binomial(link=logit),
                 control = glmerControl(optimizer = "bobyqa"), nAGQ = 1))
    }
    
    
    summary(AC_pos_af_res)
    summary(AC_neg_af_res)
    summary(AC_DSS_res)
    summary(AC_ansp_res)
    summary(AC_goodE_res)
    summary(AC_badE_res)
    
    emmeans(AC_pos_af_res, list(pairwise ~ group*timepoint), adjust = "none") #change the first argument to see other models

    

    
    
    
    
    

    
    ############## exploring and cleaning of reaction time data #################
    all_clean<-mutate(all_clean, sum_neg=(pana_traurig+pana_gereizt+pana_wutend+pana_niedergeschlagen+pana_angstlich),sum_pos=(pana_glucklich+pana_entspannt+pana_zufrieden+pana_frohlich+pana_enthusiastisch))
    
    react_times<-all_clean[,c(1:5,7,9,11,13,15,17,19,21,23,25,31,33,35,38,39)] 
    
    rtVars<-c(colnames(react_times))
    rtCols<-c (5:18)
    
    #histograms
    for (h in rtCols) {
      reaHist<-ggplot(react_times,aes(x=get(rtVars[h],react_times)))+geom_histogram()+xlab(rtVars[h])+facet_wrap(~timepoint)
      print(reaHist)
    } #group by timepoint
      
    
    for (h in rtCols) {
      b<-max(get(rtVars[h],react_times), na.rm = TRUE)/15
      hist_rt <-grouped_norm_hist(react_times, h, rtVars[h],b, day=TRUE, hour=TRUE)
      print(hist_rt)
    } #group by timepoint x group

   ######################################### LOG TRANSFORMATION
    react_times_transform<-react_times
    react_times_transform[,c(6:18)]<-mutate_all(react_times_transform[,c(6:18)], log)  
    
    
    #### histograms on transformed data
    for (h in rtCols) {
      reaHist<-ggplot(react_times_transform,aes(x=get(rtVars[h],react_times_transform)))+geom_histogram()+xlab(rtVars[h])+facet_wrap(~timepoint)
      print(reaHist)
    } #group by timepoint
    
    for (h in rtCols) {
      b<-max(get(rtVars[h],react_times_transform), na.rm = TRUE)/15
      hist_rt <-grouped_norm_hist(react_times_transform, h, rtVars[h],b, day=TRUE, hour=TRUE)
      print(hist_rt)
    } #group by timepoint x group
    
    #averages (panas) per prompt
    react_times_transform<-react_times_transform%>%
      group_by(subject, timepoint, test_day, Trigger_counter)%>%
      mutate(time_neg=mean(time_pana_niedergeschlagen,time_pana_angstlich, time_pana_gereizt, time_pana_traurig, time_pana_wutend),
             time_pos=mean(time_pana_glucklich, time_pana_entspannt, time_pana_zufrieden, time_pana_frohlich, time_pana_enthusiastisch),
             time_base=mean(time_erleb__gut,time_erleb__schlecht, na.rm=TRUE)) #add tension too?
   
    RTt<-react_times_transform[,c(1:5,19:23)] #extract only the data for the MLM
    
    ############################################################ REACTION TIMES MLM ANALYSIS ################
    #factors
    RTt$timepoint<-factor(RTt$timepoint, levels = c('pre','post', 'fu'))
    RTt$group<-factor(RTt$group, levels=c('C','E'), labels = c('TAU','NF'))
    RTt$subject<-as.factor(RTt$subject)
    
    clarNeg<-lme(time_neg ~ group+timepoint+group*timepoint+sum_neg+time_base, random = ~ 1 | subject/timepoint, data=RTt, na.action=na.omit, method='ML')
    clarPos<-lme(time_pos ~ group+timepoint+group*timepoint+sum_pos+time_base, random = ~ 1 | subject/timepoint, data=RTt, na.action=na.omit, method='ML')
    
    summary(clarNeg)
    summary(clarPos)
    
    ################ reaction times plots ####
    
    RTt_means<-RTt%>% 
      group_by(subject,timepoint,group,test_day)%>%
      summarise(sum_neg=mean(sum_neg), sum_pos=mean(sum_pos), time_neg=mean(time_neg), time_pos=mean(time_pos), time_base=mean(time_base))%>%
      group_by(subject,timepoint,group)%>%
      summarise(sum_neg=mean(sum_neg), sum_pos=mean(sum_pos), time_neg=mean(time_neg), time_pos=mean(time_pos), time_base=mean(time_base))
    
    RTt_means_long<-pivot_longer(RTt_means,cols=4:8, names_to = 'item', values_to='value')
    
    ggplot(RTt_means_long,aes(x=timepoint,y = value), group=group)+stat_summary(fun= mean,geom = 'point',size=3, aes(color=group, shape=group, group=group))+
      stat_summary(fun = mean,geom='line', aes(group=group,color=group))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.2,aes(color=group))+
      xlab('Timepoint')+ylab('Reaction time')+ggtitle('Change in reaction time')+
      scale_color_manual(values=c("#428e77", "#be154d"),labels=c("TAU","NF"))+scale_shape_discrete(labels=c("TAU","NF"))+
      theme_bw()+theme(panel.grid.major.x = element_blank())+facet_wrap(~item, scales = 'free_y')

    
    
    
    
    