# BrainBoost realtime data 
# read the datafile, extract relevant data, calculate Personal Effect Size, plot 
# Miroslava Jindrova, ZI Mannheim, 2022

library(readxl)
library(dplyr)
library(data.table)
library(ggplot2)
library(stringr)
library(Hmisc)
library(reshape)
library(effectsize)

#load data
setwd('//zi.local/flstorage/dep_psm/group_psm/AG-Paret/Projects/BrainBoost/data_analysis/realtime')
data<-read.csv('trainingData20220330T181545.csv')

#extract relevant data
data$functional_image_fn[which(data$subject=='sub-14'& data$session=='ses-training3')]<-str_replace(data$functional_image_fn[which(data$subject=='sub-14'& data$session=='ses-training3')],'SUB14_NF3.SUB14_NF3','SUB14NF3.SUB14NF3') #correct naming for sub-14, ses-training3
data<- mutate(data,imageNum=as.numeric(substr(functional_image_fn,58,60)))

##personal effect size (down-view)
#Extract ROI data, substract rois and calculate z score
roiData<-data[which(data$condition=='DOWN_trial'|data$condition=='VIEW_trial'),c('condition','subject','session','NFrun', 'ROI1_filt_detrend','ROI2_filt_detrend')]
roiData<-mutate(roiData, roi_diff=ROI1_filt_detrend-ROI2_filt_detrend)
roiData<-group_by(roiData, subject, session, NFrun)
roiData<-mutate(roiData, z_score=c(scale(roi_diff)))

#add block numbers (every block has 63 TRs)
roiData<-mutate(roiData, block=rep(c(rep(1,63),rep(2,63),rep(3,63),rep(4,63)),length(condition)/4/63))
#calculate mean and pooled SD for each condition/block
roiData<-group_by(roiData,subject, session, NFrun, block, condition)
PES<-summarise(roiData,mean_z=mean(z_score))
PES<-cast(PES,subject+session+NFrun+block~condition)
roiData<-group_by(roiData,subject, session, NFrun, block)
PES<-merge(PES,summarise(roiData, pooledSD=sd_pooled(z_score[which(condition=='VIEW_trial')], z_score[which(condition=='DOWN_trial')])))
#substract the conditions and divide by pooled SD
PES<- mutate(PES, blockES=(DOWN_trial-VIEW_trial)/pooledSD)

#add info about dropping out and remove these subjects
demographics<-read_xlsx('//zi.local/flstorage/dep_psm/group_psm/AG-Paret/Projects/BrainBoost/data_analysis/demographics.xlsx')
group_info<-select(filter(demographics, is.na(Comments)|!Comments=='dropped out'), subject=Subject)
group_info<-mutate(group_info, subject=paste('sub-',substr(subject,4,5),sep=""))
PES<-merge(PES, group_info, by='subject')

#number the runs from 1-6
PES<-mutate(PES, session=as.numeric(substr(session,13,13)), NFrun=as.numeric(substr(NFrun,6,6)))
PES<-mutate(PES, run=(session-1+(NFrun-1+session)))
PES<-group_by(PES, subject, session, run)
cycP<-summarise(PES, PES=mean(blockES))

#plots
ggplot(cycP,aes(run,PES))+stat_summary(fun.y = mean,geom = 'point')+stat_summary(fun.y = mean,geom = 'line', aes(group=1))+stat_summary(fun.data = mean_cl_normal,geom = 'errorbar', width=0.4)+scale_x_continuous(breaks = c(1:6))

ggplot(cycP,aes(session,PES))+stat_summary(fun.y = mean,geom = 'point')+stat_summary(fun.y = mean,geom = 'line', aes(group=1))+stat_summary(fun.data = mean_cl_normal,geom = 'errorbar', width=0.4)+scale_x_continuous(breaks = c(1:6))


##raw fb value
cleanData<-data[which(data$condition=='DOWN_trial'),c('condition', 'imageNum', 'raw_fb_value','subject','session','NFrun')]

#sort by pics (every pic is presented for 9 TRs)
cleanData<-mutate(cleanData, picTR=rep(c(1:9),length(imageNum)/9))
cleanData<-group_by(cleanData, subject, session, NFrun, picTR)
picData<-summarise(cleanData, raw_fb=mean(raw_fb_value))
picData<-group_by(picData, subject, picTR)
picData<-summarise(picData,raw_fb=mean(raw_fb))

#calculate mean raw fb value for NF run for each person and session
cleanData<-ungroup(cleanData)
cleanData<-group_by(cleanData, subject, session, NFrun)
aggrData<- summarise(cleanData, raw_fb=mean(raw_fb_value))
aggrData<-ungroup(aggrData)

#remove subjects that dropped out
aggrData<-merge(aggrData, group_info, by='subject')

#number the runs from 1-6
aggrData<-mutate(aggrData, session=as.numeric(substr(session,13,13)), NFrun=as.numeric(substr(NFrun,6,6)))
aggrData<-mutate(aggrData, run=(session-1+(NFrun-1+session)))

#plots
ggplot(aggrData,aes(run,raw_fb))+stat_summary(fun.y = mean,geom = 'point')+stat_summary(fun.y = mean,geom = 'line', aes(group=1))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.4)+scale_x_continuous(breaks = c(1:6))

ggplot(aggrData,aes(session,raw_fb))+stat_summary(fun.y = mean,geom = 'point')+stat_summary(fun.y = mean,geom = 'line', aes(group=1))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.4)+scale_x_continuous(breaks = c(1:6))

ggplot(aggrData,aes(run,raw_fb, colour=subject))+geom_point()+geom_line()#+geom_smooth(method="lm")

ggplot(picData, aes(picTR,raw_fb))+stat_summary(fun.y = mean,geom = 'point')+stat_summary(fun.y = mean,geom = 'line', aes(group=1))+stat_summary(fun.data = mean_cl_boot,geom = 'errorbar', width=0.4)+scale_x_continuous(breaks = c(1:9))

