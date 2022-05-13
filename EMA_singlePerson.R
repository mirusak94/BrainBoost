#EMA single person analysis
#Miroslava Jindrova, ZI Mannheim, 2022

library(readxl)
library(dplyr)
library(data.table)
library(ggplot2)
library(hms)
library(ggpubr)

#load data into one file
folders<-list.files(path = "//hobbes/daten/PSM/Brainboost/moviesens/EMA/sub12", recursive = TRUE,pattern = "*xlsx", full.names = TRUE)
files<-sapply(folders,read_xlsx, USE.NAMES= TRUE, simplify = FALSE)
alltimes<-rbindlist(files,idcol = "timepoint", fill = TRUE)

sub<-substr(folders[1],51,55) #get info about the participant to save it later
savePath<-'//zi.local/flstorage/dep_psm/group_psm/AG-Paret/Projects/BrainBoost/data_analysis/individual EMA analysis' #where to save the final graphics

#create columns containing subject code and timepoint
alltimes<- alltimes %>% mutate(timepoint=substr(timepoint,56,57)) %>%
  mutate(timepoint= gsub('pr','pre',timepoint),
         timepoint= gsub('po','post',timepoint)) %>%
  #add info about the test day
  mutate(Form_start_date=substr(Form_start_date,1,10),Form_start_time=as.POSIXct(parse_hm(substr(Form_start_time,12,19))))%>% 
  mutate(Form_start_date=as.Date(Form_start_date), timepoint=factor(timepoint, levels = c('pre', 'post', 'fu'))) %>%
  group_by(timepoint)%>%
  mutate(test_day=as.character(Form_start_date-min(Form_start_date))) %>%
  mutate(samplingTime=paste(as.character(timepoint),': ',as.character(min(format(Form_start_date,format='%d/%m/%Y'))),' - ',as.character(max(format(Form_start_date,format='%d/%m/%Y'))), sep = ''))

#clean data
cleandata<-alltimes[c(1,7:9,15:34,36:47)]
cleandata<-cleandata[-which(cleandata$test_day=='0'|cleandata$Form=='Missing'),]
cleandata<-select(cleandata, -contains('time_'))

#analyse DSS-4
cleandata<-mutate(cleandata,dss4mean=(dss4_01+dss4_02+dss4_03+dss4_04)/4) #mean value
#analyse PANAS
cleandata<-mutate(cleandata,PositiveAffect=(pana_glucklich+pana_entspannt+pana_zufrieden+pana_frohlich+pana_enthusiastisch)/5) #mean value
cleandata<-mutate(cleandata,NegativeAffect=(pana_traurig+pana_gereizt+pana_wutend+pana_niedergeschlagen+pana_angstlich)/5) #mean value


##plots for tension
ansp<-ggplot(filter(cleandata, !is.na(anspann_01)),aes(Form_start_time,anspann_01, colour=samplingTime))+geom_point()+geom_line(aes(group=1))+ggtitle('Unangenehme innere Anspannung')+ labs(x='Zeitverlauf',y='Anspannung', colour='Zeitraum')+ylim(0,9)+facet_grid(timepoint~test_day)+ scale_fill_brewer(palette='Dark2', aesthetics = 'colour')+theme_bw()

#plots for experience reactivity
posE<-ggplot(filter(cleandata, !is.na(erleb__gut)),aes(Form_start_time,erleb__gut, colour=samplingTime))+geom_point()+geom_line(aes(group=1))+ggtitle('Reaktivit?t auf positive Ereignisse')+labs(x='Zeitverlauf',y='Intensit?t des Erlebnis', colour='Zeitraum')+ylim(0,7)+facet_grid(timepoint~test_day)+ scale_fill_brewer(palette='Dark2', aesthetics = 'colour')+theme_bw()
negE<-ggplot(filter(cleandata, !is.na(erleb__schlecht)),aes(Form_start_time,erleb__schlecht, colour=samplingTime))+geom_point()+geom_line(aes(group=1))+ggtitle('Reaktivit?t auf negative Ereignisse')+labs(x='Zeitverlauf',y='Intensit?t des Erlebnis', colour='Zeitraum')+ylim(0,7)+facet_grid(timepoint~test_day)+ scale_fill_brewer(palette='Dark2', aesthetics = 'colour')+theme_bw()

#plots for DSS-4
dss<-ggplot(filter(cleandata, !is.na(dss4mean)),aes(Form_start_time,dss4mean, colour=samplingTime))+geom_point()+geom_line(aes(group=1))+ggtitle('Akute dissoziative Symptome') +labs(x='Zeitverlauf',y='Dissoziativer Zustand', colour='Zeitraum')+ylim(0,9)+facet_grid(timepoint~test_day)+ scale_fill_brewer(palette='Dark2', aesthetics = 'colour')+theme_bw()

#plots for PANAS
posA<-ggplot(filter(cleandata, !is.na(PositiveAffect)),aes(Form_start_time,PositiveAffect, colour=samplingTime))+geom_point()+geom_line(aes(group=1))+ggtitle('Momentane positive Gef?hle')+labs(x='Zeitverlauf',y='Intensit?t des positiven Zustands', colour='Zeitraum')+ylim(0,7)+facet_grid(timepoint~test_day)+ scale_fill_brewer(palette='Dark2', aesthetics = 'colour')+theme_bw()
negA<-ggplot(filter(cleandata, !is.na(NegativeAffect)),aes(Form_start_time,NegativeAffect, colour=samplingTime))+geom_point()+geom_line(aes(group=1))+ggtitle('Momentane negative Gef?hle')+labs(x='Zeitverlauf',y='Intensit?t des negativen Zustands', colour='Zeitraum')+ylim(0,7)+facet_grid(timepoint~test_day)+ scale_fill_brewer(palette='Dark2', aesthetics = 'colour')+theme_bw()

#put it all into one graphic
EMAsheet<-ggarrange(ansp, dss, posA, negA, posE, negE, ncol = 1, common.legend = TRUE)

#save the graphics
ggsave(plot = EMAsheet, path=savePath,filename = paste(sub,'.pdf', sep = ''), width = 30, height = 55, units = 'cm')

