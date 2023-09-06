#07.10.22 FK
#script to check for missing values for Brainboost

#remove "#" below 
#install.packages("tidyverse")
library(tidyverse)



#!! change working directory  !!
#setwd("//zisvfs12/Home/florian.kirn/Desktop/BB_missingdata")
setwd("W:/group_psm/AG-Paret/Projects/BrainBoost/data_analysis/Fragebogen")



#!! change loading directory !!
df_pre <- read.csv("//hobbes/daten/PSM/Brainboost/evasys/BBpre_raw.csv",sep=";",fileEncoding="latin1")
df_pre_exp <- read.csv("//hobbes/daten/PSM/Brainboost/evasys/BBpre_explained.csv",sep=";",fileEncoding="latin1")

df_post <- read.csv("//hobbes/daten/PSM/Brainboost/evasys/BBpost_raw.csv",sep=";",fileEncoding="latin1")
df_post_exp <- read.csv("//hobbes/daten/PSM/Brainboost/evasys/BBpost_explained.csv",sep=";",fileEncoding="latin1")

df_fu <- read.csv("//hobbes/daten/PSM/Brainboost/evasys/BBfu_raw.csv",sep=";",fileEncoding="latin1")
df_fu_exp <- read.csv("//hobbes/daten/PSM/Brainboost/evasys/BBfu_explained.csv",sep=";",fileEncoding="latin1")



#turns "[Bild]" and "/" into NA
df_pre[df_pre=="[BILD]"]<-NA
df_pre[df_pre=="/"]<-NA

df_post[df_post=="[BILD]"]<-NA
df_post[df_post=="/"]<-NA

df_fu[df_fu=="[BILD]"]<-NA
df_fu[df_fu=="/"]<-NA



#drop irrelevant rows
df_pre_exp <- df_pre_exp[-c(82,83),]

df_post_exp <- df_post_exp[-c(77,78,79),]

df_fu_exp <- df_fu_exp[-c(80,81,82),]



#drop irrelevant columns
df_pre_exp <- df_pre_exp %>% select(column,questionaire)

df_post_exp <- df_post_exp %>% select(column,questionaire)

df_fu_exp <- df_fu_exp %>% select(column,questionaire)



#make subject ID unique (in case one document has data from different points in time of the same subject)
df_pre[,2] <- seq(1,length(df_pre[,2])) %>% str_pad(pad=0,width = 2) %>% paste(df_pre[,2],sep = "")

df_post[,2] <- seq(1,length(df_post[,2])) %>% str_pad(pad=0,width = 2) %>% paste(df_post[,2],sep = "")

df_fu[,2] <- seq(1,length(df_fu[,2])) %>% str_pad(pad=0,width = 2) %>% paste(df_fu[,2],sep = "")



  #this function turns the letters excel uses as columns into a number (so A=1; Z=26, AA=27 and so on...)
  letters2numbers <- function(x){
  
  # letters encoding
  encoding <- setNames(seq_along(LETTERS), LETTERS)
  
  # uppercase the letters (just in "case" hahahaha i'm so funny)
  x <- toupper(x)
  
  # convert string to a list of vectors of single letters
  x <- strsplit(x, split = "")
  
  # convert each letter to the corresponding number
  # calculate the column number
  # return a numeric vector
  sapply(x, function(xs) sum(encoding[xs] * 26^((length(xs)-1):0)))
  
}


  
#because there are separators between the values, the following line gets rid of them and creates a dummylist first
dummylist_pre <- lapply(str_extract_all(df_pre_exp$column,"[A-Z]+"),letters2numbers)

dummylist_post <- lapply(str_extract_all(df_post_exp$column,"[A-Z]+"),letters2numbers)

dummylist_fu <- lapply(str_extract_all(df_fu_exp$column,"[A-Z]+"),letters2numbers)



#mind the exceptions where ";" is used in the BBpre file
except_where <- df_pre_exp$column %>% str_detect(";") %>% which()
except_what <- dummylist_pre[df_pre_exp$column %>% str_detect(";") %>% which()]



  #this function turns the numbers into sequences, so "35 40" becomes "35 36 37 38 39 40" 
  numbers2sequences_pre <- function(index){
  
  
  #pull the first element on the level of the index
  from <- dummylist_pre[[index]][1]
  
  #pull the last element of the level of the index
  to <- dummylist_pre[[index]][length(dummylist_pre[[index]])]
  
  #create the sequence
  seq(from, to)
}
  
  numbers2sequences_post <- function(index){
    
    
    #pull the first element on the level of the index
    from <- dummylist_post[[index]][1]
    
    #pull the last element of the level of the index
    to <- dummylist_post[[index]][length(dummylist_post[[index]])]
    
    #create the sequence
    seq(from, to)
  }
  
  numbers2sequences_fu <- function(index){
    
    
    #pull the first element on the level of the index
    from <- dummylist_fu[[index]][1]
    
    #pull the last element of the level of the index
    to <- dummylist_fu[[index]][length(dummylist_fu[[index]])]
    
    #create the sequence
    seq(from, to)
  }
  


#redefine column with numbers now; 
df_pre_exp$column <- seq(1,length(dummylist_pre)) %>% lapply(numbers2sequences_pre)

df_post_exp$column <- seq(1,length(dummylist_post)) %>% lapply(numbers2sequences_post)

df_fu_exp$column <- seq(1,length(dummylist_fu)) %>% lapply(numbers2sequences_fu)



#mind the exceptions where ";" is used in the BBpre file
df_pre_exp$column[except_where] <- except_what



# since some cells contain now multiple values (see example above with 35:40), a nested list is created
# use unnest to stretch it out, so each cell only contains one value
df_pre_exp <- df_pre_exp %>% unnest_longer(column) %>% as.data.frame()

df_post_exp <- df_post_exp %>% unnest_longer(column) %>% as.data.frame()

df_fu_exp <- df_fu_exp %>% unnest_longer(column) %>% as.data.frame()



#create empty dataframe (loop below has full_join function in it; the first element needs some object to be joined to)
df_missing_values_pre <- data.frame(ID=df_pre[,2])

df_missing_values_post <- data.frame(ID=df_post[,2])

df_missing_values_fu <- data.frame(ID=df_fu[,2])



#this loop searches for missing data; looks up in which questionnaire it appears, how much of them there are,
#creates a dataframe and joins them together in one dataframe
for(i in 1:length(df_pre$Bitte.geben.Sie.Ihre.Probanden.ID.an.)) {
  df_missing_values_pre <- 
    df_pre_exp[
      df_pre[i,] %>% 
      is.na() %>% 
      which(),"questionaire"] %>% 
    table(useNA = "always") %>% #this argument exists for technical reasons 
    as.data.frame() %>% 
    pivot_wider(names_from = ".",values_from = "Freq") %>% 
    as.data.frame()%>% 
    cbind(ID=df_pre[i,2]) %>% 
    select(!"NA") %>% #this line exists for technical reasons
    full_join(df_missing_values_pre)
  
  df_missing_values_pre <- df_missing_values_pre[1:i,]
}

for(l in 1:length(df_post$Bitte.geben.Sie.Ihre.Probanden.ID.an.)) {
  df_missing_values_post <- 
    df_post_exp[
      df_post[l,] %>% 
        is.na() %>% 
        which(),"questionaire"] %>% 
    table(useNA = "always") %>% #this argument exists for technical reasons
    as.data.frame() %>% 
    pivot_wider(names_from = ".",values_from = "Freq") %>% 
    as.data.frame()%>% 
    cbind(ID=df_post[l,2]) %>% 
    select(!"NA") %>% #this line exists for technical reasons
    full_join(df_missing_values_post)
  
  df_missing_values_post <- df_missing_values_post[1:l,]
}

for(m in 1:length(df_fu$Bitte.geben.Sie.Ihre.Probanden.ID.an.)) {
  df_missing_values_fu <- 
    df_fu_exp[
      df_fu[m,] %>% 
        is.na() %>% 
        which(),"questionaire"] %>% 
    table(useNA = "always") %>% #this argument exists for technical reasons 
    as.data.frame() %>% 
    pivot_wider(names_from = ".",values_from = "Freq") %>% 
    as.data.frame()%>% 
    cbind(ID=df_fu[m,2]) %>% 
    select(!"NA") %>% #this line exists for technical reasons
    full_join(df_missing_values_fu)
  
  df_missing_values_fu <- df_missing_values_fu[1:m,]
}



#change column order as they appear in questionnaire
df_missing_values_pre <- df_missing_values_pre %>% 
  relocate(ID,intersect(df_pre_exp[,2],colnames(df_missing_values_pre)))

df_missing_values_post <- df_missing_values_post %>% 
  relocate(ID,intersect(df_post_exp[,2],colnames(df_missing_values_post)))

df_missing_values_fu <- df_missing_values_fu %>% 
  relocate(ID,intersect(df_fu_exp[,2],colnames(df_missing_values_fu)))



#arrange ascending and revert ID names back
df_missing_values_pre <- df_missing_values_pre %>% 
  arrange(as.numeric(substr(df_missing_values_pre$ID,start = 1, stop = 2)))
df_missing_values_pre$ID <- substr(df_missing_values_pre$ID,start = 3, stop = length(df_missing_values_pre$ID))

df_missing_values_post <- df_missing_values_post %>% 
  arrange(as.numeric(substr(df_missing_values_post$ID,start = 1, stop = 2)))
df_missing_values_post$ID <- substr(df_missing_values_post$ID,start = 3, stop = length(df_missing_values_post$ID))

df_missing_values_fu <- df_missing_values_fu %>% 
  arrange(as.numeric(substr(df_missing_values_fu$ID,start = 1, stop = 2)))
df_missing_values_fu$ID <- substr(df_missing_values_fu$ID,start = 3, stop = length(df_missing_values_fu$ID))



#add timestamps
df_missing_values_pre <- df_missing_values_pre %>% cbind(Date=df_pre$Zeitstempel)

df_missing_values_post <- df_missing_values_post %>% cbind(Date=df_post$Zeitstempel)

df_missing_values_fu <- df_missing_values_fu %>% cbind(Date=df_fu$Zeitstempel)



#export
write.csv(df_missing_values_pre,"W:/group_psm/AG-Paret/Projects/BrainBoost/data_analysis/Fragebogen/missing_values_pre.csv",row.names = FALSE)
write.csv(df_missing_values_post,"W:/group_psm/AG-Paret/Projects/BrainBoost/data_analysis/Fragebogen/missing_values_post.csv",row.names = FALSE)
write.csv(df_missing_values_fu,"W:/group_psm/AG-Paret/Projects/BrainBoost/data_analysis/Fragebogen/missing_values_fu.csv",row.names = FALSE)