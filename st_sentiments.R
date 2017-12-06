
# data munging
library(tibble)
library(dplyr)
library(magrittr)
library(reshape2)

library(rvest) #web parsing
library (XML) # web parsing
library(tidytext) # sentiments
library(tm) # has stopwords
setwd("C:/Users/taman/Documents/Stranger Things")

load("st_scripts.rda")

means= NULL
epvecs= matrix(nrow=17,ncol=500) %>% as.data.frame()
colnames(epvecs) = 1:500

#epvecs = NULL
for(i in 1:17){
     
     thisep = scripts_df %>% filter (episode ==i) %>% filter (!is.na(score))
     
     vec=NULL
     for(j in 21:(nrow(thisep)-20)){
          vec=c(vec,mean(thisep$score[(j-20):(j+20)]))
     }
     means=c(means,mean(thisep$score))
     #means[i] = mean (thisep$score)
     vec=approx(vec,n=500)$y
     epvecs[i,]=vec
     #epvecs = rbind(vec, epvecs)

     
}

means %<>% as.data.frame %>% tibble::rowid_to_column("episode")

## Reshape Data for Tableau
epvecs %<>% tibble::rowid_to_column("episode") %>%
               melt(id = c("episode"))  %>%
               #left_join (clusters, by = "episode")
               left_join ( means, by = "episode")
     
#colnames(epvecs) = c("episode", "time", "sentiment", "cluster")
colnames(epvecs) = c("episode", "time", "sentiment", "mean")

save(epvecs, file = "st_sliding_window_sentiments.rda")