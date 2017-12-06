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

#Set up tibble for script storage

#Pull table of contents for scripts
url="https://www.springfieldspringfield.co.uk/episode_scripts.php?tv-show=stranger-things-2016"
download.file(url,destfile="stranger_things.html",quiet=TRUE)
html=read_html("stranger_things.html")

#Find links to episode scripts from table of contents (source-specific code)
hrefs = html %>% 
     html_nodes(".season-episode-title") %>% 
     html_attr("href")

hrefs=paste0("https://www.springfieldspringfield.co.uk/", hrefs)

ep_count <- length(hrefs)

#Loop through episode links, pull and clean scripts, and store data in tibble
scripts = tibble (  episode = 1 : ep_count ,
                    words = rep (" ",ep_count) )

rm(html)

###################################################################################
################################## TRIGRAMS ####################################### 
###################################################################################
'%nin%' = Negate('%in%')

for (i in 1:ep_count) {
     
     download.file ( hrefs[i] , destfile = "stranger_things.html" , quiet = TRUE )
     
     episode_script = read_html("stranger_things.html") %>%
          html_nodes ( ".scrolling-script-container" )  %>%
          html_text 
     
     scripts$words[i] = episode_script
     
     cat ("Scraped Episode",i,"\n")
     
}

rm (episode_script, hrefs, url)

trigrams <- scripts %>%
     unnest_tokens (trigram, words, token = "ngrams" , n=3) %>%
     group_by (episode, trigram) %>%
     summarize ( count = n() ) %>%
     ungroup() %>% as.data.frame

     #count (trigram) %>%
     #arrange(desc(n))
     
save (trigrams, file = "st_trigrams.rda")
