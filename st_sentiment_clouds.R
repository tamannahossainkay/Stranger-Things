
##############################
####### Scrape Scripts #######
##############################

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


#Pull table of contents for scripts
url="https://www.springfieldspringfield.co.uk/episode_scripts.php?tv-show=stranger-things-2016"
download.file(url,destfile="stranger_things.html",quiet=TRUE)
html=read_html("stranger_things.html")

hrefs = html %>% 
            html_nodes(".season-episode-title") %>% 
            html_attr("href")

hrefs=paste0("https://www.springfieldspringfield.co.uk/",hrefs)

ep_count <- length(hrefs)

#Loop through episode links, pull and clean scripts, and store data in tibble
scripts = tibble (  episode = 1 : ep_count ,
                    words = rep (" ",ep_count) )

rm(html)
##########################################################################
####### Get overall episode means, and create episode trajectories #######
##########################################################################

'%nin%' = Negate('%in%')

for (i in 1:ep_count) {
  
  download.file ( hrefs[i] , destfile = "stranger_things.html" , quiet = TRUE )
  
  episode_script = read_html("stranger_things.html") %>%
                    html_nodes ( ".scrolling-script-container" )  %>%
                    html_text %>%
                    gsub ("[\r\n]+?|\r|\n|\t", "" , .) %>%
                    gsub ( "[[:punct:]]|[[:digit:]]" , "" , . ) %>%  # remove digits and punctuations
                    gsub ( "\\s{2,}" , " ", . ) %>% #reduce repeating spaces to single space
                    gsub ("^\\s|\\s$" , "", .) %>% #remove leading and trailing spaces
                    tolower %>%
                    strsplit (" ") %>%#Parse into words
                    unlist %>% . [. %nin% stopwords("en") ] %>% list
                                        

  scripts$words[i] = episode_script
  
  cat ("Scraped Episode",i,"\n")

}

rm (episode_script, hrefs, url)

####### Put in dataframe ###########
scripts_df = NULL

for (i in 1:ep_count) {
  
ep_words <- scripts$words[i][[1]]  
  
ep_df <-   data.frame ( episode = i ,
                        word = ep_words)
  
scripts_df <- rbind( scripts_df , ep_df)

}

rm(ep_df, ep_words, scripts)

scripts_df <- scripts_df %>% left_join ( get_sentiments ("afinn") ) 

write.csv ( scripts_df , file = "st_scripts.csv" )
save(scripts_df, file = "st_scripts.rda" )

###################################################################
################## WORD CLOUD GENERATION ##########################
###################################################################

wordcloud <- NULL


for (j in 1 : ep_count) {
     
dm <- scripts_df %>% 
          filter (episode == j) %>%
          group_by (word) %>% 
               summarize (freq = n() )     
     
file_name = paste0("ep", j, "_wordcloud.svg" )

words_df <- wordcloud.coords(dm, file_name)     
     


#############################################################################
############ WORDCLOUD TO SVG ::: BY WORD ###################################
#############################################################################

for ( i in 1:nrow(words_df) )   {
 
  svg("word.svg" )
  plot.new()
  op <- par("mar")
  par(mar = c(0, 0, 0, 0))
  plot.window(c(0, 1), c(0, 1), asp = 1)
  text(words_df$x[i], words_df$y[i], words_df$word[i], cex = words_df$size[i],    offset = 0, srt = words_df$rotWord[i] * 90)
  dev.off()  
  
  parsed_svg <- parse.svg("word.svg") 
  parsed_svg$word <- words_df$word[i]
  
  parsed_svg$episode <- j
  
  
  wordcloud <- rbind (wordcloud, parsed_svg) %>% filter ( !is.na(poly_id) )
  

}


}

#sentiment Scores

wordcloud %<>% left_join ( get_sentiments ("afinn") ) 



###############################################################
################## Word Frequencies ##########################
##############################################################
#load("st_wordcloud.rda")

word_freqs <- NULL

for (j in 1 : ep_count) {
     
     dm <- scripts_df %>% 
          filter (episode == j) %>%
          group_by (word) %>% 
          summarize (freq = n() )     %>% ungroup()
     
     dm$episode = j
     
     word_freqs <- rbind (word_freqs,dm )
     
}


wordcloud %<>% left_join( word_freqs, by = c("episode", "word"))
wordcloud %<>% as.data.frame


save(wordcloud, file = "st_wordcloud.rda")
#write.csv ( wordcloud , file = "st_wordclouds.csv" )



