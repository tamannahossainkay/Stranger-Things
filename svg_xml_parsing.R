
library(rvest)
library(XML)
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(stringi)

parse.svg <- function (file_name) {

file_name = "word.svg"
  
# read in vectorized image
svg <- read_html(file_name) 

#extract path nodes, id, and coordinate lists
path_nodes <- svg %>% html_nodes("path")
# ID -- > when id is part of the path node
#ids <- paths %>% xml_attr("id")  


#But from R svg output, we have it as part of the symbol node
ids <- svg %>% html_nodes("symbol") %>%
                html_attr("id") 


paths <- path_nodes %>% xml_attr ( "d" ) %>% 
                         gsub ( "^M", "",. ) %>%
                         stri_replace_last_fixed ( "M", "" ) %>% 
                         str_split_fixed("M",2) %>%
                         cbind(ids) %>% as.tibble() 


colnames(paths) = c("path1", "path2", "id")

paths$path1 %<>%          gsub ("[A-Z]", "", .) %>%
                          gsub ("\\s{2,}", " ", .) %>%
                          gsub ( "^\\s|\\s$", "", . )  %>%
                          strsplit (" ") %>%
                          lapply(as.numeric)

paths$path2 %<>%          gsub ("[A-Z]", "", .) %>%
     gsub ("\\s{2,}", " ", .) %>%
     gsub ( "^\\s|\\s$", "", . )  %>%
     strsplit (" ") %>%
     lapply(as.numeric)



rel_coords = NULL

for (i in 1:nrow(paths) ) {
     
     
rel_xy1 <- paths$path1[i]  %>% 
           unlist %>%
           matrix ( ncol=2, byrow = TRUE ) 

path_tbl1 <- tibble ( sym_id = paths$id[i] ,
                     path_id = 1:nrow(rel_xy1)  ,
                     rel_x = rel_xy1[,1] ,
                     rel_y = rel_xy1[,2] ,
                     split_flag = 1
                    )

rel_xy2 <- paths$path2[i] %>% 
     unlist %>%
     matrix ( ncol=2, byrow = TRUE ) 




     if (nrow(rel_xy2) >0) {
          
          path_tbl2 <- tibble ( sym_id = paths$id[i] ,
                                path_id = 1:nrow(rel_xy2)  ,
                                rel_x = rel_xy2[,1] ,
                                rel_y = rel_xy2[,2] ,
                                split_flag = 2
          )
          
          path_tbl <- rbind ( path_tbl1, path_tbl2)
          
     }     else path_tbl <- path_tbl1


rel_coords <- rbind(rel_coords, path_tbl)
     
}


rel_coords
########### Find starting points ####################

st_coords <- tibble (

 sym_id = svg %>% html_nodes("use") %>%
                html_attr ( "xlink:href") %>%
                gsub ("#", "", .) 

, st_x = svg %>% html_nodes("use") %>%
  html_attr ( "x") %>% 
  as.numeric

, st_y = svg %>% html_nodes("use") %>%
            html_attr ( "y") %>% 
              as.numeric 

, st_coord_id = 1:length ( svg %>% html_nodes("use") %>% html_attr ( "xlink:href"))



) 



################# Parsed SVG with absolute coordiantes #################

parsed_svg <- st_coords %>% 
                left_join (rel_coords , by = "sym_id") %>%
                mutate (x = (rel_x + st_x)
                        , y = (rel_y + st_y) 
                        
                        )

         parsed_svg$poly_id    <- group_indices_(parsed_svg, .dots=c("st_coord_id", "split_flag"))
  
         
         

return (parsed_svg)

}

#write.csv ( parsed_svg , file = "test_coords.csv" )