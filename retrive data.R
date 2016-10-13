library(readr)
library(rvest)
library(jsonlite)
library(httr)
library(xml2)
library(stringr)

base_url1 <- "http://www.allstays.com/c/target-"
base_url2 <- "-locations.htm"

state_brev = "CA"
state<-state.name[grep(state_brev,state.abb)]
state <- tolower(state)
url <- paste(base_url1,state,base_url2,sep = "")
html<- read_html(url)

txt <-html_nodes(html,".col-md-5") %>% html_text()

all_zips <- as.vector(str_extract_all(txt,paste(state,"[0-9][0-9][0-9][0-9][0-9]")))
to_remove = paste(state_brev,"")
all_zips <- gsub(to_remove,"",unlist(all_zips))



