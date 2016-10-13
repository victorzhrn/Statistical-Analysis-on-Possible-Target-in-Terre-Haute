library(readr)
library(rvest)
library(jsonlite)
library(httr)


income_df = read.csv("input/ACS_14_5YR_B19013_with_ann.csv",skip =1)
colnames(income_df)[4]<-"estimate_median"
colnames(income_df)[5]<-"marginal_error"

income_df$Geography=gsub("ZCTA5 ","" ,income_df$Geography)
income_df$Geography = as.factor(income_df$Geography)
income_df$estimate_median = as.numeric(income_df$estimate_median)



base_url1<-"http://gam.target.com/store-locator/state-result?lnk=statelisting_stateresult&stateCode="
base_url2<-"&stateName="


df_store_info= data.frame()
for(state_abbrev in state.abb){
  
  state<-state.name[grep(state_abbrev,state.abb)]
  url<-paste(base_url1,state_abbrev,base_url2,state,sep= "")
  print(url)
  location_html <- read_html(url)
  json <- html_nodes(location_html,"#primaryJsonResponse") %>% html_text() 
  jlist <-fromJSON(json)
  df <- jlist[2]
  df <- as.data.frame(df)
  df$storeList.zipCode <- gsub("-.*","",df$storeList.zipCode)
  df_summary <- as.data.frame(table(df$storeList.zipCode))
  df_store_info <- rbind(df_store_info,df_summary)
}

save(df_store_info,file = "df_raw.Rda")
write_csv(df_store_info)

names(df_store_info) <- c("ZipCode","#stores")
df_store_info <-as.data.frame(table(df_store_info$ZipCode))
names(df_store_info) <- c("ZipCode","#stores")
df_store_info$ZipCode <- as.factor(df_store_info$ZipCode)

merge(df_store_info,income_df,by=c(ZipCode,Geography))



