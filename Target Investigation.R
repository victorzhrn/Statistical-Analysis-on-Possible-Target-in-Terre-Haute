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

df_income <- income_df[c("Geography","estimate_median")]
names(df_income) <- c("ZipCode","est_income_median")

load("target_info.Rda")


df_gen <- merge(df_income,df_stores,by="ZipCode",all = TRUE)

TH_median <- df_gen$est_income_median[df_gen$ZipCode==47803]

store_higher_median <- sum(df_gen$est_income_median>TH_median & df_gen$`#stores`>0,na.rm=TRUE)
store_lower_median <- sum(df_gen$est_income_median<TH_median & df_gen$`#stores`>0,na.rm=TRUE)


no_store_higher_median <- sum(df_gen$est_income_median>TH_median & is.na(df_gen$`#stores`))
no_store_lower_median <- sum(df_gen$est_income_median<TH_median & is.na(df_gen$`#stores`))

