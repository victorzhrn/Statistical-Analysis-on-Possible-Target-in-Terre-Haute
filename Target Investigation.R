library(readr)
library(rvest)
library(jsonlite)
library(httr)
library(ggplot2)


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

groups <- c("higher median income","lower median income")
info <- rbind(store_higher_median,store_lower_median)
df_tem <- data.frame(groups,info)    # create data frame for plot usage


g <- ggplot(df_tem,aes(x="",y=info,fill=groups))+geom_bar(width = 1,stat = "identity") # construct bar plot
g <- g+guides(fill=guide_legend(title="Conditions"))+xlab("")+ylab("")      # set labels
g <- g+coord_polar("y",start=0)              # change to polar coordinate system
g



no_store_higher_median <- sum(df_gen$est_income_median>TH_median & is.na(df_gen$`#stores`))
no_store_lower_median <- sum(df_gen$est_income_median<TH_median & is.na(df_gen$`#stores`))

df_has_store <- df_gen[!is.na(df_gen$`#stores`),]
df_median_store <- as.data.frame(table(df_has_store$est_income_median))
names(df_median_store) <- c("median_income","number_of_store")

df_median_store$median_income <- as.numeric(df_median_store$median_income)

