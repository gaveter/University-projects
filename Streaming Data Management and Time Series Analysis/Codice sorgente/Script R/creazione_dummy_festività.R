library(KFAS)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(caret)
library(stringr)
library(ggfortify)
library(forecast)
library(urca)
library(tseries)
library(forecast)
library(scales)
library(timeDate)
library(xts)
library(lubridate)

setwd('D:\\Università\\Magistrale\\Secondo anno\\Streaming Data Management and time series\\Progetto')

source('funzioni.R')

time_series_xts<-readRDS('time_series_xts.RDS')

#creazione dataframe
hol_dummy_df<-data.frame(Date=index(time_series_xts),
                     Easter=0,EasterMonday=0,ChristmasDay=0,
                     BoxingDay=0,ChristmasEve=0,NewYearsDay=0,
                     Epiphany=0, PentecostMonday=0)

seq_year<-seq(year(index(time_series_xts[1])), 
              year(index(time_series_xts[length(time_series_xts)])),1)

holiday<-c("Easter","EasterMonday","ChristmasDay",
           "BoxingDay","ChristmasEve","NewYearsDay",
           "Epiphany","PentecostMonday")

df_holiday<-data.frame(seq_year)

date_hol<-t(apply(df_holiday,1,ritorna_giorni_vacanza,arg1=holiday))
colnames(date_hol)<-holiday

df_holiday<-cbind(df_holiday, date_hol)

holiday_list<-as.list(df_holiday)

hol_dummy_df<- hol_dummy_df %>% mutate(Easter=ifelse(as.character(Date) %in% as.character(holiday_list$Easter),1,0)) %>%
                        mutate(EasterMonday=ifelse(as.character(Date) %in% as.character(holiday_list$EasterMonday),1,0)) %>%
                          mutate(ChristmasDay=ifelse(as.character(Date) %in% as.character(holiday_list$ChristmasDay),1,0)) %>%
                            mutate(BoxingDay=ifelse(as.character(Date) %in% as.character(holiday_list$BoxingDay),1,0)) %>%
                              mutate(ChristmasEve=ifelse(as.character(Date) %in% as.character(holiday_list$ChristmasEve),1,0)) %>%
                                mutate(NewYearsDay=ifelse(as.character(Date) %in% as.character(holiday_list$NewYearsDay),1,0)) %>%
                                  mutate(Epiphany=ifelse(as.character(Date) %in% as.character(holiday_list$Epiphany),1,0)) %>%
                                    mutate(PentecostMonday=ifelse(as.character(Date) %in% as.character(holiday_list$PentecostMonday),1,0))

hol_dummy_df <- hol_dummy_df[,-ncol(hol_dummy_df)]

saveRDS(hol_dummy_df,file = "holiday_dummy_df.RDS")
