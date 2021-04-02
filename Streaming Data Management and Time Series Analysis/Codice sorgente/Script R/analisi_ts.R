library(dplyr)
library(ggplot2)
library(ggfortify)
library(forecast)
library(urca)
library(tseries)
library(forecast)
library(tsbox)
library(scales)
library(xts)
library(lubridate)

setwd('D:\\Università\\Magistrale\\Secondo anno\\Streaming Data Management and time series\\Progetto')

source('funzioni.R')

time_series_df<-read.csv('time_series_dataset.csv',sep=';')

index<-as.Date(time_series_df$Data)
time_series_xts<- xts(time_series_df$value, order.by = as.Date(time_series_df$Data))
values<-as.numeric(time_series_xts)


date_init<-c(2010,1,1)

time_series_ts<-ts_ts(time_series_xts)



time_series_df<-time_series_df %>% mutate_if(is.factor, as.character)
time_series_df<- time_series_df %>% mutate(year=year(Data)) %>% mutate(month = month(Data))
years<-unique(time_series_df$year)
info_year<-time_series_df %>% group_by(year) %>% summarise(count=n())


#plot grafico time series completa
plot(ts(time_series_df$value), main="Time series dal 1/1/2010 al 31/12/2018")
autoplot(time_series_xts,main='Prezzi mercato energetico dal 1/1/2010 al 31/12/2018') +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_x_date(date_breaks = "year" , date_labels = "%Y") +
  xlab("Anno") +
  ylab("Prezzo")


#distribuzione valori per anno
list_time_series<-list()
rm(info_year_df)
for (i in 1:length(years))
{
  time_year<-time_series_df %>% filter(year==years[i])
  index<-as.Date(time_year$Data)
  title<-paste('Distribuzione anno',years[i],sep=' ')
  time_series_year_xts<-xts(time_year$value, index)
  
  list_time_series[[i]]<-time_series_year_xts
  info<-c(length(time_series_year_xts), mean(time_series_year_xts), var(time_series_year_xts)[1])
  
    if (!exists('info_year_df'))
    info_year_df<-info
  else
    info_year_df<-rbind(info_year_df,info)
  
  titlevar<-paste0("Var: ",round(info[3],2))
  titlemean<-paste0("Media: ",round(info[2],2))
  title<-paste(title,titlemean,sep=" | ")
  title<-paste(title,titlevar,sep=" - ")
  
  print(autoplot(time_series_year_xts) +
          labs(x='Data',y='Valore') +
          ggtitle(title) +
          theme(plot.title = element_text(hjust=0.5)) +
          scale_x_date(date_breaks = "month" , date_labels = "%b") +
          theme(legend.position = "right") )

}
rownames(info_year)<-years
names(list_time_series)<-years
colnames(info_year_df)<-c("days",'mean','var')


#decomposizione serie temporale - commento: emerge stagionalità annuale
plot(stl(time_series_ts,s.window = 'periodic'))

time_series_log_xts<-log(time_series_xts)

autoplot(time_series_log_xts,main='Distribuzione totale log') +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_x_date(date_breaks = "year" , date_labels = "%Y")

lambda_trasformazione <- BoxCox.lambda(time_series_xts)
time_series_boxcox_xts<- trasforma_boxcox(time_series_xts, lambda_trasformazione)


#trasformazione Box-Cox per anno
rm(time_series_trasf_xts)
rm(var_info_df)
for (i in 1:length(years))
{
  time_series_year<-list_time_series[[as.character(years[i])]]
  trasf_time_series<-trasforma_boxcox(time_series_year, lambda_trasformazione)
  
  trasf_time_series_log<-trasforma_boxcox(time_series_year,0)
  
  trasf_time_series_square<-sqrt(time_series_year)
  
  print(var(trasf_time_series_square))
  print(var(trasf_time_series))
  print(var(trasf_time_series_log))
  
  var_info<-c(var(trasf_time_series),var(trasf_time_series_log), var(trasf_time_series_square))
  if (!exists('var_info_df'))
    var_info_df<-var_info
  else
    var_info_df<-rbind(var_info_df,var_info)
  
  title<-paste("Traformazione Box-cox lambda ottimo anno",years[i],sep=' ')
  
  print(autoplot(trasf_time_series) +
          ggtitle(title) +
          theme(plot.title = element_text(hjust=0.5)))
  
  title<-paste("Traformazione Box-cox log anno",years[i],sep=' ')
  
  print(autoplot(trasf_time_series_log) +
          ggtitle(title) +
          theme(plot.title = element_text(hjust=0.5)))
  
  if (!exists('time_series_trasf_xts'))
    time_series_trasf_xts<-trasf_time_series_log
  else
    time_series_trasf_xts<-rbind(time_series_trasf_xts, trasf_time_series_log)

}
colnames(var_info_df)<-c("lambda_optimal","log","square")
rownames(var_info_df)<-years

var_box_cox<-apply(var_info_df, 2, var)


autoplot(time_series_trasf_xts,main='Distribuzione totale log') +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_x_date(date_breaks = "year" , date_labels = "%Y")


#commento: si seleziona la trasformazione  Box-Cox con varianza minima
time_series_def<-time_series_log_xts

time_series_log_ts<-ts_ts(time_series_log_xts)
summary(ur.df(time_series_log_ts, "drift", lags = 20,"AIC"))


#controllo stazionarietà in media (stagionalità)
plot(stl(time_series_log_ts, s.window = 'periodic'))

#commento: ACF mostra che esiste stagionalità settimanale
acf(main="ACF serie temporale",time_series_xts)
acf(time_series_log_ts)

pacf(main="PACF serie temporale",time_series_xts)

diff_ts<-diff(time_series_log_ts,7)

weekly_time_series_def<-diff(time_series_log_xts,7)
weekly_time_series_def<-weekly_time_series_def[8:length(weekly_time_series_def),]

autoplot(weekly_time_series_def,main='Distribuzione applicando differenza stagionale settimanale') +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_x_date(date_breaks = "year" , date_labels = "%Y")

acf(main="ACF",weekly_time_series_def)
pacf(main="PACF",weekly_time_series_def)


#controllo stazionarietà in media (trend)
plot(stl(diff_ts, s.window='periodic'))

calcola_media_anno(weekly_time_series_def, years)

first_difference_weekly<-diff(weekly_time_series_def,1)
first_difference_weekly<-first_difference_weekly[-1,]

diff_ts<-diff(diff_ts,1)
plot(stl(diff_ts, s.window = 'periodic'))

first_difference<-calcola_media_anno(first_difference_weekly, years)

#test Augmented Dickey Fuller - commento: rigetto ipotesi nulla H0, la serie è stazionaria
summary(ur.df(first_difference_weekly, "drift", lags = 20,"AIC")) 

autoplot(first_difference_weekly,main='Distribuzione applicando 1 differenza semplice') +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_x_date(date_breaks = "year" , date_labels = "%Y")


#salvataggio oggetti ts xts serie temporale
saveRDS(time_series_ts,
        "D:\\Università\\Magistrale\\Secondo anno\\Streaming Data Management and time series\\Progetto\\time_series_ts.RDS")

saveRDS(time_series_xts,
        "D:\\Università\\Magistrale\\Secondo anno\\Streaming Data Management and time series\\Progetto\\time_series_xts.RDS")
