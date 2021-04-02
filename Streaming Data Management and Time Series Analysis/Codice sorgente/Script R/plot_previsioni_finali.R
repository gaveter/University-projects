library(dplyr)
library(ggplot2)
library(reshape2)
library(caret)
library(ggfortify)
library(forecast)
library(urca)
library(tsbox)
library(tseries)
library(forecast)
library(scales)
library(xts)
library(lubridate)

setwd('D:\\Università\\Magistrale\\Secondo anno\\Streaming Data Management and time series\\Progetto')

prev_finali<-readRDS('df_previsioni_finali.RDS')

#prev_rnn<-read.csv('pred_finale_rnn.csv')
prev_rnn<-read.csv('pred_finale_rnn_1passo.csv')

prev_rnn<-prev_rnn %>% select(-X) %>% mutate(Data=as.Date(Data))

prev_complete<-merge(prev_finali,prev_rnn,by='Data')

colnames(prev_complete)[ncol(prev_complete)]<-"RNN"


testo<-paste0("Previsione dal ",prev_complete[1,"Data"])
testo<-paste0(testo," al ")
testo<-paste0(testo,prev_complete[nrow(prev_complete),"Data"])


df_plot<-melt(prev_complete, id.vars='Data')
colnames(df_plot)[2]<-"Legenda"

ggplot(df_plot, aes(x=Data,y=value,colour=Legenda)) +
  ggtitle(testo) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%B") +
  xlab("Data") +
  ylab("Prezzo") +
  geom_line()


colnames(prev_complete)[ncol(prev_complete)]<-"ML"
write.csv(prev_complete,"SDMTSA_808101_1.csv")

