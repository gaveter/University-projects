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

time_series_xts<-readRDS('time_series_xts.RDS')

time_series_ts<-readRDS('time_series_ts.RDS')

source('funzioni.R')

#plot(stl(time_series_ts,s.window = 'periodic')) #emerge stagionalità annuale


#creazione range train, test e validation
train_range<-seq(as.Date("2010-01-01"),as.Date("2016-12-31"),"day")
validation_range<-seq(as.Date("2017-01-01"), as.Date("2017-12-31"),"day")
test_range<-seq(as.Date("2018-01-01"), as.Date("2018-11-30"),"day")

#lettura dataframe dummy festività
dummy_hol_df<-readRDS('holiday_dummy_df.RDS')

train_xts<-time_series_xts[train_range]
test_xts<-time_series_xts[test_range]


#modello iniziale
mod1<-Arima(train_xts, c(0,1,0), list(order = c(1,1,1), period=7), lambda = 0) 
#commento: lambda=0 perchè si sceglie trasf.log

acf(main="ACF residui ottenuti da ARIMA(0,1,0)(1,1,1)[7]",mod1$residuals)
pacf(main="Pacf residui ottenuti da ARIMA(0,1,0)(1,1,1)[7]",mod1$residuals)

pre_mod1<-forecast(mod1,h=length(test_xts),biasadj = TRUE)$mean



#commento: osservando i plot non si capisce se esiste sia una componente AR che MA oppure solo uno di questi
          # perchè da PACF i valori sembrano ad avvicinarsi a 0 a velocità geometrica però c'è andamento crescente
          # da lag4 a lag5 mentre osservando ACF sembra esserci velocità geometrica però tra il primo
          # e il secondo lag c'è un "cut off" importante. Per questo motivo si testano n modelli ARMA con ordine
          # p  da 1 a 6 e q da 1 a 6 e tra questi si prende il modello che sui dati di validation risulta avere MAPE più basso

validation_xts<-time_series_xts[validation_range]

res_mod<-genera_modello_ARMA_migliore(train_xts, validation_xts)
res_mod<-as.data.frame(res_mod) %>% arrange(`MAPE`)


mod2<-Arima(train_xts, c(res_mod[1,"AR"],1,res_mod[1,"MA"]), 
               list(order=c(1,1,1), period=7), lambda = 0)

arima_mod<-paste0("ARIMA(",res_mod[1,"AR"])
arima_mod<-paste0(arima_mod,",1,")
arima_mod<-paste0(arima_mod,res_mod[1,"MA"])
arima_mod<-paste0(arima_mod,")(1,1,1)[7]")

testo<-paste0("residui ottenuti da ARIMA(",res_mod[1,"AR"])
testo<-paste0(testo,",1,")
testo<-paste0(testo,res_mod[1,"MA"])
testo<-paste0(testo,")")
testo<-paste0(testo,"(1,1,1)[7]")
testo_ACF<-paste("ACF",testo," ")
testo_PACF<-paste("PACF",testo," ")

acf(main=testo_ACF,mod2$residuals)
pacf(main=testo_PACF,mod2$residuals)

train_agg<-rbind(train_xts,validation_xts)

arima_mod_1<-Arima(validation_xts,model=mod1)
arima_mod_2<-Arima(validation_xts,model=mod2)

pre_mod_1<-forecast(arima_mod_1,h=length(test_xts),biasadj = TRUE)
pre_mod_2<-forecast(arima_mod_2,h=length(test_xts),biasadj = TRUE)

autoplot(pre_mod_1)
autoplot(pre_mod_2)

values_pre_mod_1<-pre_mod_1$mean
values_pre_mod_2<-pre_mod_2$mean

test_pre_df<-fortify.zoo(test_xts)
columns_name<-c("Data","reale",arima_mod,"ARIMA(0,1,0)(1,1,1)[7]")

ris_df<-confronta_previsioni(test_pre_df,values_pre_mod_1,values_pre_mod_2,columns_name)

genera_grafico(ris_df,test_xts)

rm(performance_df)
performance_df<-calcola_performance(ris_df,columns_name[3:4])



#si inizia la fase di refine
#per migliorare le previsione modello ARIMA(5,1,5)(1,1,1)[7] migliore aggiungo regressore che considera stag. annuale
period<-365.25

rm(result_sin_df)
for (i in 2:5)
{
  rm(test_pre_df)
  print(i)
  reg_365<-crea_matrice_sinusoidi(time_series_xts,i)
  
  mod2_refine<-Arima(train_xts, c(res_mod[1,"AR"],1,res_mod[1,"MA"]), list(order=c(1,1,1), period=7), lambda = 0,
                xreg = reg_365[1:length(train_xts),])

  pre_mod<-forecast(mod2_refine,biasadj = TRUE,
                      xreg = reg_365[(length(train_xts)+1):nrow(reg_365),],
                      h = length(validation_xts))

  values_pre_mod_def<-pre_mod$mean[1:length(validation_xts)]

  test_pre_df<-fortify.zoo(validation_xts)
  test_pre_df<-cbind(test_pre_df, values_pre_mod_def)
  colnames(test_pre_df)<-c("index","real","pred")
  
  mape<-calcola_MAPE(test_pre_df[,-1])
  
  perf1<-RMSE(test_pre_df$pred, test_pre_df$real)
  perf2<-MAE(test_pre_df$pred, test_pre_df$real)
  
  #print(perf1)
  
  if (!exists("result_sin_df"))
    result_sin_df<-c(i, perf1, perf2,mape)
  else
    result_sin_df<-rbind(result_sin_df, c(i, perf1, perf2,mape))
}

result_sin_df<-as.data.frame(result_sin_df)
colnames(result_sin_df)<-c("#Sin","RMSE","MAE","MAPE")
rownames(result_sin_df)<-1:nrow(result_sin_df)

result_sin_df<- result_sin_df %>% arrange(MAPE)
#commento: eseguendo il ciclo si ottiene come numero ottimo sinusoidi 26

reg_365<-crea_matrice_sinusoidi(time_series_xts,26)

mod2_refine<-Arima(train_xts, c(res_mod[1,"AR"],1,res_mod[1,"MA"]), list(order=c(1,1,1), period=7), lambda = 0,
                   xreg = reg_365[1:length(train_xts),])

arima_refine<-Arima(validation_xts,model=mod2_refine,
                    xreg = reg_365[((length(train_xts)+1):(length(train_xts)+365)),])

pre_mod_def_reg<-forecast(arima_refine,h=length(test_xts),biasadj = TRUE,
                      xreg = reg_365[(length(train_xts)+366):(nrow(reg_365)-31),])

autoplot(pre_mod_def_reg)
values_pre_mod_def_reg<-pre_mod_def_reg$mean

rm(test_pre_df)
test_pre_df<-fortify.zoo(test_xts)

arima_mod_reg<-paste(arima_mod,"+ reg(stag.)"," ")

test_pre_df<-cbind(test_pre_df,as.numeric(values_pre_mod_1),
                   as.numeric(values_pre_mod_2),as.numeric(values_pre_mod_def_reg))

colnames(test_pre_df)<-c("Data","reale","ARIMA(0,1,0)(1,1,1)[7]",
                         arima_mod,arima_mod_reg)

models<-colnames(test_pre_df)[3:ncol(test_pre_df)]
genera_grafico(test_pre_df[,-3],test_xts)

rm(performance_df)
performance_df<-calcola_performance(test_pre_df, models)

acf(main="ACF residui ottenuti da ARIMA(5,1,4)(1,1,1)[7] + reg (stag.)",mod2_refine$residuals)

checkresiduals(mod2_refine)


#si prova ad aggiungere al modello ARIMA(5,1,5)(1,1,1)[7] dummy che considerano le festività
dummy_hol_matrix<-as.matrix(sapply(dummy_hol_df, as.numeric))  
dummy_hol_matrix_prova<-dummy_hol_matrix[,3]

holiday<-rep(0,nrow(reg_365))

for (i in 1:nrow(reg_365))
{
  row<-dummy_hol_matrix[i,2:ncol(dummy_hol_matrix)]
  if (1 %in% row)
    holiday[i]<-1
}

mod2_refine_festiv<-Arima(train_xts, c(res_mod[1,"AR"],1,res_mod[1,"MA"]), list(order=c(1,1,1), period=7), lambda = 0,
                   xreg = cbind(reg_365[1:length(train_xts),],
                                holiday[1:length(train_xts)]))


arima_refine_festiv<-Arima(validation_xts,model=mod2_refine_festiv,
                    xreg = cbind(reg_365[(length(train_xts)+1):(length(train_xts)+365),],
                    holiday[(length(train_xts)+1):(length(train_xts)+365)]))

pre_mod_def_reg_festiv<-forecast(arima_refine_festiv,h=length(test_xts),biasadj = TRUE,
                          xreg = cbind(reg_365[(length(train_xts)+366):(nrow(reg_365)-31),],
                          holiday[(length(train_xts)+366):(nrow(reg_365)-31)]))


autoplot(pre_mod_def_reg_festiv)
values_pre_mod_def_reg_festiv<-pre_mod_def_reg_festiv$mean


rm(test_pre_df)
test_pre_df<-fortify.zoo(test_xts)

arima_mod_reg<-paste(arima_mod,"+ reg (stag.)"," ")
arima_mod_reg_festiv<-paste(arima_mod," + reg (stag.) + festiv"," ")

test_pre_df<-cbind(test_pre_df,as.numeric(values_pre_mod_1),
                   as.numeric(values_pre_mod_2),
                   as.numeric(values_pre_mod_def_reg),
                   as.numeric(values_pre_mod_def_reg_festiv))


colnames(test_pre_df)<-c("Data","reale","ARIMA(0,1,0)(1,1,1)[7]",
                         arima_mod,arima_mod_reg,arima_mod_reg_festiv)

models<-colnames(test_pre_df)[3:ncol(test_pre_df)]

genera_grafico(test_pre_df[,-c(3,4,6)],test_xts)

saveRDS(test_pre_df,"df_previsioni_ARIMA_test.RDS")

rm(performance_df)
performance_df<-calcola_performance(test_pre_df, models)



#confronto MAPE train MAPE test modello migliore
arima_refine_festiv_train<-Arima(train_xts,model=mod2_refine,
                                 xreg=reg_365[1:length(train_xts),])

perf_train<-accuracy(arima_refine_festiv_train)

perf_mod_migliore<-t(as.data.frame(c(perf_train[1,"MAPE"],performance_df[3,"MAPE"])))
rownames(perf_mod_migliore)<-"MAPE"
colnames(perf_mod_migliore)<-c("Training","Test")


#previsione finale modello migliore 2019-01-01/30-11-2019
prev_index<-seq(as.Date("2019-01-01"), as.Date("2019-11-30"),by='days')

reg_365<-crea_matrice_sinusoidi(time_series_xts,26)

mod2_def<-Arima(time_series_xts, c(res_mod[1,"AR"],1,res_mod[1,"MA"]), list(order=c(1,1,1), period=7), lambda = 0,
                   xreg = reg_365[1:length(time_series_xts),])

pre_mod_def<-forecast(mod2_def,h=length(prev_index),biasadj = TRUE,
                      xreg = reg_365[1:length(time_series_xts),])$mean

pre_def_xts<-xts(x = pre_mod_def[1:334], order.by = prev_index)

pre_def_model_df<-data.frame(index(pre_def_xts))
rownames(pre_def_model_df)<-1:nrow(pre_def_model_df)
colnames(pre_def_model_df)[1]<-"Data"
pre_def_model_df$ARIMA<-as.numeric(pre_def_xts)
saveRDS(pre_def_model_df,file="df_previsioni_finali.RDS")

autoplot(main="Previsione finale h=334",pre_def_xts)
