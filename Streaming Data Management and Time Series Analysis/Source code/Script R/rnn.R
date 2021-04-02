library(dplyr)
library(ggplot2)
library(reshape2)
library(caret)
library(tfruns)
library(ggfortify)
library(forecast)
library(urca)
library(car)
library(tseries)
library(forecast)
library(scales)
library(timeDate)
library(xts)
library(lubridate)
library(e1071)
library(keras)


setwd('D:\\Università\\Magistrale\\Secondo anno\\Streaming Data Management and time series\\Progetto')

source('funzioni.R')

time_series_xts<-readRDS('time_series_xts.RDS')
train_xts<-time_series_xts["2010-01-01/2016-12-31"]
validation_xts<-time_series_xts["2017-01-01/2017-12-31"]
test_xts<-time_series_xts["2018-01-01/2018-11-30"]


#ispezione normalità serie storica
qqPlot(time_series_xts)

shapiro.test(as.numeric(time_series_xts))

range_year<-unique(year(index(time_series_xts)))

rm(range_year_df)
for (i in 1:length(range_year))
{
  anno_xts<-time_series_xts[as.character(range_year[i])]
  if (!exists("range_year_df"))
    range_year_df<-c(range_year[i],min(anno_xts),max(anno_xts))
  else
    range_year_df<-rbind(range_year_df, c(range_year[i],min(anno_xts),max(anno_xts)))
}

rownames(range_year_df)<-1:nrow(range_year_df)
colnames(range_year_df)<-c("anno","min","max")


#preprocessing
media_train<-mean(train_xts)
sd_train<-sd(train_xts)

time_series_norm_xts<-(time_series_xts- media_train) / sd_train




df_prev_model<-readRDS("df_previsioni_finali.RDS")
dummy_hol_df<-readRDS("holiday_dummy_df.RDS")


train_norm_xts<-time_series_norm_xts["2010-01-01/2016-12-31"]
validation_norm_xts<-time_series_norm_xts["2017-01-01/2017-12-31"]
test_norm_xts<-time_series_norm_xts["2018-01-01/2018-11-30"]

test_val_norm_xts<-rbind(validation_norm_xts,test_norm_xts)

history_seq<-seq(30,360,30)

history_seq<-c(history_seq,365,730)

#LSTM con previsione diretta 334 giorni
set.seed(2020)

future<-334
feature<-1

rm(ris_history_diretta_df)

for (i in 1:length(history_seq))
{
  #validation_norm_xts<-rbind(train_agg,validation_norm_xts)
  history_ts<-history_seq[i]
  train_matrix_multistep<-crea_matrice_multistep(train_norm_xts,history_ts,future)
  
  x_train_matrix_multistep<-train_matrix_multistep[,c(1:history_ts)]
  y_train_matrix_multistep<-train_matrix_multistep[,c((history_ts+1):ncol(train_matrix_multistep))]
  
  
  x_train_multistep_tensor<- array(unlist(x_train_matrix_multistep), 
                                   dim=c(nrow(x_train_matrix_multistep), 
                                         ncol(x_train_matrix_multistep), 1))
  y_train_multistep_tensor<-array(unlist(y_train_matrix_multistep), 
                                  dim=c(nrow(y_train_matrix_multistep),
                                        ncol(y_train_matrix_multistep),
                                        1))
  
  train_agg<-train_norm_xts[(length(train_norm_xts)-(history_ts-1)):length(train_norm_xts)]
  validation_norm_agg_xts<-rbind(train_agg,validation_norm_xts)
  
  validation_matrix_multistep<-crea_matrice_multistep(validation_norm_agg_xts,history_ts,future)
  
  x_validation_multistep<-validation_matrix_multistep[,c(1:history_ts)]
  y_validation_multistep<-validation_matrix_multistep[, c((history_ts+1):ncol(validation_matrix_multistep))]
  
  
  x_validation_multistep_tensor<-array(unlist(x_validation_multistep), 
                             dim=c(nrow(x_validation_multistep), ncol(x_validation_multistep),1))
  
  y_validation_multistep_tensor<-array(unlist(y_validation_multistep), 
                                       dim=c(nrow(y_validation_multistep),
                                             ncol(y_validation_multistep),
                                             1))
  if (history_ts>365)
  {
    train_agg<-train_norm_xts[(length(train_norm_xts)-(365-1)):length(train_norm_xts)]
    val_agg<-validation_norm_xts[(length(validation_norm_xts)-(365-1)):length(validation_norm_xts)]
    val_agg<-rbind(train_agg,val_agg)
  }else
    val_agg<-validation_norm_xts[(length(validation_norm_xts)-(history_ts-1)):length(validation_norm_xts)]
  
  test_norm_agg_xts<-rbind(val_agg,test_norm_xts)
  test_matrix_multistep<-crea_matrice_multistep(test_norm_agg_xts,history_ts,future)
  
  x_test_multistep_pred<-test_matrix_multistep[,c(1:history_ts)]
  y_test_multistep<-test_matrix_multistep[, c((history_ts+1):ncol(test_matrix_multistep))]
  
  
  x_test_multistep_tensor<-array(unlist(x_test_multistep_pred), 
                                        dim=c(1, history_ts,1))
  
  y_test_multistep_tensor<-array(unlist(y_test_multistep),
                                       dim=c(1,334,1))
  
  #LSTM
  model_lstm<-keras_model_sequential()
  model_lstm %>%
    layer_lstm(units = 100,activation="tanh", 
               input_shape = c(history_ts, feature)) %>%
    layer_dense(units=future)
  
  model_lstm %>% compile(
    loss = "mean_squared_error",
    optimizer = optimizer_rmsprop(lr=0.0001)
  )
  
  print(history_ts)
  
  history <- model_lstm %>% fit(
    x_train_multistep_tensor, y_train_multistep_tensor,
    epochs = 50, batch_size = round(dim(x_train_multistep_tensor)[1]/2,0),
    validation_data=list(x_validation_multistep_tensor,y_validation_multistep_tensor)
    
  )
  
  #multi-step forecast diretta
  
  pred<-predict(model_lstm,x_test_multistep_tensor)
  
  pred_val_reale<-sapply(pred,converti_dato_originale,media_train,sd_train)
  
  testo<-paste0("Orizzonte temporale ",history_ts)
  testo<-paste0(testo," giorni e forecast 334 giorni")
  
  plot(main=testo,as.numeric(test_xts)[1:future],type='lines') +
    lines(pred_val_reale,col='blue')
  
  
  ris_df<-fortify.zoo(test_xts[1:future])
  ris_df<-cbind(ris_df,pred_val_reale)
  colnames(ris_df)<-c("Data","reale","LSTM 1 layer")
  ris_df<-ris_df[,-1]
  
  mape<-calcola_MAPE(ris_df)
  
  if (!exists('ris_history_diretta_df'))
  {
    ris_history_diretta_df<-c(history_ts,mape)
  }else
    ris_history_diretta_df<-rbind(ris_history_diretta_df,c(history_ts,mape))
}


rownames(ris_history_diretta_df)<-1:nrow(ris_history_diretta_df)
colnames(ris_history_diretta_df)<-c("#giorni_orizzonte","mape")

ris_history_diretta_df<-as.data.frame(ris_history_diretta_df)
ris_history_diretta_df<-ris_history_diretta_df %>% arrange(mape)

#commento: dopo aver testato le previsioni sui primi 334 giorni del test 
            #emerge che il miglior orizzonte temporale valori passati è 365



#N.B l'ottimizzazione degli iperparametri è stato effettuato con Python per problemi 
#computazionali riscontrati sul pc