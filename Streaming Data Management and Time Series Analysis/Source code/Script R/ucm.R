library(KFAS)
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
library(timeDate)
library(xts)
library(lubridate)

setwd('D:\\Università\\Magistrale\\Secondo anno\\Streaming Data Management and time series\\Progetto')

time_series_xts<-readRDS('time_series_xts.RDS')

time_series_ts<-readRDS('time_series_ts.RDS')

df_prev_model<-readRDS('df_previsioni_finali.RDS')

dummy_hol_df<-readRDS('holiday_dummy_df.RDS')

source('funzioni.R')

#creazione range train, test e validation
train_range<-seq(as.Date("2010-01-01"),as.Date("2016-12-31"),"day")
validation_range<-seq(as.Date("2017-01-01"), as.Date("2017-12-31"),"day")
test_range<-seq(as.Date("2018-01-01"), as.Date("2018-11-30"),"day")


train_xts<-time_series_xts[train_range]
test_xts<-time_series_xts[test_range]
validation_xts<-time_series_xts[validation_range]

time_series_ts_NA<-time_series_ts

#validation e test settati a NA
time_series_ts_NA[(length(train_range)+1):length(time_series_ts_NA)]<-NA

var_y<-as.numeric(var(time_series_ts_NA,na.rm = TRUE)) #varianza train ts

#parametri iniziali
par_initial<-numeric(3)
par_initial[1]<-log(var_y/100) #var. level
par_initial[2]<-log(0.01) # var. stag.
par_initial[3]<-log(var_y/100) #var errore oss.


#primo modello UCM: RW + stag settimanale (dummy) + rumore
mod1<- SSModel(time_series_ts_NA ~ 0+
                 SSMtrend(1,NA) +
                 SSMseasonal(7, NA, 'dummy'),
               H = NA)
mod1$Q


fit_mod1<-fitSSM(mod1,par_initial, control=list(maxit=1000))
fit_mod1$optim.out$convergence #commento: 0 converge

fit_mod1$model$Q #matrice var.covarianza con i parametri stimati

kfilter_mod1<-KFS(fit_mod1$model, smoothing = c("state","disturbance","signal"))

plot(time_series_ts[(length(train_xts)+366):length(time_series_ts)],type='lines') +
  lines(kfilter_mod1$muhat[(length(train_xts)+366):length(kfilter_mod1$muhat)], 
        col='blue')


ris_df<-fortify.zoo(test_xts)
pre_values_mod<-kfilter_mod1$muhat[((length(train_xts)+366)):(length(kfilter_mod1$muhat)-31)]
ris_df<-cbind(ris_df,as.numeric(pre_values_mod))
colnames(ris_df)<-c("Data","reale","UCM (RW + stag 7 (dummy))")

genera_grafico(ris_df,test_xts)

perf_df<-calcola_performance(ris_df,colnames(ris_df)[3])



#secondo modello UCM: RW + stag. settimanale (trig.) + rumore
mod2<- SSModel(time_series_ts_NA ~ 0 +
                 SSMtrend(1,NA) +
                 SSMseasonal(7,NA, "trigonometric"),
               H=NA)

mod2$Q

fit_mod2<-fitSSM(mod2, par_initial, update_custom_KFS_RW)
fit_mod2$optim.out$convergence

fit_mod2$model$Q

kfilter_mod2<-KFS(fit_mod2$model, smoothing = c("state","disturbance","signal"))

pre_mod<-kfilter_mod2$muhat[(length(train_xts)+366):(length(kfilter_mod1$muhat)-31)]


ris_df<-cbind(ris_df, as.numeric(pre_mod))
colnames(ris_df)[ncol(ris_df)]<-"UCM (RW + stag 7 (trig))"

genera_grafico(ris_df, test_xts)

perf_df<-calcola_performance(ris_df,colnames(ris_df[3:ncol(ris_df)]))


#commento: si decide di modellare la stag. settimanale con una dummy apposto che una trig.
          #per ridurre la complessità computazionale dei parametri da stimare



#terzo modello UCM: RW + stag. settimanale (dummy) + stag. annuale (trig) + rumore

#validation su iperparametro armoniche (1 anno - 2017)
validation_ts<- as.numeric(time_series_xts["2010-01-01/2017-12-31"])
validation_xts_ott<- time_series_xts["2010-01-01/2017-12-31"]

validation_ts_NA<-validation_ts
validation_ts_NA[(length(validation_ts)-364): length(validation_ts)]<-NA

var_y<-as.numeric(var(validation_ts_NA,na.rm = TRUE)) #varianza dati

par_initial<-numeric(4)
par_initial[1]<-log(var_y/100) #var. level
par_initial[2]<-log(0.01) # var. stag. 7
par_initial[3]<-log(0.01) # var. stag. 365
par_initial[4]<-log(var_y/100) #var errore oss.

ris_ott_df<-as.data.frame(validation_ts[(length(validation_ts)-364):length(validation_ts)])
ris_ott_df<-cbind(index(validation_xts_ott[(length(validation_ts)-364):length(validation_ts)]), ris_ott_df)
colnames(ris_ott_df)<-c("Data","reale")

rm(ris_sin_ott_df)
for (i in 2:5)
{
  mod_i<-SSModel(validation_ts_NA ~ 0 +
                  SSMtrend(1, NA) +
                  SSMseasonal(7,NA,"dummy") +
                  SSMseasonal(365, NA, "trigonometric", harmonics = 1:i),
                H = NA)
  print(i)
  
  mod_i$P1inf<-mod_i$P1inf * 0 #in questo modo non ci sono più componenti diffuse (var. inf)
  mod_i$a1["level",1] <- mean(validation_ts_NA, na.rm = TRUE)
  diag(mod_i$P1)<- var_y
  
  fit_mod_i<-fitSSM(mod_i, par_initial, update_custom_KFS, control = list(maxit=1000))
  kfilter_mod_i<-KFS(fit_mod_i$model, smoothing = c("state","disturbance","signal"))
  
  prev<-kfilter_mod_i$muhat[(length(validation_ts_NA)-364): length(validation_ts_NA)]
  
  ris_ott_df[,3]<-prev
  colnames(ris_ott_df)[3]<-paste("numero sinusoidi",i,":")
  ris<-calcola_performance(ris_ott_df,colnames(ris_ott_df)[2])
  ris<-cbind(ris,i)
  
  if(!exists("ris_sin_ott_df"))
    ris_sin_ott_df<-as.data.frame(ris)
  else
    ris_sin_ott_df<-rbind(ris_sin_ott_df,ris)
}

colnames(ris_sin_ott_df)[2]<-"numero_sinusoidi"
rownames(ris_sin_ott_df)<-1:nrow(ris_sin_ott_df)
ris_sin_ott_df<- ris_sin_ott_df %>% 
   select(numero_sinusoidi,MAPE) %>%
  arrange(MAPE)
#commento: eseguendo il ciclo si ottiene come numero ottimo sinusoidi 19


mod3_ott<-SSModel(time_series_ts_NA ~ 0 +
                SSMtrend(1, NA) +
                SSMseasonal(7,NA,"dummy") +
                SSMseasonal(365, NA, "trigonometric", harmonics = 1:19),
              H = NA)

mod3_ott$Q

var_y<-as.numeric(var(time_series_ts_NA,na.rm = TRUE)) #varianza train ts

par_initial<-numeric(4)
par_initial[1]<-log(var_y/100) #var. level
par_initial[2]<-log(0.01) # var. stag. 7
par_initial[3]<-log(0.01) # var. stag. 365
par_initial[4]<-log(var_y/100) #var errore oss.


mod3_ott$P1inf<-mod3_ott$P1inf * 0 #commento: in questo modo non ci sono più componenti diffuse (var. inf)
mod3_ott$a1["level",1] <- mean(time_series_ts_NA, na.rm = TRUE)
diag(mod3_ott$P1)<- var_y

fit_mod3_ott<-fitSSM(mod3_ott, par_initial, update_custom_KFS, control=list(maxit=1000))

kfilter_mod3_ott<-KFS(fit_mod3_ott$model,smoothing = c("state","disturbance","signal"))

plot(time_series_ts[(length(train_xts)+366):length(time_series_ts)],type='lines') +
  lines(kfilter_mod3_ott$muhat[(length(train_xts)+366):length(time_series_ts)], col='blue')


pre_values_mod_ott<-kfilter_mod3_ott$muhat[(length(train_xts)+366):(length(kfilter_mod3_ott$muhat)-31)]
ris_df<-cbind(ris_df,as.numeric(pre_values_mod_ott))
colnames(ris_df)[ncol(ris_df)]<-"UCM (RW + stag 7 (dummy) + stag. 365 (trig.)"

genera_grafico(ris_df,test_xts)

perf_df<-calcola_performance(ris_df,colnames(ris_df)[3:ncol(ris_df)])



#quarto modello UCM: LLT + stag. settimanale (dummy) + stag. annuale (trig.) + rumore
mod4<- SSModel(time_series_ts_NA ~ 0 +
                 SSMtrend(2, list(NA,NA)) +
                 SSMseasonal(7,NA,"dummy") +
                 SSMseasonal(365, NA, "trigonometric", harmonics = 1: 19),
               H = NA)

mod4$Q

par_initial<-numeric(5)
par_initial[1]<-log(var_y/100) #var. level
par_initial[2]<-log(0.01) #var. slope
par_initial[3]<-log(0.01) # var. stag. 7
par_initial[4]<-log(0.01) # var. stag. 365
par_initial[5]<-log(var_y/100) #var errore oss.

mod4$P1inf<-mod4$P1inf * 0 #in questo modo non ci sono più componenti diffuse (var. inf)
mod4$a1["level",1] <- mean(time_series_ts_NA, na.rm = TRUE)
diag(mod4$P1)<- var_y

fit_mod4<- fitSSM(mod4, par_initial, update_custom_KFS_5par, control=list(maxit=1000))

kfilter_mod4<-KFS(fit_mod4$model, smoothing = c("state","disturbance","signal"))

plot(time_series_ts[(length(train_xts)+366):length(time_series_ts)],type='lines') +
  lines(kfilter_mod4$muhat[(length(train_xts)+366):length(time_series_ts)], col='blue')

pre_values_mod4<-kfilter_mod4$muhat[(length(train_xts)+366):(length(kfilter_mod4$muhat)-31)]

ris_df<-cbind(ris_df, pre_values_mod4)
colnames(ris_df)[ncol(ris_df)]<-"UCM (LLT + stag 7 (dummy) + stag 365 (trig.)"

genera_grafico(ris_df[,-3],test_xts)

perf_df<-calcola_performance(ris_df,colnames(ris_df)[3:ncol(ris_df)])


#si prova come ultimo modello ad aggiungere regressori che considerano festività
train_df<-dummy_hol_df

train_reg_xts<-xts(train_df[,-1], train_df$Date)
train_reg_xts$y<-as.numeric(time_series_xts)
train_reg_xts[index(test_xts),"y"]<-NA

train_reg_df<-as.data.frame(train_reg_xts)


mod5_reg<-SSModel(y ~ 0 +
                    Easter +
                    EasterMonday +
                    ChristmasDay +
                    BoxingDay +
                    ChristmasEve +
                    NewYearsDay +
                    SSMtrend(1, NA) +
                    SSMseasonal(7,NA,"dummy") +
                    SSMseasonal(365, NA, "trigonometric", harmonics = 1:19),
                  H = NA,
                  data = train_reg_df)


var_y<-as.numeric(var(train_reg_df$y,na.rm = TRUE)) #varianza train ts

par_initial<-numeric(4)
par_initial[1]<-log(var_y/100) #var. level
par_initial[2]<-log(0.01) # var. stag. 7
par_initial[3]<-log(0.01) # var. stag. 365
par_initial[4]<-log(var_y/100) #var errore oss.


mod5_reg$P1inf<-mod5_reg$P1inf * 0 #commento: in questo modo non ci sono più componenti diffuse (var. inf)
mod5_reg$a1["level",1] <- mean(train_reg_df$y,na.rm = TRUE)
diag(mod5_reg$P1)<- var_y

fit_mod5_reg<-fitSSM(mod5_reg, par_initial, update_custom_KFS, control=list(maxit=1000))

kfilter_mod5_reg<-KFS(fit_mod5_reg$model,smoothing = c("state","disturbance","signal"))

plot(time_series_ts[(length(train_xts)+366):length(time_series_ts)],type='lines') +
  lines(kfilter_mod5_reg$muhat[(length(train_xts)+366):length(time_series_ts)], col='blue')

pre_values_mod5<-kfilter_mod5_reg$muhat[(length(train_xts)+366):(length(kfilter_mod5_reg$muhat)-31)]

ris_df<-cbind(ris_df, pre_values_mod5)
colnames(ris_df)[ncol(ris_df)]<-"UCM (RW + stag 7 (dummy) + stag. 365 (trig.) + reg. festività"

genera_grafico(ris_df[,-3],test_xts)

perf_df<-calcola_performance(ris_df,colnames(ris_df)[3:ncol(ris_df)])

genera_grafico(ris_df[,-c(3,4,6)],test_xts)


#confronto MAPE train MAPE test modello migliore
val_train_pred<-fitted(kfilter_mod5_reg)

df<-data.frame(train_xts,val_train_pred[1:length(train_xts)])
perf_train<-calcola_MAPE(df)

perf_mod_migliore<-t(as.data.frame(c(perf_train,perf_df[3,"MAPE"])))
rownames(perf_mod_migliore)<-"MAPE"
colnames(perf_mod_migliore)<-c("Training","Test")

genera_grafico(ris_df[,-c(3,4)],test_xts)



#previsione finale 2019-01-01/30-11-2019
prev_index<-seq(as.Date("2019-01-01"), as.Date("2019-11-30"),by='days')

time_series_ts_def_NA<-c(time_series_ts,rep(NA,334))

var_y<-as.numeric(var(time_series_ts_def_NA,na.rm = TRUE)) #var ts

par_initial<-numeric(4)
par_initial[1]<-log(var_y/100) #var. level
par_initial[2]<-log(0.01) # var. stag. 7
par_initial[3]<-log(0.01) # var. stag. 365
par_initial[4]<-log(var_y/100) #var errore oss.

mod_def<-SSModel(time_series_ts_def_NA ~ 0 +
                    SSMtrend(1, NA) +
                    SSMseasonal(7,NA,"dummy") +
                    SSMseasonal(365, NA, "trigonometric", harmonics = 1:19),
                  H = NA,
                 data = train_reg_df)


mod_def$P1inf<-mod_def$P1inf * 0 #commento: in questo modo non ci sono più componenti diffuse (var. inf)
mod_def$a1["level",1] <- mean(time_series_ts_def_NA, na.rm = TRUE)
diag(mod_def$P1)<- var_y

fit_mod_def<-fitSSM(mod_def, par_initial, update_custom_KFS, control=list(maxit=1000))

kfilter_mod_def<-KFS(fit_mod_def$model,smoothing = c("state","disturbance","signal"))

plot(kfilter_mod_def$muhat[(length(time_series_xts)+1):length(time_series_ts_def_NA)], col='blue',type='lines')


pre_values_mod_def<-kfilter_mod_def$muhat[(length(time_series_xts)+1):length(time_series_ts_def_NA)]

df_prev_model$UCM<-pre_values_mod_def
saveRDS(df_prev_model,file="df_previsioni_finali.RDS")


