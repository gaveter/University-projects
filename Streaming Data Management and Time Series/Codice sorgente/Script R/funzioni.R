trasforma_boxcox<-function(xts,lambda)
{
  if (lambda!=0)
    trasf<-(xts^lambda - 1) / lambda
  else
    trasf<-log(xts)
  return(trasf)
}

calcola_media_anno<-function(ts,range_year)
{
  for (i in 1:length(range_year))
  {
    start<-paste0(range_year[i],"-01-01")
    end<-paste0(range_year[i],"-12-31")
    anno_ts<-window(ts,start = start, end= end)
    if (!exists('mean_anno_vector'))
      mean_anno_vector<-mean(anno_ts)
    else
      mean_anno_vector<-cbind(mean_anno_vector, mean(anno_ts))
  }
  colnames(mean_anno_vector)<-range_year
  return(mean_anno_vector)
  
}

genera_grafico<-function(df_pre, range)
{
  testo<-paste0("Previsione dal ",index(range)[1])
  testo<-paste0(testo," al ")
  testo<-paste0(testo,index(range)[length(range)])
  
  df_plot<-melt(df_pre, id.vars='Data')
  colnames(df_plot)[2]<-"Legenda"
  
  ggplot(df_plot, aes(x=Data,y=value,colour=Legenda)) +
    ggtitle(testo) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_date(date_labels = "%B") +
    xlab("Data") +
    ylab("Prezzo") +
    geom_line()
  
}
confronta_previsioni<-function(df,pre1,pre2,index_col)
{
  df<-cbind(df, as.numeric(pre1))
  df<-cbind(df, as.numeric(pre2))
  colnames(df)<-index_col
  return(df)
}

calcola_performance<-function(df, name_models)
{
  rm(performance_df)
  real<-df[,"reale"]
  for (i in 3:length(df))
  {
    pred<-df[,i]
    df_mape<-data.frame(real, pred)
    mape<-calcola_MAPE(df_mape)
    if (!exists('performance_df'))
      performance_df<-as.data.frame(mape)
    else
      performance_df<-rbind(performance_df,mape)
    
  }
  rownames(performance_df)<-name_models
  colnames(performance_df)<-"MAPE"
  return(performance_df)
}

calcola_MAPE<-function(df)
{
  colnames(df)<-c("real","pred")
  df<-df %>% mutate(diff = abs(real-pred) / abs(real))
  mape<-as.numeric(colMeans(df))[3]*100
  return(mape)
}

genera_seno<-function(periodo,num_sin,lunghezza)
{
  x<-matrix(nrow =lunghezza,ncol=num_sin,NA)
  for (t in 1:lunghezza)
  {
    for(j in 1:num_sin)
    {
      x[t,j]<-sin(2*pi*t*j/periodo)
    }
  }
  return(x)
}

genera_coseno<-function(periodo,num_sin,lunghezza)
{
  x<-matrix(nrow =lunghezza,ncol=num_sin,NA)
  for (t in 1:lunghezza)
  {
    for(j in 1:num_sin)
    {
      x[t,j]<-cos(2*pi*t*j/periodo)
    }
  }
  return(x)
}

genera_modello_ARMA_migliore<-function(train,validation)
{
  for (p in 0:6)
  {
    for (q in 0:6)
    {
      debug<-paste(p,q,"-")
      print(debug)
      mod_x<-Arima(train, c(p,1,q), list(order = c(1,1,1), period=7), lambda = 0)
      pred<-forecast(mod_x,h=length(validation),biasadj = TRUE)$mean
      df<-data.frame(as.numeric(validation), pred)
      mape<-calcola_MAPE(df)
      if (!exists('res_models_df'))
        res_models_df<-c(p,q,mape)
      else
        res_models_df<-rbind(res_models_df,c(p,q,mape))
    }
  }
  colnames(res_models_df)<-c("AR","MA","MAPE")
  rownames(res_models_df)<-1:nrow(res_models_df)
  return(res_models_df)
}

crea_matrice_sinusoidi<-function(data_xts,n_sin)
{
  freq_matrix<-outer(1:length(data_xts), 1:n_sin)*2*pi/365.25
  
  cos<-cos(freq_matrix)
  colnames(cos)<-paste("cos",1:n_sin)
  
  sin<-sin(freq_matrix)
  colnames(sin)<- paste("sin",1:n_sin)
  
  reg<-as.matrix(cbind(cos,sin))
  return(reg)
}

update_custom_KFS_RW<-function(pars, model)
{
  model$Q[1,1,1]<- exp(pars[1])
  diag(model$Q[2:ncol(model$Q),2:ncol(model$Q),1])<-exp(pars[2]) #sin. hanno stessa varianza non componenti diffuse
  model$H[1,1,1]<- exp(pars[3])
  return(model) 
}

update_custom_KFS<-function(pars, model)
{
  if (pars[1]==-1)
    i<-2
  else
    i<-1
  
  model$Q[i,i,1]<- exp(pars[i])
  model$Q[i+1,i+1,1]<-exp(pars[i+1])
  diag(model$Q[(i+2):ncol(model$Q),(i+2):ncol(model$Q),1])<-exp(pars[i+2]) #sin. hanno stessa varianza non componenti diffuse
  model$H[1,1,1]<- exp(pars[i+3])
  return(model)
}

update_custom_KFS_5par<-function(pars, model)
{
  model$Q[1,1,1]<- exp(pars[1])
  model$Q[2,2,1]<-exp(pars[2])
  model$Q[3,3,1]<-exp(pars[3])
  diag(model$Q[4:ncol(model$Q),4:ncol(model$Q),1])<-exp(pars[4]) #sin. hanno stessa varianza non componenti diffuse
  model$H[1,1,1]<- exp(pars[5])
  return(model)
  
}

update_custom_KFS_IRW<-function(pars, model)
{
  model$Q[2,2,1]<- exp(pars[1])
  model$Q[3,3,1]<-exp(pars[2])
  diag(model$Q[4:ncol(model$Q),4:ncol(model$Q),1])<-exp(pars[3]) #sin. hanno stessa varianza non componenti diffuse
  model$H[1,1,1]<- exp(pars[4])
  return(model)
}

ritorna_giorni_settimana<-function(row)
{
  date<-as.Date(row[1])
  giorno<-weekdays(date)
  return(giorno)
}

ritorna_giorni_vacanza<-function(dato,arg1)
{
  national<-"-06-02"
  vettore<-character(length(arg1))
  anno<-as.numeric(dato)
  for (i in 1:length(arg1))
  {
    if (arg1[i]=='ITNationalDay')
      giorno<-paste0(anno,national)
    else
      giorno<-holiday(anno, arg1[i])
    
    vettore[i]<-as.character(giorno)
    
  }
  return(vettore)
}

trasforma_xts_svr<-function(xts)
{
  df<-as.data.frame(xts)
  df$Index<-index(xts)
  df<-df %>% select(Index,V1)
  colnames(df)[2]<-"Value"
  rownames(df)<-1:nrow(df)
  return(df)
}

crea_matrice_ml<-function(xts,history)
{
  inizio<-1
  fine<-1
  while(inizio+history<=length(xts))
  {
    fine<-inizio+history
    if(!exists('matrice_ml'))
      matrice_ml<-t(as.numeric(xts[inizio:fine]))
    else
      matrice_ml<-rbind(matrice_ml, t(as.numeric(xts[inizio:fine])))
    inizio<-inizio+1
  }
  colnames(matrice_ml)<-paste0("V",1:ncol(matrice_ml))
  colnames(matrice_ml)[ncol(matrice_ml)]<-"Y"
  return(matrice_ml)
}

converti_dato_originale<-function(data,media,sd)
{
  reale<-(data*sd)+media
  return(reale)
}

crea_matrice_multistep<-function(xts,history,step_future)
{
  inizio<-1
  fine<-history+step_future
  while(inizio+(fine-1)<=length(xts))
  {
    if(!exists('matrice_multistep'))
      matrice_multistep<-t(as.numeric(xts[inizio:fine]))
    else
      matrice_multistep<-rbind(matrice_multistep, t(as.numeric(xts[inizio:(inizio+(fine-1))])))
    
    inizio<-inizio+1
    
  }
  nomi_colonne<-c(paste0("V",1:history),paste0("Y",1:step_future))
  colnames(matrice_multistep)<-nomi_colonne
  return(matrice_multistep)
}
