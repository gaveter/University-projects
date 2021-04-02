#funzione che ritorna lista utenti rt in uno specifica data
ritorna_lista<-function(rt)
{
  utenti<-as.character(rt$user_screen_name)
  utenti_retweettati<-as.character(rt$retweet_screen_name)
  
  utenti<-c(utenti,utenti_retweettati)
  utenti_unique<-unique(utenti) #lista utenti presenti
  
  id<-seq(1,length(utenti_unique))
  utenti_id<-as.data.frame(cbind(id,utenti_unique))
  
  #ottengo id degli utenti
  retweet_id<-merge(rt,utenti_id,by.x = 'retweet_screen_name',by.y = 'utenti_unique')
  colnames(retweet_id)[3]<-"id_retweet"
  retweet_id<-merge(retweet_id,utenti_id,by.x = 'user_screen_name',by.y = 'utenti_unique')
  colnames(retweet_id)[4]<-"id_user"
  
  #browser()
  relation_id<-retweet_id %>% select(id_user,id_retweet)
  personechesiritwittano<-relation_id %>% filter(id_user==id_retweet)
  
  
  primacolonna<-as.numeric(as.character(relation_id$id_user))
  secondacolonna<-as.numeric(as.character(relation_id$id_retweet))
  relation_id.m<-cbind(primacolonna,secondacolonna)
  
  #return(relation_id.m)
  
  relation_id.df<-as.data.frame(relation_id.m)
  relation_id.df<-relation_id.df %>% arrange(primacolonna)
  relation_id.df<-relation_id.df %>% mutate(coppia=paste(primacolonna,"",secondacolonna))
  
  relationlista<-relation_id.df$coppia
  relationlista<-strsplit(relationlista," ") #creo lista
  relationlista<-unlist(relationlista) #flat lista
  relationlista<-relationlista[!relationlista %in% ""]
  
  utenti<-as.data.frame(relationlista)
  colnames(utenti)<-"id"
  utenti_screenID<-join(utenti,utenti_id,type="inner")
  
  listautenti<-as.character(utenti_screenID$utenti_unique)
  listautenti_id<-as.numeric(as.character(utenti_screenID$id))
  return(list('listautenti'=listautenti,'num_relazioni'=nrow(relation_id.df),
              'cappi'=nrow(personechesiritwittano)))
}

ritornagradiutenti<-function(df)
{
  colnames(df)<-'grado'
  gradi<-df %>% mutate(utenti=rownames(df)) %>% arrange(desc(df))
  numeroNodi<-nrow(gradi)
  
  numNodiGrado<-gradi %>% group_by(grado) %>% dplyr::summarise(count=n())
  
  gradi<-merge(gradi,numNodiGrado,by='grado')
  gradi<- gradi %>% mutate(freq=count/numeroNodi) %>% arrange(desc(grado))
  return(gradi)
}