library(dplyr)
library(lsa)
library(plyr)
library(igraph)
library(stringr)
library(ggplot2)
library(RColorBrewer)

#BISOGNA CAMBIARE WD
setwd("D:\\Università\\Magistrale\\Secondo anno\\Social Media Analytics\\Progetto\\rete\\")
getwd()
source('funzioni_def.R') #si richiamano funzioni necessarie
relation_utenti<- read.csv("relazioni.csv")

relation_utenti<- relation_utenti %>% select(-X)

#estrazione intervallo tweet
giornomese<-relation_utenti %>% select(month,day)
giornomese<-unique(giornomese) %>% arrange(month,day)

giornomese<-giornomese %>% mutate(mese=if_else(month=='Dec','dicembre','gennaio'))

giornomese$complete<-seq(as.Date("2019-12-27"),as.Date("2020-01-08"),"day")




#ANALISI GIORNO PER GIORNO (DA 27 a 8)
gradi.lista<-list()
rm(hub3_grado.df)

for (i in 1:nrow(giornomese))
{
  testo<-paste0(giornomese[i,"day"]," ",giornomese[i,"mese"])
  print(testo)
  rt_giornomese<-relation_utenti %>% filter(month==giornomese[i,"month"] & day==giornomese[i,"day"]) %>%
    select(-c(month,day))
  
  rt_giornomese<-rt_giornomese %>% mutate(user_screen_name=as.character(user_screen_name)) %>%
    mutate(retweet_screen_name=as.character(retweet_screen_name))
  rt_giornomese_noCappi<-rt_giornomese %>% filter(user_screen_name!=retweet_screen_name)
  
  
  listautenti_numRelation<-ritorna_lista(rt_giornomese_noCappi) #ritorna lista utenti relazioni e numerorelazioni
  
  g_giorno<-graph(listautenti_numRelation$listautenti,directed=FALSE)
  g_giorno<-simplify(g_giorno)
  
  giornomese[i,"num_vertici"]<-vcount(g_giorno)
  giornomese[i,"num_archi"]<-gsize(g_giorno)
  
  plot(main=testo,g_giorno, vertex.size= 4,vertex.color="gold",
       edge.arrow.size=0.09,vertex.label.color='blue',vertex.label=NA)
}
write.csv(giornomese,"evoluzione.csv")




#ANALISI CUMULATIVA
rm(hub3_grado_cumulata.df)

for (i in 2:nrow(giornomese))
{
  int<-giornomese[1:i,'day']
  relation_utenti_c<-relation_utenti %>% filter(day %in% int) %>% select(-c(month,day))
  relation_utenti_c<-relation_utenti_c %>% mutate(user_screen_name=as.character(user_screen_name)) %>%
    mutate(retweet_screen_name=as.character(retweet_screen_name))
  relation_utenti_c_noCappi<-relation_utenti_c %>% filter(user_screen_name!=retweet_screen_name)
  n_relation<-nrow(relation_utenti_c_noCappi)
  utenti<-c(relation_utenti_c_noCappi$user_screen_name,relation_utenti_c_noCappi$retweet_screen_name)
  n_utenti<-length(unique(utenti))
  info<-paste0("Numero relazioni: ",n_relation)
  info<-paste0(info," , ")
  info<-paste0(info, paste0("Numero utenti: ",n_utenti))
  testo<-as.character(int)
  titolo<-paste0("da ",testo[1])
  titolo<-paste0(titolo," a ")
  titolo<-paste0(titolo,testo[length(testo)])
  titolo<-ifelse(testo[1]==testo[length(testo)],testo[1],titolo)
  infoRow<-titolo
  titolo<-paste0(titolo," | ")
  titolo<-paste0(titolo,info)
  
  listautenti_numRelation_cum<-ritorna_lista(relation_utenti_c_noCappi)
  
  g_cumulata<-graph(listautenti_numRelation_cum$listautenti,directed=FALSE)
  
  gradi.df<-as.data.frame(degree(g_cumulata, normalized = TRUE))
  gradi_cumulata<-ritornagradiutenti(gradi.df)
  gradi_cumulata<-gradi_cumulata %>% arrange(desc(grado))
  
  hub_grado<-t(head(gradi_cumulata,3))
  hub_grado<-c(hub_grado[2,],hub_grado[1,])
  hub_grado<-t(as.data.frame(hub_grado))
  rownames(hub_grado)<-infoRow
  
  if (!exists('hub3_grado_cumulata.df'))
    hub3_grado_cumulata.df<-hub_grado
  else
    hub3_grado_cumulata.df<-rbind(hub3_grado_cumulata.df,hub_grado)
  
  plot(main=titolo,g_cumulata, vertex.size= 3, vertex.size.color='lightblue',
       edge.arrow.size=0.08,vertex.label.color='blue',vertex.label=NA)
  
}
hub3_grado_cumulata.df<-as.data.frame(hub3_grado_cumulata.df)

colnames(hub3_grado_cumulata.df)<-c("hub1","hub2","hub3","grado1","grado2","grado3")
hub3_grado_cumulata.df<-hub3_grado_cumulata.df %>% select(hub1,grado1,hub2,grado2,hub3,grado3)
write.csv(hub3_grado_cumulata.df,file='csv/hub3_gradoN_cumulata.csv')
