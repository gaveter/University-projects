#### LIBRERIE USATE ####
library(dplyr)
library(magrittr)
library(ggplot2)
library(forcats)
library(reshape2)
library(caret)
library(olsrr)
library(rattle)
library(varhandle)
library(keras)
library(DataCombine)
library(tidyr)
library(BBmisc)
library(e1071)


####################################################################################################################################################

# Si caricano i dataset utili per creare il DF che conterrà le informazioni necessarie 

# Per poter importare i file bisogna cambiare la directory

dir <- "/home/federico/Scrivania/Università/Web_marketing/"
set.seed(12345)

### FIDELIZZAZIONE ###
df_1_cli_fid <- read.csv2(paste0(dir, "raw_1_cli_fid.csv"), na.strings = c("NA", ""))

### DETTAGLI ACCOUNT ###
df_2_cli_account <- read.csv2(paste0(dir, "raw_2_cli_account.csv"), na.strings = c("NA", ""))

### INDIRIZZI CLIENTI ###
df_3_cli_address <- read.csv2(paste0(dir, "raw_3_cli_address.csv"), na.strings = c(""), stringsAsFactors = F)

### PRIVACY CLIENTI ###
df_4_cli_privacy <- read.csv2(paste0(dir, "raw_4_cli_privacy.csv") , na.strings = c("NA", ""))

### CAMPAGNE EMAIL ###
df_5_camp_cat <- read.csv2(paste0(dir, "raw_5_camp_cat.csv") , na.strings = c("NA", ""))

### EVENTI EMAIL ###
df_6_camp_event <- read.csv2(paste0(dir, "raw_6_camp_event.csv") , na.strings = c("NA", ""))


# PROPENSITY OF EMAIL ENGAGEMENT (PRIMA DOMANDA DI BUSINNES)
# Preprocessing


# Dataset fidelizzazione (1)

# Overview
str(df_1_cli_fid)
summary(df_1_cli_fid)

# Lavoro sul DF

df_1_cli_fid_clean <- df_1_cli_fid

# Formattazione date 
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(DT_ACTIVE = as.Date(DT_ACTIVE))

# Formattazione Attributi in Factor 
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(ID_NEG = as.factor(ID_NEG)) %>%
  mutate(TYP_CLI_FID = as.factor(TYP_CLI_FID)) %>%
  mutate(STATUS_FID = as.factor(STATUS_FID))


# Conteggio Fidelity per cliente #
num_fid_x_cli <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  summarize(NUM_FIDs =  n_distinct(ID_FID), NUM_DATEs = n_distinct(DT_ACTIVE))

# Si modificano i campi fidelizzazione, per avere la prima fidelizzazione e quella attuale
df_1_cli_fid_first <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  filter(DT_ACTIVE == min(DT_ACTIVE)) %>%
  arrange(ID_FID) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

df_1_cli_fid_last <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  filter(DT_ACTIVE == max(DT_ACTIVE)) %>%
  arrange(desc(ID_FID)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

df_1_cli_fid_clean <- df_1_cli_fid_last %>%
  left_join(df_1_cli_fid_first %>%
              select(ID_CLI, FIRST_ID_NEG = ID_NEG, FIRST_DT_ACTIVE = DT_ACTIVE), by = 'ID_CLI') %>%
  left_join(num_fid_x_cli %>%
              select(ID_CLI, NUM_FIDs) %>%
              mutate(NUM_FIDs = as.factor(NUM_FIDs)), by = 'ID_CLI')


# Dataset dettagli account (2):

# Overview

str(df_2_cli_account)
summary(df_2_cli_account)


df_2_cli_account_clean <- df_2_cli_account

# Booleani in factor 
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(W_PHONE = as.factor(W_PHONE))

# Categorici in factor 
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(TYP_CLI_ACCOUNT = as.factor(TYP_CLI_ACCOUNT))


# Si sostituisce a NA i valori 0 e missing
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(W_PHONE = fct_explicit_na(W_PHONE, "0")) %>%
  mutate(EMAIL_PROVIDER = fct_explicit_na(EMAIL_PROVIDER, "(missing)")) %>%
  mutate(TYP_JOB = fct_explicit_na(TYP_JOB, "(missing)"))

# Si osservano quali valori assumono e di che tipo sono gli attributi

str(df_2_cli_account_clean)
summary(df_2_cli_account_clean)


# Sono presenti molti missing in EMAIL_PROVIDER, si procede a risolvere
freq_email_providers <- df_2_cli_account_clean %>%
  group_by(EMAIL_PROVIDER) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs))


clean_email_providers <- freq_email_providers %>%
  mutate(EMAIL_PROVIDER = as.character(EMAIL_PROVIDER)) %>%
  mutate(AUX = if_else(PERCENT_COVERED < 0.85 | (PERCENT_COVERED > 0.85 & lag(PERCENT_COVERED) < 0.85), 1,0)) %>%
  mutate(EMAIL_PROVIDER_CLEAN = if_else(AUX | EMAIL_PROVIDER == "(missing)", EMAIL_PROVIDER, "others"))


df_2_cli_account_clean <- df_2_cli_account %>%
  mutate(EMAIL_PROVIDER = as.character(EMAIL_PROVIDER)) %>%
  left_join(clean_email_providers %>%
              select(EMAIL_PROVIDER, EMAIL_PROVIDER_CLEAN)
            , by = "EMAIL_PROVIDER") %>%
  select(-EMAIL_PROVIDER) %>%
  mutate(EMAIL_PROVIDER_CLEAN = as.factor(EMAIL_PROVIDER_CLEAN))


# Circa 27000 clienti non hanno inserito un numero telefonico. 
# Il tipo di lavoro risulta inutile, troppi valori mancanti (quasi tutti).



# Dataset Indirizzi Clienti (4)

# Overview
str(df_3_cli_address)
summary(df_3_cli_address)


df_3_cli_address_clean <- df_3_cli_address

# PRV e REGION in factor
df_3_cli_address_clean <- df_3_cli_address_clean %>%
  mutate(PRV = as.factor(PRV)) %>%
  mutate(REGION = as.factor(REGION)) %>%
  distinct()

df_3_cli_address_clean %>%
  group_by(w_CAP = !is.na(CAP), w_PRV = !is.na(PRV), w_REGION = !is.na(REGION)) %>%
  summarize(TOT_ADDs = n_distinct(ID_ADDRESS))


# Eliminazione record che non hanno valori validi
df_3_cli_address_clean <- df_3_cli_address_clean %>%
  filter(!is.na(CAP) & !is.na(PRV) & !is.na(REGION))


# Dataset Privacy Clienti (4)

# Overview

str(df_4_cli_privacy)
summary(df_4_cli_privacy)


# Preprocessing pulizia dati

df_4_cli_privacy_clean <- df_4_cli_privacy

# Booleani in factor
df_4_cli_privacy_clean <- df_4_cli_privacy_clean %>%
  mutate(FLAG_PRIVACY_1 = as.factor(FLAG_PRIVACY_1)) %>%
  mutate(FLAG_PRIVACY_2 = as.factor(FLAG_PRIVACY_2)) %>%
  mutate(FLAG_DIRECT_MKT = as.factor(FLAG_DIRECT_MKT))


# Dataset Campagna Promozionale Email (5)

# Overview

str(df_5_camp_cat)
summary(df_5_camp_cat)


# Preprocessing e pulizia dati
df_5_camp_cat_clean <- df_5_camp_cat

# Si rimuove l'attributo "CHANNEL_CAMP" in quanto ha solo un valore
df_5_camp_cat_clean <- df_5_camp_cat_clean %>%
  select(-CHANNEL_CAMP)



# Dataset eventi email (6)

# Overview

str(df_6_camp_event)
summary(df_6_camp_event)


# Preprocessing e pulizia dati

df_6_camp_event_clean <- df_6_camp_event

# Conversione Event_date in data formattata correttamente
df_6_camp_event_clean <- df_6_camp_event_clean %>%
  mutate(EVENT_DATE = as.Date(EVENT_DATE, format="%Y-%m-%dT%H:%M:%S"))

# Non si considera alcuna differenza tra bounce ed error, vengono combinati nella categoria "failure"
df_6_camp_event_clean <- df_6_camp_event_clean %>%
  mutate(TYP_EVENT = as.factor(if_else(TYP_EVENT == "E" | TYP_EVENT == "B", "F", as.character(TYP_EVENT))))



####################################################################################################################################################



# CREAZIONE DATAFRAME PER IL PROPENSITY OF EMAIL ENGAGEMENT

# Si effettuano le seguenti operazioni:
  # 1) join DF 6 e DF 5
  # 2) si calcola il numero di email:
        # inviate
        # aperte
        # cliccate
        # non a buon fine
        # iniviate e aperte
        # inviate ma non aperte
  # 3) si calcola il tempo trascorso tra l'invio della mail e l'evento associato


df_6_camp_event_clean_w_type <- df_6_camp_event_clean %>%
  left_join(df_5_camp_cat_clean
            , by = "ID_CAMP")


# Inviate
df_sents <- df_6_camp_event_clean_w_type %>%
  filter(TYP_EVENT == "S") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_S = ID_EVENT, ID_CLI, ID_CAMP, TYP_CAMP, ID_DELIVERY, SEND_DATE = EVENT_DATE)

# Aperte
df_opens <- df_6_camp_event_clean_w_type %>%
  filter(TYP_EVENT == "V") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_O = ID_EVENT, ID_CLI, ID_CAMP, TYP_CAMP, ID_DELIVERY, OPEN_DATE = EVENT_DATE) %>%
  group_by(ID_CLI, ID_CAMP, ID_DELIVERY) %>%
  filter(OPEN_DATE == min(OPEN_DATE)) %>%
  filter(row_number() == 1) %>%
  ungroup()

# Click
df_clicks <- df_6_camp_event_clean_w_type %>%
  filter(TYP_EVENT == "C") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_C = ID_EVENT, ID_CLI, ID_CAMP, TYP_CAMP, ID_DELIVERY, CLICK_DATE = EVENT_DATE) %>%
  group_by(ID_CLI, ID_CAMP, ID_DELIVERY) %>%
  filter(CLICK_DATE == min(CLICK_DATE)) %>%
  filter(row_number() == 1) %>%
  ungroup()

# Non anadate a buon fine
df_fails <- df_6_camp_event_clean_w_type %>%
  filter(TYP_EVENT == "F") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_F = ID_EVENT, ID_CLI, ID_CAMP, TYP_CAMP, ID_DELIVERY, FAIL_DATE = EVENT_DATE) %>%
  group_by(ID_CLI, ID_CAMP, ID_DELIVERY) %>%
  filter(FAIL_DATE == min(FAIL_DATE)) %>%
  filter(row_number() == 1) %>%
  ungroup()

# Email aperte con allegati
df_sents_w_open <- df_sents %>%
  left_join(df_opens
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(OPEN_DATE) | SEND_DATE <= OPEN_DATE) %>%
  mutate(DIFF = as.integer(OPEN_DATE - SEND_DATE))

# Email inviate ma non aperte (escluse quelle di cui non si conosce l'esito "NA")
df_sents_w_open %>%
  group_by(w_open = !is.na(DIFF)) %>%
  summarize(TOT_SENTs = n_distinct(ID_EVENT_S)) %>%
  mutate(PERCENT = TOT_SENTs/sum(TOT_SENTs)) %>%
  arrange(desc(PERCENT))


# Distribuzione del tempo che trascorre da invio ad apertura
df_sents_w_open %>% filter(!is.na(DIFF)) %>%
  group_by(DIFF) %>%
  summarize(TOT_EVENTs = n_distinct(ID_EVENT_S)) %>%
  arrange(DIFF) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_EVENTs)/sum(TOT_EVENTs))

# Grafico cumulativo
ggplot(df_sents_w_open %>% filter(!is.na(DIFF)) %>%
         group_by(DIFF) %>%
         summarize(TOT_EVENTs = n_distinct(ID_EVENT_S)) %>%
         arrange(DIFF) %>%
         mutate(PERCENT_COVERED = cumsum(TOT_EVENTs)/sum(TOT_EVENTs)) %>%
         filter(DIFF <= 14)
       , aes(y=PERCENT_COVERED, x=DIFF)) + geom_line() + geom_point() + scale_x_continuous(breaks=seq(0,14,2), minor_breaks=0:14) + 
  ggtitle("Cumulative percentage of interaction by days") + 
  theme(plot.title = element_text(hjust = 0.5))


# DEFINIZIONE VARIABILE TARGET

# Lo scopo è sapere se una email inviata viene aperta entro l'arco temporale definito,
# la variabile TARGET assumerà valore 1 in caso di esito positivo, 0 altrimenti.

# Viene scelta una finestra temporale di 3 giorni
# principalmente perchè si vuole lasciare al cliente l'opportunità di interagire con l'email
# anche dopo un weekend o un ponte festivo

window_days <- 3


target_event <- df_sents_w_open %>%
  mutate(TARGET = as.factor(if_else(!is.na(DIFF) & DIFF <= window_days, "1", "0"))) %>%
  select(ID_EVENT_S, ID_CLI, ID_CAMP, ID_DELIVERY, SEND_DATE, TARGET)


# Variabili informative utili
# Si aggiungono informazioni determinate dai 21 giorni precedenti per ogni evento mail, 
# nello specifico si contano tutti gli eventi di invio, apertura e click delle email.
# L'arco temporale considerato è il seguente: 1/2/19 - 30/4/19:


rate_window <- 21
prev_window <- 21

dt_start <- as.Date("2019-02-01")
dt_end <- as.Date("2019-04-30") - window_days

relevant_event <- df_sents %>%
  left_join(df_opens
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(OPEN_DATE) | SEND_DATE <= OPEN_DATE) %>%
  left_join(df_clicks
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(CLICK_DATE) | SEND_DATE <= CLICK_DATE) %>%
  left_join(df_fails
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(FAIL_DATE) | SEND_DATE <= FAIL_DATE) %>%
  mutate(DIFF_OPEN = as.integer(OPEN_DATE - SEND_DATE)) %>%
  mutate(DIFF_CLICK = as.integer(CLICK_DATE - SEND_DATE)) %>%
  filter(is.na(DIFF_OPEN) | DIFF_OPEN < rate_window) %>%
  filter(is.na(DIFF_CLICK) | DIFF_CLICK < rate_window)

names(relevant_event) <- sapply(names(relevant_event), paste0, "_PREV")

# Si selezionano le date comprese nel periodo temporale definito
# e si integrano con gli eventi  rilevanti contenuti nel df target_event
# Si introducono gli attributi OPEN_RATE_PREV e CLICK_RATE_PREV
target_event_w_prev <- target_event %>% filter(SEND_DATE >= dt_start & SEND_DATE <= dt_end) %>%
  left_join(relevant_event
            , by = c("ID_CLI" = "ID_CLI_PREV")
  ) %>%
  filter(is.na(SEND_DATE_PREV) | (SEND_DATE_PREV < SEND_DATE & SEND_DATE <= SEND_DATE_PREV + prev_window)) %>%
  mutate(OPENED = if_else(OPEN_DATE_PREV <= SEND_DATE & SEND_DATE <= OPEN_DATE_PREV + prev_window, 1, 0)) %>%
  mutate(CLICKED = if_else(CLICK_DATE_PREV <= SEND_DATE & SEND_DATE <= CLICK_DATE_PREV + prev_window, 1, 0)) %>%
  mutate(FAILED = if_else(!is.na(ID_EVENT_F_PREV), 1, 0)) %>%
  group_by(ID_EVENT_S, ID_CLI, ID_CAMP, ID_DELIVERY, SEND_DATE,  TARGET) %>%
  summarize(NUM_SEND_PREV = n_distinct(ID_EVENT_S_PREV, na.rm = T)
            , NUM_OPEN_PREV = sum(OPENED, na.rm = T)
            , NUM_CLICK_PREV = sum(CLICKED, na.rm = T)
            , NUM_FAIL_PREV = sum(FAILED, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(OPEN_RATE_PREV = NUM_OPEN_PREV/NUM_SEND_PREV) %>%
  mutate(CLICK_RATE_PREV = NUM_CLICK_PREV/NUM_OPEN_PREV) %>%
  mutate(W_SEND_PREV = as.factor(NUM_SEND_PREV > 0)) %>%
  mutate(W_FAIL_PREV = as.factor(NUM_FAIL_PREV > 0)) %>%
  mutate(SEND_WEEKDAY = as.factor(weekdays(SEND_DATE))) %>%
  mutate(OPEN_RATE_PREV = if_else(is.na(OPEN_RATE_PREV), 0, OPEN_RATE_PREV)) %>%
  mutate(CLICK_RATE_PREV = if_else(is.na(CLICK_RATE_PREV), 0, CLICK_RATE_PREV))


# Si compone il DF finale (df_master) integrandolo con le informazioni contenute nei DF
# precedentemente definiti: 

df_master <- target_event_w_prev %>%
  left_join(df_1_cli_fid_clean %>%
              select(ID_CLI, ID_NEG, TYP_CLI_FID, COD_FID, STATUS_FID, FIRST_DT_ACTIVE, NUM_FIDs)
            , by = "ID_CLI") %>%
  filter(FIRST_DT_ACTIVE <= SEND_DATE) %>%
  mutate(AGE_FID = as.integer(SEND_DATE - FIRST_DT_ACTIVE)) %>%
  left_join(df_2_cli_account_clean
            , by = "ID_CLI") %>%
  left_join(df_3_cli_address_clean %>%
              select(ID_ADDRESS, PRV, REGION)
            , by = "ID_ADDRESS") %>%
  left_join(df_4_cli_privacy_clean
            , by = "ID_CLI") %>%
  left_join(df_5_camp_cat_clean
            , by = "ID_CAMP") %>%
  mutate(PRV = fct_explicit_na(PRV)) %>%
  mutate(REGION = fct_explicit_na(REGION)) %>%
  select(-ID_ADDRESS, -ID_DELIVERY, -SEND_DATE, -FIRST_DT_ACTIVE)

# Controllo per duplicati
df_master %>%
  group_by(ID_EVENT_S) %>% 
  summarize(num = n()) %>% 
  group_by(num) %>%
  count()



# Valore di target rispetto a numero di eventi sul totale
ggplot(df_master, aes(x = TARGET)) + geom_bar()

# Numero di eventi con relativo open rate
df_master %>%
  group_by(TARGET,  W_SEND_PREV) %>%
  summarize(NUM_EVENTs = n_distinct(ID_EVENT_S), mean_OR = mean(OPEN_RATE_PREV, na.rm = T))

str(df_master)
summary(df_master)

# Si decide di effetuare il binning dei seguenti attributi per favorire un migliore apprendimento
# e previsione dei dati con i modelli implementati successivamente

# Binning tempo fedeltà
ggplot(df_master, aes(x=AGE_FID)) + geom_bar()
v <- c(0, 30, 120, 240, +Inf)
names <- c("Newborn", "Kindergarten", "Junior", "Senior")
df_master <- df_master %>%
  mutate(AGE_FID_BIN = cut(AGE_FID, breaks = v, labels = names))

# Binning Regione

# Si dividono le regioni in Nord, Centro e Sud a causa di una discrepanza tra le medie
# di interazione con le mail inviate.

nord = c("FRIULI VENEZIA GIULIA", "LOMBARDIA", "VENETO", "PIEMONTE", "LIGURIA", "TRENTINO ALTO ADIGE", "VALLE D'AOSTA")
centro = c("LAZIO", "EMILIA ROMAGNA", "TOSCANA", "ABRUZZO", "MARCHE", "UMBRIA")
sud = c("PUGLIA", "MOLISE", "SICILIA", "SARDEGNA", "CALABRIA", "BASILICATA", "CAMPANIA")
mis = c("(Missing)")

REGION_N <- ifelse(df_master$REGION %in% nord, "NORD", ifelse(df_master$REGION %in% centro, "CENTRO", ifelse(df_master$REGION %in% sud, "SUD", "MISSING")))
TYP_CAMP <- ifelse(df_master$TYP_CAMP == "LOCAL", 1, ifelse(df_master$REGION=="NATIONAL" , 2, ifelse(df_master$REGION=="NEWSLETTER", 3, ifelse(df_master$REGION=="PERSONALIZED", 4, 5))))
df_master <- df_master %>%
  mutate(REGION_N = REGION_N,
         TYP_CAMP = TYP_CAMP)

df_temp <- df_master %>%
  select("REGION_N", "OPEN_RATE_PREV", "CLICK_RATE_PREV") %>%
  group_by(REGION_N) %>%
  summarise(media_open = mean(OPEN_RATE_PREV), media_click = mean(CLICK_RATE_PREV))

ggplot(df_temp, aes(x = REGION_N, y = media_open, fill = REGION_N)) + 
  geom_bar(stat="identity") + 
  xlab("ZONA") + ylab("MEDIA APERTURA MAIL")

ggplot(df_temp, aes(x = REGION_N, y = media_click, fill = REGION_N)) + 
  geom_bar(stat="identity") + 
  xlab("ZONA") + ylab("MEDIA CLICK MAIL")

# Missing Replacement

# Numero di missing per colonna
colSums(is.na(df_master))

# Eliminazione delle osservazioni del DF che hanno come email_provider NA 
# per cui le mail non sono mai state recapitate (failure); 
# gli email_provider che hanno valore NA,
# ma possiedono degli eventi di interazione (non failure), sono mantenuti con valore "others".

temp <- df_master[which(is.na(df_master$EMAIL_PROVIDER_CLEAN)),]
temp <- temp %>%
  filter(NUM_FAIL_PREV == NUM_SEND_PREV)

df_master <- setdiff(df_master, temp)
df_master <- df_master %>%
  mutate(EMAIL_PROVIDER_CLEAN = fct_explicit_na(EMAIL_PROVIDER_CLEAN, "others"))

# Tolti TYP_JOB, PRV e W_PHONE:
# Type job perchè al 97% è NA
# PRV perchè si tiene conto solo della regione
# W_PHONE in quanto non utile allo scopo

df_master <- df_master %>%
  select(-c("TYP_JOB", "PRV", "W_PHONE"))

#########################################################################################################################################################

# CORRELAZIONE ATTRIBUTI
# Si utilizza un DF ausiliario per il calcolo della correlazione tra gli attributi

# Vengono cambiati quindi tutti gli attributi utili in numerici


df_corr<- df_master %>% mutate(SEND_WEEKDAY=as.numeric(SEND_WEEKDAY),COD_FID=as.numeric(COD_FID),
                               EMAIL_PROVIDER_CLEAN=as.numeric(EMAIL_PROVIDER_CLEAN),
                               AGE_FID_BIN=as.numeric(AGE_FID_BIN))

TYP_CAMP_NUM <- ifelse(df_corr$TYP_CAMP == "LOCAL", 1, ifelse(df_corr$REGION=="NATIONAL" , 2,
                                                       ifelse(df_corr$REGION=="NEWSLETTER", 3,
                                                       ifelse(df_corr$REGION=="PERSONALIZED", 4, 5))))

REGION_N_NUM <-  ifelse(df_corr$REGION %in% nord, 1, ifelse(df_corr$REGION %in% centro, 2, ifelse(df_corr$REGION %in% sud, 3, -1)))



df_corr <- df_corr %>%
  select(-c("REGION", "TARGET", "ID_EVENT_S", "ID_CLI", "W_SEND_PREV", 
            "W_FAIL_PREV", "FLAG_PRIVACY_1", "FLAG_PRIVACY_2",
            "FLAG_DIRECT_MKT", "NUM_FIDs", "AGE_FID")) %>%
  mutate(ID_NEG = as.numeric(levels(ID_NEG)[ID_NEG]),
         TYP_CLI_FID = as.numeric(levels(TYP_CLI_FID)[TYP_CLI_FID]),
         STATUS_FID = as.numeric(levels(STATUS_FID)[STATUS_FID]),
         TYP_CAMP = TYP_CAMP_NUM,
         REGION_N = REGION_N_NUM)

# Heatmap che mostra la correlazione
ggplot(data = melt(round(cor(df_corr),2)), aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white") +
  geom_text(aes(Var1, Var2, label = value), color = "black", size = 4)

# Dalla matrice di correlazione si nota che "CLICK_RATE_PREV" è fortemente correlato a "NUM_CLICK_PREV",
# lo stesso vale per "OPEN_RATE_PREV" e "NUM_OPEN_PREV". Quindi si rimuovono le variabili NUM. 

##########################################################################################################################################


# Si osserva la struttura attuale del DF

str(df_master)
colSums(is.na(df_master))

# Si eliminano le varibili ritenute non significative per lo scopo in questione:
  # W_SEND_PREV/W_FAIL_PREV forniscono informazioni ridondanti 
  # FLAG_PRIVACY_1/2, FLAG_DIRECT_MKT e NUM_FIDs non sono rilevanti per l'obiettivo designato
  # TYP_CLI_FID perchè indica solamente se l'account utilizzato dal cliente è primario o no
  # TYP_CLI_ACCOUNT è ridondante in quanto le informazioni sono già contenute in "COD_FID"
  # STATUS_FID in quanto i valori sono per la quasi totalità "1"
  # ID_EVENT perchè è un id univoco 
ggplot(df_master, aes(x= as.factor(STATUS_FID))) + geom_bar()
  

df_master <- df_master %>%
  select(-c("W_SEND_PREV", "W_FAIL_PREV", "TYP_CLI_FID", "STATUS_FID",
            "TYP_CLI_ACCOUNT", "FLAG_PRIVACY_1", "FLAG_PRIVACY_2", "FLAG_DIRECT_MKT",
            "NUM_FIDs", "REGION", "AGE_FID", "ID_EVENT_S", "NUM_CLICK_PREV", "NUM_OPEN_PREV"))


# Si convertono in numerici gli attributi considerati rilevanti
AGE_FID_N <- ifelse(df_master$AGE_FID_BIN =="Newborn" , 1, ifelse(df_master$AGE_FID_BIN == "Kindergarten", 2, ifelse(df_master$AGE_FID_BIN == "Junior", 3, 4)))
df_master <- df_master %>%
  mutate(SEND_WEEKDAY = as.numeric(SEND_WEEKDAY),
         COD_FID = as.numeric(COD_FID),
         ID_NEG = as.numeric(ID_NEG),
         EMAIL_PROVIDER_CLEAN = as.numeric(EMAIL_PROVIDER_CLEAN),
         AGE_FID_BIN = as.numeric(AGE_FID_BIN),
         TYP_CAMP = as.numeric(TYP_CAMP),
         REGION_N = REGION_N_NUM)



#######################################################################################################################

# si calcola la proporzione della classe obiettivo, si nota che è fortemente 
# sbilanciata a favore dello "0"
class_embalance <- 1 - sum(df_master$TARGET == 1)/sum(df_master$TARGET == 0)


# Si procede in questo modo: si divide il DF in due parti, rispettivamente 10% (partizione B) 
# per la feature selection e 90% per il modello (partizione A).


righe_trainset <- createDataPartition(df_master$TARGET, p = 0.9, list=FALSE)
partition_A <- df_master[righe_trainset, ]  #90% per modello
partition_B <- df_master[-righe_trainset, ] #10% per la feature selection


# FEATURE SELECTION

# Si suddivide ulteriormente la partizione B in train e test da utilizzare per la feature selection.

righe_part_b <- createDataPartition(partition_B$TARGET, p = 0.8, list=FALSE)
train_b <- partition_B[righe_part_b, ]
test_b <- partition_B[-righe_part_b, ]

# Si effetua un Down Sample sulla variabile obiettivo (solo sul train) per ottenere un bilanciamento nella classe
# così da non essere influenzati negativamente dai dati e non cadere nel fenomeno dello ZeroR rule.
train_b <- downSample(x = train_b[, -ncol(train_b) + 11],
                      y = train_b$TARGET)

colnames(train_b)[length(train_b)] = "TARGET"


# Si adopera anche uno shuffle dei dati perchè alcuni modelli potrebbero apprendere anche dall'ordinamento
train_b = train_b[sample(nrow(train_b)), ]
rownames(train_b) = 1:nrow(train_b)

ggplot(train_b, aes(x=TARGET)) + geom_bar()


# Per la Feature Selection si sceglie di utilizzare un modello random forest


rf_model = train(TARGET ~ .,
                 data = train_b,
                 method = "rf",
                 ntree = 10,
                 trControl = trainControl(method = "none"))


# Importanza degli attributi per spiegare target:
varimp_rf <- varImp(rf_model)
plot(varimp_rf, main="Variable Importance with RF")

previsioni <- predict(rf_model, test_b)

confusionMatrix(reference = test_b$TARGET, data = previsioni, mode='everything', positive='1')

# A seguito di numerosi tentativi con svariate combinazioni di attributi 
# si è deciso di implementare il modello con le variabili OPEN_RATE_PREV e CLICK_RATE_PREV, 
# nonostante il grafico precedente metta in risalto altri attributi.


rf_model = train(TARGET ~ OPEN_RATE_PREV + CLICK_RATE_PREV + OPEN_RATE_PREV:CLICK_RATE_PREV,
                 data = train_b,
                 method = "rf",
                 ntree = 10,
                 trControl = trainControl(method = "none"))

varimp_rf <- varImp(rf_model)
plot(varimp_rf, main="Variable Importance with RF")


previsioni <- predict(rf_model, test_b)

confusionMatrix(reference = test_b$TARGET, data = previsioni, mode='everything', positive='1')


###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################


# IMPLEMENTAZIONE MODELLI SULLA PARTITION A

# Si utilizzano i seguenti modelli:
  # Random Forest
  # Logistic regression
  # SVM
  # Reti neurali
# Per i primi due modelli verrà utilizzata una cross validation,
# mentre per gli altri un semplice hold out con DownSampling sul train set;
# per la NN è stata utilizzata anche una porzione di DF come validation.

# RANDOM FOREST

ctrl <- trainControl(method = "cv", 
                     number = 5,
                     savePredictions = TRUE)

rf_model = train(TARGET ~ OPEN_RATE_PREV + CLICK_RATE_PREV + OPEN_RATE_PREV:CLICK_RATE_PREV,
                 data = partition_A,
                 method = "rf",
                 ntree = 10,
                 trControl = ctrl)

rf_model$results



# LOGISTIC REGRESSION

lreg <- train(TARGET ~ OPEN_RATE_PREV + CLICK_RATE_PREV + OPEN_RATE_PREV:CLICK_RATE_PREV,
              data = partition_A,
              method = "glm",
              family = binomial(),
              trControl = ctrl)


lreg$results

################################################################################################################################

# DownSampling per SVM e NN

righe_part_a <- createDataPartition(partition_A$TARGET, p = 0.8, list=FALSE)
train_a <- partition_A[righe_part_a, ]
test_a <- partition_A[-righe_part_a, ]
train_a <- downSample(x = train_a[, -3],
                      y = train_a$TARGET)
colnames(train_a)[length(train_a)] = "TARGET"
train_a = train_a[sample(nrow(train_a)), ]
rownames(train_a) = 1:nrow(train_a)


# SVM
# Per la SVM e NN vengono scelte anche le variabili "TYP_CAMP", "AGE_FID_BIN" e "REGION_N" perchè portano un contributo


# Il modello SVM non verrà eseguito per motivi di tempo e risorse, ma i risultati verranno presentati comunque
#
# train_a_svm <- train_a %>% 
#   mutate(TARGET = as.numeric(TARGET)) %>% 
#   select(OPEN_RATE_PREV, CLICK_RATE_PREV, REGION_N, AGE_FID_BIN, TYP_CAMP, TARGET)
# 
# svm_tuneLinear <- tune(svm, train.x = train_a_svm, train.y = as.matrix(train_a_svm$TARGET),
#                        kernel = "linear", ranges = list(cost=10^(-3:1), gamma = 10^(-5:-1)))
# svm_tuneLinear$best.parameters
# 
# svm_Model<-svm(as.factor(TARGET) ~ OPEN_RATE_PREV + CLICK_RATE_PREV + OPEN_RATE_PREV:CLICK_RATE_PREV + REGION_N + AGE_FID_BIN + TYP_CAMP,
#                data = train_a_svm, kernel = "linear", cost = 0.01, gamma = 1e-05)
# 
# previsioni <- predict(svm_Model, test_a)
# levels(previsioni) <- c(0,1)
# 
# 
# confusionMatrix(reference = test_a$TARGET, data = previsioni, mode='everything', positive='1')



# Rete neurale con keras


X_train <- train_a %>%
  select(c("OPEN_RATE_PREV", "CLICK_RATE_PREV", "TYP_CAMP", "AGE_FID_BIN", "REGION_N")) 
y_train <- to_categorical(train_a$TARGET)

X_test <- test_a %>%
  select(c("OPEN_RATE_PREV", "CLICK_RATE_PREV", "TYP_CAMP", "AGE_FID_BIN", "REGION_N")) 
y_test_cat <-  to_categorical(test_a$TARGET)

# Si crea una verione del test per calcolare la confusion matrix
y_test <- test_a$TARGET

X_train <- as.matrix(X_train)
y_train <- as.matrix(y_train)

X_test <- as.matrix(X_test)
y_test <- as.matrix(y_test)



model <- keras_model_sequential() 


# Dopo diverse prove il miglior compromesso tra risultato e velocità per la NN risulta essere:
model %>% 
  layer_dense(units = 36, activation = 'relu', input_shape = ncol(X_train)) %>% 
  layer_dense(units = 36, activation = 'relu') %>%
  layer_dense(units = 36, activation = 'relu') %>%
  layer_dense(units = 2, activation = 'sigmoid')


history <- model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy')
)

model %>% fit(
  X_train, y_train, 
  epochs = 10, 
  batch_size = 1000,
  verbose = 1,
  validation_split = 0.3
)


model %>% evaluate(X_test, y_test_cat)


# Confusion matrix 
prediction <- model %>% 
  predict_classes(X_test)

confusionMatrix(table(y_test, prediction), mode='everything', positive='1')






#############################################################################################################################################



# PROPENSITY TO CHURN

# Si carica il dataset relativo agli acquisti
df_fatture <- read.csv2(paste0(dir, "raw_7_tic.csv"), sep=";")
df_fatture$DATETIME <- as.Date(df_fatture$DATETIME)


# Si selezionano i clienti attivi perchè sono gli unici su cui si sceglie di sviluppare il modello
df_1_cli_fid_fidelizzazione <- df_1_cli_fid %>%
  select(ID_CLI, ID_NEG, COD_FID, DT_ACTIVE, STATUS_FID) %>%
  filter(STATUS_FID == 1) %>%
  distinct(ID_CLI, COD_FID, .keep_all = TRUE) %>%
  select(ID_CLI, COD_FID)

df_1_cli_fid <- df_1_cli_fid %>%
  select(ID_CLI, ID_NEG, DT_ACTIVE) %>%
  distinct(ID_CLI, .keep_all = TRUE) %>%
  left_join(df_1_cli_fid_fidelizzazione)

# Rimozione degli attributi considerati inutili
df_2_cli_account <- df_2_cli_account %>%
  select(-c(W_PHONE, TYP_JOB))


# Info spese dei clienti
df_fatture <- df_fatture %>%
  group_by(ID_SCONTRINO, ID_CLI) %>%
  mutate(SPESA_TOT_PER_ACQUISTO = sum(IMPORTO_LORDO))
df_fatture <- df_fatture %>%
  group_by(ID_CLI) %>%
  mutate(SPESA_TOT = sum(IMPORTO_LORDO))


# Numero resi e acquisti per cliente
df_fatture <- df_fatture %>%
  mutate(RESO_PRODOTTO = ifelse(DIREZIONE == -1, 1, 0))%>%
  group_by(ID_CLI) %>%
  mutate(N_RESI_PRODOTTI = sum(RESO_PRODOTTO))

df_fatture <- df_fatture %>%
  mutate(ACQUISTO_PRODOTTO = ifelse(DIREZIONE == 1, 1, 0))%>%
  group_by(ID_CLI) %>%
  mutate(N_ACQUISTO_PRODOTTI = sum(ACQUISTO_PRODOTTO))

# Si calcola il numero degli scontrini, ossia quante volte il cliente si è recato in un negozio
df_scontrini <- df_fatture %>%
  group_by(ID_CLI, DATETIME, DIREZIONE) %>%
  summarise(N_SCONTRINI = n()) %>%
  group_by(ID_CLI) %>%
  mutate(N_SCONTRINI = n()) %>%
  select(-c("DATETIME", "DIREZIONE")) %>%
  distinct(ID_CLI, .keep_all = TRUE)

df_res <- df_fatture %>%
  select(c("ID_CLI", "N_RESI_PRODOTTI", "N_ACQUISTO_PRODOTTI")) %>%
  unique()



# Join delle informazioni calcolate precedentemente in un unico DF (df_customer_info)
df_clienti <- df_fatture %>%
  group_by(ID_CLI) %>%
  select(ID_CLI, SPESA_TOT) %>%
  unique()

df_clienti_sconti <- df_fatture %>%
  group_by(ID_CLI) %>%
  summarise(SCONTO_TOT = sum(SCONTO)) %>%
  select(ID_CLI, SCONTO_TOT) %>%
  unique()

df_customer_info <- df_clienti %>%
  left_join(df_clienti_sconti) %>%
  mutate(PERC_DISC = round( SCONTO_TOT/SPESA_TOT, digits = 2)) %>%
  left_join(df_scontrini) %>%
  mutate(SPESA_MEDIA_LORDA = round(SPESA_TOT/N_SCONTRINI, digit = 2)) %>%
  mutate(SPESA_MEDIA_NETTA = round((SPESA_TOT-SCONTO_TOT)/N_SCONTRINI, digit = 2)) %>%
  right_join(df_1_cli_fid) %>%
  left_join(df_2_cli_account) %>%
  left_join(df_3_cli_address, by = "ID_ADDRESS") %>%
  distinct(ID_CLI, .keep_all = TRUE) %>%
  select(-c("PRV", "CAP", "EMAIL_PROVIDER", "ID_ADDRESS", "TYP_CLI_ACCOUNT")) %>%
  left_join(df_res)


# Binning Regione (come effettuato precedentemente)

REGION_N <- ifelse(df_customer_info$REGION %in% nord, 1, ifelse(df_customer_info$REGION %in% centro, 2, ifelse(df_customer_info$REGION %in% sud, 3, -1)))

df_customer_info <- cbind(df_customer_info, REGIONE = REGION_N) %>%
  select(-REGION)


# Calcolo tempi fidelizzazione e tempi medio per acquisto
FINE = max(df_fatture$DATETIME)
INIZIO = min(df_fatture$DATETIME)

df_customer_info <- df_customer_info %>%
  mutate(DT_ACTIVE = as.Date(DT_ACTIVE)) %>%
  mutate(AGE_FID = FINE - DT_ACTIVE) %>%
  mutate(T_MED_ACQ = round(as.numeric(FINE-INIZIO)/N_SCONTRINI , digit = 0))


# Si individua l'ultimo acquisto del cliente e le relative informazioni

df_ultimo <- df_fatture %>%
  group_by(ID_CLI) %>%
  distinct(ID_SCONTRINO, .keep_all = TRUE) %>%
  arrange(desc(DATETIME)) %>%
  distinct(ID_CLI, .keep_all = TRUE) %>%
  select(ID_CLI, DATETIME)

df_customer_info <- df_customer_info %>%
  left_join(df_ultimo) %>%
  mutate(ULTIMO_ACQUISTO = DATETIME) %>%
  select(-DATETIME)

# Binning durata fidelizzazione

df_customer_info <- df_customer_info %>%
  mutate(AGE_FID = as.numeric(AGE_FID), AGE_FID_BIN = cut(AGE_FID, breaks = v, labels = names))


# Si controllano gli NA e si procede con una missing replacement
colSums(is.na(df_customer_info))

# I clienti che non hanno mai acquistato
df_customer_info$T_MED_ACQ <- replace_na(df_customer_info$T_MED_ACQ, 99999) 

# Si sostituisce per i clienti di cui non si conosce il tipo di fidelizzazione il valore più frequente: STANDARD
df_customer_info$COD_FID <- replace_na(df_customer_info$COD_FID, "STANDARD")  
# I clienti che hanno NA sono quelli che si sono iscritti l'ultimo giorno dell'arco temporale considerato
df_customer_info$AGE_FID_BIN <- replace_na(df_customer_info$AGE_FID_BIN, "Newborn")

# Clienti che non hanno mai acquistato
df_customer_info$ULTIMO_ACQUISTO <- replace_na(df_customer_info$ULTIMO_ACQUISTO, "1900-01-01")  
# Gli altri Na verranno sistemati in seguito



# Si trasformano in attributi numerici: età e cod_fid_bin 
# Per perc_disc_tot vengono sostituiti i valori di infinito con 0
AGE_FID_N <- ifelse(df_customer_info$AGE_FID_BIN =="Newborn" , 1, ifelse(df_customer_info$AGE_FID_BIN == "Kindergarten", 2,
                                                                         ifelse(df_customer_info$AGE_FID_BIN == "Junior", 3, 4)))
COD_FID_BIN <- ifelse(df_customer_info$COD_FID =="STANDARD" , 1, ifelse(df_customer_info$COD_FID == "STANDARD BIZ", 2,
                                                                        ifelse(df_customer_info$COD_FID == "PREMIUM", 3, 4)))
PERC_DISC_TOT <- ifelse(is.infinite(df_customer_info$PERC_DISC =="Inf" | df_customer_info$PERC_DISC =="-Inf"),
                        0, df_customer_info$PERC_DISC)

df_customer_info <- cbind(df_customer_info, AGE_FID_N = AGE_FID_N)
df_customer_info <- cbind(df_customer_info, COD_FID_BIN = COD_FID_BIN)
df_customer_info <- cbind(df_customer_info, PERC_DISC_TOT = PERC_DISC_TOT)


# Si associa il valore standard 99999 ai clienti che hanno tempo da ultimo acquisto superiore a 370 giorni,
# che è impossibile in quanto il periodo temporale considerato è di 1 anno
df_customer_info <- df_customer_info %>%
  mutate(TEMPO_DA_ULITMO_ACQ = as.numeric(FINE - ULTIMO_ACQUISTO)) %>%
  mutate(TEMPO_DA_ULITMO_ACQ = ifelse(TEMPO_DA_ULITMO_ACQ > 370, 99999, TEMPO_DA_ULITMO_ACQ)) %>%
  select(-c("AGE_FID_BIN", "COD_FID", "DT_ACTIVE", "ULTIMO_ACQUISTO", "AGE_FID", "PERC_DISC"))


# Si divide il numero di volte che un cliente compra in trimestri
delta_t <- (FINE - INIZIO)/4

date_sep <- c(INIZIO, INIZIO + delta_t, INIZIO + 2*delta_t, INIZIO + 3*delta_t)


df_fatture$INTERVAL_1 <- df_fatture$DATETIME <= date_sep[2]
df_fatture$INTERVAL_2 <-  date_sep[2] < df_fatture$DATETIME & df_fatture$DATETIME <= date_sep[3]
df_fatture$INTERVAL_3 <-  date_sep[3] < df_fatture$DATETIME & df_fatture$DATETIME <= date_sep[4]
df_fatture$INTERVAL_4 <-  date_sep[4] < df_fatture$DATETIME


df_fatture <- df_fatture %>%
  group_by(ID_CLI, DATETIME, DIREZIONE) %>%
  distinct(ID_CLI, .keep_all = TRUE) %>%
  mutate(INTERVAL_1 = as.integer(as.logical(INTERVAL_1) * DIREZIONE),
         INTERVAL_2 = as.integer(as.logical(INTERVAL_2) * DIREZIONE),
         INTERVAL_3 = as.integer(as.logical(INTERVAL_3) * DIREZIONE),
         INTERVAL_4 = as.integer(as.logical(INTERVAL_4) * DIREZIONE)) 

# Si calcola il numero di acquisti e resi che il clente ha effettuato nei 4 trimestri
df_fatture <- df_fatture %>%
  group_by(ID_CLI) %>%
  summarise(INTERVAL_1_BUY = sum(INTERVAL_1[which(INTERVAL_1 > 0)]), INTERVAL_2_BUY = sum(INTERVAL_2[which(INTERVAL_2 > 0)]),
            INTERVAL_3_BUY = sum(INTERVAL_3[which(INTERVAL_3 > 0)]), INTERVAL_4_BUY = sum(INTERVAL_4[which(INTERVAL_4 > 0)]),
            INTERVAL_1_RES = sum(INTERVAL_1[which(INTERVAL_1 < 0 )]), INTERVAL_2_RES = sum(INTERVAL_2[which(INTERVAL_2 < 0)]),
            INTERVAL_3_RES = sum(INTERVAL_3[which(INTERVAL_3 < 0 )]), INTERVAL_4_RES = sum(INTERVAL_4[which(INTERVAL_4 < 0 )]))


df_customer_info <- df_customer_info %>%
  left_join(df_fatture, by = "ID_CLI")

colSums(is.na(df_customer_info))
sum(is.infinite(df_customer_info$PERC_DISC_TOT))

# Viene sistemata nuovamente la percentuale di sconto che ha subito modifiche a seguito del join
PERC_DISC_TOT <- ifelse(is.infinite(df_customer_info$PERC_DISC_TOT), 0, df_customer_info$PERC_DISC_TOT)

df_customer_info <- df_customer_info %>%
  select(- "PERC_DISC_TOT")
df_customer_info <- cbind(df_customer_info, PERC_DISC_TOT = PERC_DISC_TOT)

# Si associa il valore 0 ai clienti che non hanno mai effettuato acquisti e presentano valori NA
df_customer_info[is.na(df_customer_info)] <- 0 

df_churn <- df_customer_info


# Si calcola il DELAY per ogni cliente ossia la differenza
# tra il suo ultimo acquisto e un ammontare n di giorni;
# la dimensione di n viene stabilita in funzione della
# media di acquisto ed un "bonus", che viene ad essa sommato per concedere tempo extra al cliente.


df_churn_top1 <- df_churn %>%
  filter(T_MED_ACQ <= 14) %>%
  mutate(DELAY = (TEMPO_DA_ULITMO_ACQ - (T_MED_ACQ + 14)))


df_churn_top2 <- df_churn %>%
  filter(T_MED_ACQ > 14 & T_MED_ACQ <= 30) %>%
  mutate(DELAY = (TEMPO_DA_ULITMO_ACQ - (2 * T_MED_ACQ)))

df_churn_top3 <- df_churn %>%
  filter(T_MED_ACQ > 30 & T_MED_ACQ <= 60) %>%
  mutate(DELAY = (TEMPO_DA_ULITMO_ACQ - (T_MED_ACQ + T_MED_ACQ * 0.5)))

df_churn_top4 <- df_churn %>%
  filter(T_MED_ACQ > 60 & T_MED_ACQ <= 182) %>%
  mutate(DELAY = (TEMPO_DA_ULITMO_ACQ - (T_MED_ACQ + 30)))

# Per i clienti con tempo medio superiore a 182 giorni, cioè che hanno effettuato un solo acquisto,
# si valuta la spesa: un valore inferiore a 130 delinea un cliente occasionale e più portato al churn
df_churn_top5 <- df_churn %>%
  filter(T_MED_ACQ > 182) %>%
  mutate(DELAY = as.numeric(as.logical(SPESA_MEDIA_LORDA < 130 )))

df_churn <- rbind(df_churn_top1, df_churn_top2, df_churn_top3, df_churn_top4, df_churn_top5)
rm(df_churn_top1, df_churn_top2, df_churn_top3, df_churn_top4, df_churn_top5)

df_churn$CHURN <- 0

# Si determina, in funzione delle attività che ha effettuato nei trimestri precedenti, 
# il trend degli acquisti di ogni cliente con un modello di regressione lineare.



lin_coef <- function(a, b, c, d){
  x <- seq(1,4)
  y <- c(a, b, c, d)
  
  reg <- lm(y~x)
  
  return(reg$coefficients[[2]])
}

for (i in 1:nrow(df_churn)){
  df_churn$TREND[i] = round(lin_coef(df_churn$INTERVAL_1_BUY[i], df_churn$INTERVAL_2_BUY[i],
                                     df_churn$INTERVAL_3_BUY[i], df_churn$INTERVAL_4_BUY[i]), 2)
}

# Vengono normalizzati gli attributi DELAY e TREND
df_churn$TREND <- BBmisc::normalize(df_churn$TREND, method = "range", range = c(0,1))
df_churn$DELAY <- BBmisc::normalize(df_churn$DELAY, method = "range", range = c(0,1))

# Definizione del CHURN

# L'assegnazione della variabile CHURN è determinata come combinazione lineare delle variabili TREND e DELAY 
# con coefficienti diversi in funzione della categoria del cliente.
is_churn <- function(a, b, c, d){
  ris <- 0
  age <- a
  cod_fid <- b
  c <- 1-c
  if(age == 2) {
    if(cod_fid == 1) {
      ris <- 0.3 * c + 0.7 * d
    }
    else if(cod_fid == 2) {
      ris <- 0.5 * c + 0.5 * d
    }
    else if(cod_fid == 3) {
      ris <- 0.4 * c + 0.6 * d
    }
    else {
      ris <- 0.6 * c + 0.4 *d
    }
  }
  
  else if(age == 3) {
    if(cod_fid == 1) {
      ris <- 0.5 * c + 0.5 * d
    }
    else if(cod_fid == 2) {
      ris <- 0.6 * c + 0.4 * d
    }
    else if(cod_fid == 3) {
      ris <- 0.5 * c + 0.5 * d
    }
    else {
      ris <- 0.7 * c + 0.3 * d
    }
  }
  
  else if(age == 4) {
    if(cod_fid == 1) {
      ris <- 0.5 * c + 0.5 * d
    }
    else if(cod_fid == 2) {
      ris <- 0.4 * c + 0.6 * d
    }
    else if(cod_fid == 3) {
      ris <- 0.5 * c + 0.5 * d
    }
    else {
      ris <- 0.3 * c + 0.7 * d
    }
  }
  
  return(ris)
}


for (i in 1:nrow(df_churn)){
  df_churn$CHURN[i] = is_churn(df_churn$AGE_FID_N[i], df_churn$COD_FID_BIN[i],
                               df_churn$TREND[i], df_churn$DELAY[i])
}

df_churn$CHURN <- as.factor(as.numeric(as.logical(df_churn$CHURN > 0.5)))



# Inoltre si definiscono a priori alcune categorie particolari di clienti come CHURN o NON CHURN, indipendentemente 
# dal valore che viene restituito dalla funzione is_churn

# Clienti che hanno più resi che acquisti. CHURN
df_churn[which(df_churn$N_ACQUISTO_PRODOTTI <= df_churn$N_RESI_PRODOTTI), "CHURN"] <- 1 

# Utenti iscritti da meno di 1 mese che non hanno ancora effettuato acquisti. NO CHURN
df_churn[which(df_churn$INTERVAL_1_BUY == 0 & df_churn$INTERVAL_2_BUY == 0 & df_churn$INTERVAL_3_BUY == 0 & df_churn$INTERVAL_4_BUY == 0 & df_churn$AGE_FID_N == 1),"CHURN"] <- 0

# Utenti che sono iscitti da più di un mese e non hanno mai effettuato acquisti. CHURN
df_churn[which(df_churn$INTERVAL_1_BUY == 0 & df_churn$INTERVAL_2_BUY == 0 & df_churn$INTERVAL_3_BUY == 0 &  df_churn$INTERVAL_4_BUY == 0 & df_churn$AGE_FID_N != 1),"CHURN"] <- 1

# Clienti che hanno più acquisti che resi, ma una spesa totale negativa. CHURN
df_churn[which(df_churn$N_ACQUISTO_PRODOTTI >= df_churn$N_RESI_PRODOTTI & df_churn$SPESA_TOT < 0), "CHURN"] <- 1 

# Per i clienti con tempo medio superiore a 182 giorni, cioè che hanno effettuato un solo acquisto,
# si valuta la spesa: un valore inferiore a 130 delinea un clente occasionale e più portato al churn
df_churn[which(df_churn$T_MED_ACQ > 182 & df_churn$SPESA_MEDIA_LORDA < 130), "CHURN"] <- 1
df_churn[which(df_churn$T_MED_ACQ > 182 & df_churn$SPESA_MEDIA_LORDA >= 130), "CHURN"] <- 0


# Si evidenzia la spoporzione della variabile CHURN

ggplot(df_churn, aes(x = CHURN, fill=CHURN)) + geom_bar() +
  geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2)

#############################################################################################################################

# Heatmap che mostra la correlazione
# Si rimuovono le variabili INTERVAL_X_BUY che sono state utilizzate per calcolare il TREND 
# e le variabili INTERVAL_X_RES perchè è gia presente la somma.
df_churn <- df_churn %>%
  select(-c("INTERVAL_1_BUY", "INTERVAL_2_BUY", "INTERVAL_3_BUY", "INTERVAL_4_BUY",
            "INTERVAL_1_RES", "INTERVAL_2_RES", "INTERVAL_3_RES", "INTERVAL_4_RES"))


ggplot(data = melt(round(cor(df_churn[,-17]),2)), aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white") +
  geom_text(aes(Var1, Var2, label = value), color = "black", size = 3)


# Osservando la heatmap ed esaminando le variabili create (DELAY e TREND) si decide di considerare per i modelli
# le variabili: DELAY, TREND, SPESA_MEDIA_NETTA, N_RESI_PRODOTTI, AGE_FID_N, COD_FID_BIN E REGIONE.

df_churn <- df_churn %>%
  select(ID_CLI, DELAY, TREND, SPESA_MEDIA_NETTA, N_RESI_PRODOTTI, AGE_FID_N, COD_FID_BIN, REGIONE, CHURN)


# Previsioni

# Feature Selection
righe_part <- createDataPartition(df_churn$CHURN, p = 0.9, list = FALSE)
partition_A <- df_churn[righe_part, ]  #90% per modello
partition_B <- df_churn[-righe_part, ] #10% per la feature selection


righe_train <- createDataPartition(partition_B$CHURN, p = 0.8, list = FALSE)
train_b <- partition_B[righe_train, ] 
test_b <- partition_B[-righe_train, ]
train_b <- downSample(x = partition_B[, -ncol(train_b)],
                   y = partition_B$CHURN)

colnames(train_b)[length(train_b)] <- "CHURN"

train_b <- train_b[sample(nrow(train_b)), ]
rownames(train_b) <- 1:nrow(train_b)

ctrl <- trainControl(method = "none")

rf_model = train(CHURN ~ .,
                 data = train_b,
                 method = "rf",
                 ntree = 10,
                 trControl = ctrl)


# Importanza degli attributi per spiegare target:
varimp_rf <- varImp(rf_model)
plot(varimp_rf, main="Variable Importance with RF")

previsioni <- predict(rf_model, test_b)

confusionMatrix(reference = test_b$CHURN, data = previsioni, mode='everything', positive='1')

# Considerando anche la feature selection gli attributi utilizzati per i modelli sarrno: DELAY, TREND e SPESA_MEDIA_NETTA

# Implementazione modelli su partition_A

# RANDOM FOREST
ctrl <- trainControl(method = "cv", 
                     number = 5,
                     savePredictions = TRUE)

rf_model = train(CHURN ~ DELAY + TREND + SPESA_MEDIA_NETTA,
                 data = partition_A,
                 method = "rf",
                 ntree = 10,
                 trControl = ctrl)

rf_model$results



# LOGISTIC REGRESSION

lreg <- train(CHURN ~ DELAY + TREND + SPESA_MEDIA_NETTA,
              data = partition_A,
              method = "glm",
              family = binomial(),
              trControl = ctrl)


lreg$results


# Equal size sampling per SVM e NN

righe_part_a <- createDataPartition(partition_A$CHURN, p = 0.8, list=FALSE)
train_a <- partition_A[righe_part_a, ]
test_a <- partition_A[-righe_part_a, ]
train_a <- downSample(x = train_a[, -ncol(train_a)],
                      y = train_a$CHURN)
colnames(train_a)[length(train_a)] = "CHURN"
train_a = train_a[sample(nrow(train_a)), ]
rownames(train_a) = 1:nrow(train_a)

################################################################################################################################

# Il modello SVM non verrà eseguito per motivi di tempo e risorse, ma i risultati verranno presentati comunque
# train_a_svm <- train_a %>% 
#   mutate(CHURN = as.numeric(CHURN)) %>%
#  select(c("DELAY", "TREND", "SPESA_MEDIA_NETTA", "CHURN"))
# 
# svm_tuneLinear <- tune(svm, train.x = train_a_svm, train.y = as.matrix(train_a_svm$CHURN),
#                        kernel = "linear", ranges = list(cost=10^(-3:1), gamma = 10^(-5:-1)))
# svm_tuneLinear$best.parameters
# 
# svm_Model<-svm(as.factor(CHURN) ~ .,
#                data = train_a_svm, kernel = "linear", cost = svm_tuneLinear$best.parameters$cost, gamma = svm_tuneLinear$best.parameters$gamma)
# 
# previsioni <- predict(svm_Model, test_a)
# levels(previsioni) <- c(0,1)
# 
# 
# confusionMatrix(reference = test_a$TARGET, data = previsioni, mode='everything', positive='1')

################################################################################################################################


# Rete neurale


X_train <- train_a %>%
  select(c("DELAY", "TREND", "SPESA_MEDIA_NETTA")) 
y_train <- to_categorical(train_a$CHURN)

X_test <- test_a %>%
  select(c("DELAY", "TREND", "SPESA_MEDIA_NETTA"))
X_test <- X_test[,-1]
y_test_cat <-  to_categorical(test_a$CHURN)


# Si crea una verione del test per calcolare la confusion matrix
y_test <- test_a$CHURN

X_train <- as.matrix(X_train)
y_train <- as.matrix(y_train)

X_test <- as.matrix(X_test)
y_test <- as.matrix(y_test)



model <- keras_model_sequential() 


# dopo diverse prove il miglior compromesso tra risultato e velocità per la NN risulta così:
model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = ncol(X_train)) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 2, activation = 'sigmoid')


history <- model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy')
)

model %>% fit(
  X_train, y_train, 
  epochs = 30, 
  batch_size = 100,
  verbose = 1,
  validation_split = 0.3
)


model %>% evaluate(X_test, y_test_cat)

prediction <- model %>% 
  predict_classes(X_test)

confusionMatrix(table(y_test, prediction), mode='everything', positive='1')


