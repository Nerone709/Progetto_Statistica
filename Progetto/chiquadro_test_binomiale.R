#Carico le librerie
library(dplyr)

#Carico il csv
data <- read.csv("dataset_filtraggio_finale2.csv", sep = ",")

#Eliminazione duplicati
data_no_dup <- data %>% distinct()

#Estrazione delle colonne
colum_norm <- data_no_dup$Crypto

#Calcolo osservazioni assolute relative a intervallo 0 e intervallo 1
obs_interval <- table(colum_norm)
obs_interval

#Calcolo numero osservazioni totali della distribuzione
n <- length(colum_norm)
n

#Generazione intervalli per il test del chiquadro
#Proprietà da rispettare: Sommatoria delle probabilità che uno degli elementi sia pari a uno
#Numero di osservazioni attese per ogni intervallo >= 5
min(n * round(obs_interval[1]/n, digits = 3), n * 1-round(obs_interval[1]/n, digits = 3))
intervals <- numeric(2)
intervals[1] <- round(obs_interval[1]/n, digits = 3)
intervals[2] <- 1 - intervals[1]
intervals

#Generazione osservazioni per ogni intervallo 
n_obs_int <- numeric(2)
n_obs_int[1] <- length(which(colum_norm < intervals[1]))
n_obs_int[2] <- length(which(colum_norm > intervals[1]))

n_obs_int
sum(n_obs_int)

#Calcolo chiquadro
chi2 <- sum(((n_obs_int - n * intervals)/sqrt(n*intervals)) ^ 2)
chi2

#Calcolo intervallo accettazione ipotesi Nulla H_0:col_norm ha funzione di distribuzione normale
#Ipotesi alternativa H_1:col_norm non ha funzione di distribuzione normale
num_intervalli <- 2
k <- 0
alpha <- 0.05
first <- qchisq(alpha/2, df = num_intervalli - k - 1)
last <- qchisq(1 - alpha/2, df = num_intervalli - k - 1)

first 
last

