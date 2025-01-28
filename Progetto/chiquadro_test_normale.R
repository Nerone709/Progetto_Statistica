#Carico le librerie
library(dplyr)

#Carico il csv
data <- read.csv("synthetic_dataset/gpt4o/dataset_sintetico.csv", sep = ",")

#Eliminazione duplicati
data_no_dup <- data %>% distinct()

#Estrazione delle colonne
colum_norm <- data_no_dup$NoOfEmptyRef

#Stimatore parametri non noti della distribuzione normale
media <- mean(colum_norm)
deviazione_standard <- sd(colum_norm)

#Calcolo numero osservazioni totali della distribuzione
n <- length(colum_norm)
n

#Generazione intervalli per il test del chiquadro
#Proprietà da rispettare: Sommatoria delle probabilità che uno degli elementi sia pari a uno
#Numero di osservazioni attese per ogni intervallo >= 5
intervals <- numeric(4)
for(i in 1:4)
{
  intervals[i] <- qnorm(0.2*i, mean = media, sd = deviazione_standard)
}

#Generazione osservazioni per ogni intervallo 
n_obs_int <- numeric(5)
n_obs_int[1] <- length(which(colum_norm < intervals[1]))
n_obs_int[2] <- length(which(colum_norm >= intervals[1] & colum_norm <= intervals[2]))
n_obs_int[3] <- length(which(colum_norm >= intervals[2] & colum_norm <= intervals[3]))
n_obs_int[4] <- length(which(colum_norm >= intervals[3] & colum_norm <= intervals[4]))
n_obs_int[5] <- length(which(colum_norm >= intervals[4]))

n_obs_int
sum(n_obs_int)

#Calcolo chiquadro
chi2 <- sum(((n_obs_int - n * 0.2)/sqrt(n*0.2)) ^ 2)
chi2

#Calcolo intervallo accettazione ipotesi Nulla H_0:col_norm ha funzione di distribuzione normale
#Ipotesi alternativa H_1:col_norm non ha funzione di distribuzione normale
num_intervalli <- 5
k <- 2
alpha <- 0.05
first <- qchisq(alpha/2, df = num_intervalli - k - 1)
last <- qchisq(1 - alpha/2, df = num_intervalli - k - 1)

first
last

#Rigettata ipotesi nulla -> kvalue: 17826.1 intervallo accettazione:  0.0009820691 e 5.023886 (Colonna HasFavicon)
