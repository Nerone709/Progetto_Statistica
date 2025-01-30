#Carico le librerie
library(dplyr)

#Carico il csv
data <- read.csv("dataset_filtraggio_finale2.csv", sep = ",")

#Eliminazione duplicati
data_no_dup <- data %>% distinct()

#Estrazione delle colonne
colum_norm <- data_no_dup$DomainLength

#Calcolo osservazioni assolute relative a intervallo 0 e intervallo 1
obs_interval <- table(colum_norm)
obs_interval

# Stima parametro lambda
lambda <- mean(colum_norm)

#Calcolo numero osservazioni totali della distribuzione
n <- length(colum_norm)
n

#Generazione probabilità intervalli
p <- numeric(5)
p[1]<-dpois(4, lambda)
p[2]<-dpois(5, lambda)
p[3]<-dpois(6, lambda)
p[4]<-dpois(7, lambda)
p[5]<-1-p[1]-p[2]-p[3]-p[4]
p

#Generazione osservazioni per ogni intervallo 
n_obs_int <- numeric(5)
n_obs_int[1] <- length(which(colum_norm == 4 ))
n_obs_int[2] <- length(which(colum_norm  == 5))
n_obs_int[3] <- length(which(colum_norm == 6))
n_obs_int[4] <- length(which(colum_norm == 7))
n_obs_int[5] <- length(which(colum_norm > 7))

n_obs_int
sum(n_obs_int)

#Calcolo chiquadro
chi2 <- sum(((n_obs_int - n * p)/sqrt(n*p)) ^ 2)
chi2

#Calcolo intervallo accettazione ipotesi Nulla H_0:col_norm ha funzione di distribuzione normale
#Ipotesi alternativa H_1:col_norm non ha funzione di distribuzione normale
num_intervalli <- 5
k <- 1
alpha <- 0.05
first <- qchisq(alpha/2, df = num_intervalli - k - 1)
last <- qchisq(1 - alpha/2, df = num_intervalli - k - 1)

first 
last

