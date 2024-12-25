#Carica ggplot2, crayon, polycor e dplyr
library(ggplot2)
library(crayon)
library(polycor)
library(dplyr)
library(reshape2)

# Carico il dataset
dataset <- read.csv("Phishing_URL_Dataset_4.csv", sep=';')

#Ricavo il nuovo dataset eliminando le colonne ricavate
new_dataset <- dataset %>%
  select(-CharContinuationRate, -URLTitleMatchScore, -URLCharProb, -TLDLegitimateProb)

#Verifico il nuovo dataset
print(new_dataset)

# Calcola la varianza delle colonne
variances <- apply(new_dataset, 2, var)

#Media delle varianze
mean_variance <- mean(variances)

# Elimino le colonne con varianza maggiore della media
filtered_dataset <- new_dataset %>%
  select(which(variances <= mean_variance))

print(filtered_dataset)

# Converto tutte le colonne di tipo `character` in `factor`
filtered_dataset <- filtered_dataset %>%
mutate(across(where(is.character), as.factor))

# Conto i valori mancanti in ciascuna colonna
colSums(is.na(filtered_dataset))

# Calcolo la deviazione standard per ogni colonna (considerando solo valori numerici)
std_devs <- apply(filtered_dataset, 2, function(x) sd(as.numeric(x), na.rm = TRUE))

# Identifico le colonne con deviazione standard pari a zero
zero_std_columns <- names(std_devs[std_devs == 0])

# Rimuovo le colonne con deviazione standard pari a zero
filtered_dataset <- filtered_dataset[, !names(filtered_dataset) %in% zero_std_columns]

# Verifico che le colonne siano state rimosse
print(zero_std_columns)  # Mostra le colonne rimosse


#elimino le colonne inutili
filtered_dataset <- na.omit(filtered_dataset)

#Per le variabili numeriche, sostituisco i NA con la mediana
filtered_dataset <- filtered_dataset %>%
mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

#Per le variabili categoriche, sostituisco i NA con la modalità
filtered_dataset <- filtered_dataset %>%
mutate(across(where(is.factor), ~ ifelse(is.na(.), Mode(.), .)))

#Funzione per calcolare la modalità (il valore più frequente)
Mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

print(ncol(filtered_dataset))

#Ricavo la matrice di correlazione ed effettuo la stampa
correlation_matrix <- hetcor(filtered_dataset)

# Estraggo la matrice di correlazione dal risultato di hetcor
cor_matrix <- correlation_matrix$cor

# Converto la matrice in formato "long" (necessario per ggplot)
cor_matrix_long <- melt(cor_matrix)

# Creo la heatmap
ggplot(cor_matrix_long, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +  # Aggiunge i valori numerici
  theme_minimal() +
  xlab("Variabili") +
  ylab("Variabili") +
  ggtitle("Heatmap della Matrice di Correlazione con Valori") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Visualizzo il grafico delle varianze
ggplot(variance_df, aes(x = reorder(Column, Variance), y = Variance)) +
  geom_point(color = "blue", size = 3) +
  coord_flip() +
  xlab("Colonne") +
  ylab("Varianza") +
  ggtitle("Varianza delle colonne ordinate dal più basso al più alto")
