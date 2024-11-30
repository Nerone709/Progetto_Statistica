#Carica ggplot2, crayon, polycor e dplyr
library(ggplot2)
library(crayon)
library(polycor)
library(dplyr)
library(reshape2)

# Carico il dataset
dataset <- read.csv("PhiUSIIL_Phishing_URL_Dataset_4.csv", sep=';')



# Calcola la varianza delle colonne
variances <- apply(dataset, 2, var)

# Ordina le varianze in ordine crescente
sorted_variances <- sort(variances)

# Crea un data frame per il grafico
variance_df <- data.frame(Column = names(sorted_variances), Variance = sorted_variances)
print(variance_df)

# Converte tutte le colonne di tipo `character` in `factor`
dataset <- dataset %>%
mutate(across(where(is.character), as.factor))

# Conta i valori mancanti in ciascuna colonna
colSums(is.na(dataset))

# Calcola la deviazione standard per ogni colonna (considerando solo valori numerici)
std_devs <- apply(dataset, 2, function(x) sd(as.numeric(x), na.rm = TRUE))

# Identifica le colonne con deviazione standard pari a zero
zero_std_columns <- names(std_devs[std_devs == 0])

# Rimuovi le colonne con deviazione standard pari a zero
dataset <- dataset[, !names(dataset) %in% zero_std_columns]

# Verifica che le colonne siano state rimosse
print(zero_std_columns)  # Mostra le colonne rimosse


#elimina le colonne non utili
dataset <- na.omit(dataset)

#Per le variabili numeriche, puoi sostituire i NA con la mediana
dataset <- dataset %>%
mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

#Per le variabili categoriche, puoi sostituire i NA con la modalità
dataset <- dataset %>%
mutate(across(where(is.factor), ~ ifelse(is.na(.), Mode(.), .)))

#Funzione per calcolare la modalità (il valore più frequente)
Mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

#Ricavo la matrice di correlazione ed effettuo la stampa
correlation_matrix <- hetcor(dataset)

# Estrai la matrice di correlazione dal risultato di hetcor
cor_matrix <- correlation_matrix$cor

# Converte la matrice in formato "long" (necessario per ggplot)
cor_matrix_long <- melt(cor_matrix)

# Crea la heatmap
ggplot(cor_matrix_long, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +  # Aggiunge i valori numerici
  theme_minimal() +
  xlab("Variabili") +
  ylab("Variabili") +
  ggtitle("Heatmap della Matrice di Correlazione con Valori") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Visualizza il grafico delle varianze
ggplot(variance_df, aes(x = reorder(Column, Variance), y = Variance)) +
  geom_point(color = "blue", size = 3) +
  coord_flip() +
  xlab("Colonne") +
  ylab("Varianza") +
  ggtitle("Varianza delle colonne ordinate dal più basso al più alto")
