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

# Verifico che tutte le colonne di tipo `character` siano state convertite
str(filtered_dataset)

# Ricavo la matrice di correlazione ed effettuo la stampa
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
variance_df <- data.frame(Column = names(variances), Variance = variances)
ggplot(variance_df, aes(x = reorder(Column, Variance), y = Variance)) +
  geom_point(color = "blue", size = 3) +
  coord_flip() +
  xlab("Colonne") +
  ylab("Varianza") +
  ggtitle("Varianza delle colonne ordinate dal più basso al più alto")