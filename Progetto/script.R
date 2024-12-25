# Carica ggplot2, crayon, polycor e dplyr
library(ggplot2)
library(crayon)
library(polycor)
library(dplyr)
library(reshape2)

# Carico il dataset
dataset <- read.csv("Phishing_URL_Dataset_4.csv", sep=';')

# Ricavo il nuovo dataset eliminando le colonne ricavate
new_dataset <- dataset %>%
  select(-CharContinuationRate, -URLTitleMatchScore, -URLCharProb, -TLDLegitimateProb)

# Numero di colonne numeriche originali
numeric_cols <- new_dataset %>% select(where(is.numeric))
num_original_numeric_cols <- ncol(numeric_cols)

# Calcolo la varianza
variances <- apply(numeric_cols, 2, var, na.rm = TRUE)

# Media delle varianze
mean_variance <- mean(variances, na.rm = TRUE)

# Calcolo la soglia come metà della media delle varianze
threshold_variance <- mean_variance / 2

# Filtro le colonne con varianza <= soglia
filtered_numeric_cols <- numeric_cols %>%
  select(which(variances <= threshold_variance))

# Numero di colonne numeriche rimaste
num_filtered_numeric_cols <- ncol(filtered_numeric_cols)

# Numero di colonne numeriche rimosse
num_removed_numeric_cols <- num_original_numeric_cols - num_filtered_numeric_cols
print(paste("Numero di colonne numeriche rimosse con varianza <= soglia:", num_removed_numeric_cols))

# Converto tutte le colonne di tipo `character` in `factor`
filtered_dataset <- new_dataset %>%
  mutate(across(where(is.character), as.factor))

# Combina dati numerici filtrati e categorici
filtered_dataset <- filtered_dataset %>%
  select(names(filtered_numeric_cols), where(is.factor))

# Calcolo la matrice di correlazione
correlation_matrix <- hetcor(filtered_dataset)

# Estraggo la matrice di correlazione dal risultato di hetcor
cor_matrix <- correlation_matrix$cor

# Converto la matrice in formato "long"
cor_matrix_long <- melt(cor_matrix)

# Creo l'heatmap
ggplot(cor_matrix_long, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +  # Aggiunge i valori numerici
  theme_minimal() +
  xlab("Variabili") +
  ylab("Variabili") +
  ggtitle("Heatmap della Matrice di Correlazione") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
