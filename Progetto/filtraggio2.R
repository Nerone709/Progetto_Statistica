# Carica i pacchetti necessari
library(ggplot2)
library(crayon)
library(dplyr)
library(reshape2)
library(furrr)
library(polycor)
library(data.table)
library(psych)

# Configurazione per il lavoro in parallelo
plan(sequential)

# Carico il dataset
dataset <- fread("Phishing_URL_Dataset_4.csv", sep = ';')

# Ricavo il nuovo dataset eliminando le colonne ricavate (secondo la lettura del paper)
new_dataset <- dataset %>%
  select(-CharContinuationRate, -URLTitleMatchScore, -URLCharProb, -TLDLegitimateProb)

# Converto le colonne `character` in `factor`
filtered_dataset <- new_dataset %>%
  mutate(across(where(is.character), as.factor))

# Converto `factor` in numerico utilizzando l'indice dei livelli
filtered_dataset <- filtered_dataset %>%
  mutate(across(where(is.factor), as.numeric))

# Seleziono tutte le colonne numeriche per il calcolo della varianza
numeric_cols <- filtered_dataset %>% select(where(is.numeric))
num_original_numeric_cols <- ncol(numeric_cols)

# Calcolo della varianza
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

# Combino dati numerici filtrati
filtered_dataset <- filtered_dataset %>%
  select(names(filtered_numeric_cols), where(is.factor))

# Controllo che il dataset sia un data frame
filtered_dataset <- as.data.frame(filtered_dataset)

# Funzione per calcolare la matrice di correlazione mista
compute_correlation_matrix <- function(data) {
  cor_matrix <- matrix(NA, ncol(data), ncol(data))
  problematic_pairs <- list()
  for (i in 1:ncol(data)) {
    for (j in i:ncol(data)) {
      tryCatch({
        if (is.numeric(data[[i]]) && is.numeric(data[[j]])) {
          cor_matrix[i, j] <- cor(data[[i]], data[[j]], use = "pairwise.complete.obs")
        } else {
          cor_matrix[i, j] <- mixedCor(data[, c(i, j)], use = "pairwise.complete.obs")$rho[1, 2]
        }
        cor_matrix[j, i] <- cor_matrix[i, j]
      }, error = function(e) {
        message(paste("Could not compute correlation between variables", colnames(data)[i], "and", colnames(data)[j], ":", e$message))
        problematic_pairs <<- append(problematic_pairs, list(c(colnames(data)[i], colnames(data)[j])))
        cor_matrix[i, j] <- NA
        cor_matrix[j, i] <- NA
      })
    }
  }
  
  if (length(problematic_pairs) > 0) {
    message("Problematic variable pairs: ", paste(sapply(problematic_pairs, paste, collapse = " and "), collapse = "; "))
  }
  return(cor_matrix)
}

# Calcolo la matrice di correlazione
cor_matrix <- compute_correlation_matrix(filtered_dataset)

# Assegnazione dei nomi per righe e colonne
rownames(cor_matrix) <- colnames(filtered_dataset)
colnames(cor_matrix) <- colnames(filtered_dataset)

# Converto la matrice utilizzando reshape2::melt
melted_cor_matrix <- reshape2::melt(cor_matrix)
colnames(melted_cor_matrix) <- c("Var1", "Var2", "value")

# Filtraggio per correlazioni significative
melted_cor_matrix <- melted_cor_matrix %>%
  filter(!is.na(value))  # Mantengo tutti i valori non-NA

# Verifica che `melted_cor_matrix` non sia vuoto
if (nrow(melted_cor_matrix) == 0) {
  stop("Il dataset `melted_cor_matrix` è vuoto.")
}

# Creo l'heatmap 
g <- ggplot(melted_cor_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +  # Aggiunge i valori numerici
  theme_minimal() +
  xlab("Variabili") +
  ylab("Variabili") +
  ggtitle("Heatmap della Matrice di Correlazione iniziale") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Stampo il plot
print(g)

# Filtro la matrice long in base ai criteri specificati
melted_cor_matrix_filtered <- melted_cor_matrix %>%
  filter((value >= 0.60 & value > 0) | (value <= -0.50 & value < 0))

# Controllo che il dataset per il plot non sia vuoto
if (nrow(melted_cor_matrix_filtered) == 0) {
  stop("Il dataset filtrato per il plot è vuoto.")
}

# Creo il primo heatmap con la matrice filtrata
g_filtered <- ggplot(melted_cor_matrix_filtered, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +  # Aggiunge i valori numerici
  theme_minimal() +
  xlab("Variabili") +
  ylab("Variabili") +
  ggtitle("Heatmap della Matrice di Correlazione Filtrata per valori >= 0.50 e <= di -0.50") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Stampo il plot
print(g_filtered)

# Eliminazione colonne con valore positivo >= 0.60, e valore negativo <= -0.50
dataset_finale <- filtered_numeric_cols %>%
  select(-DegitRatioInURL, -Domain, -DomainTitleMatchScore, -FILENAME, -HasCopyrightInfo, 
         -HasDescription, -HasObfuscation, -HasSocialNet, -IsHTTPS, -LetterRatioInURL, -NoOfDegitsInURL,
         -NoOfEqualsInURL, -NoOfExternalRef, -NoOfLettersInURL, -NoOfObfuscatedChar, -NoOfOtherSpecialCharsInURL,
         -NoOfQMarkInURL, -NoOfSelfRef, -ObfuscationRatio, -SpacialCharRatioInURL, -Title, -URL, -URLLength, 
         -URLSimilarityIndex)

# Calcolo la matrice di correlazione
cor_matrix_finale <- cor(dataset_finale, use = "complete.obs")

# Converto la matrice di correlazione in formato "long" per ggplot
melted_cor_matrix_finale <- reshape2::melt(cor_matrix_finale)
colnames(melted_cor_matrix_finale) <- c("Var1", "Var2", "value")

# Creo la heatmap per il dataset finale
ggplot(data = melted_cor_matrix_finale, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +  # Aggiungi bordi bianchi alle celle
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white", 
    midpoint = 0, limit = c(-1, 1), space = "Lab", 
    name = "Correlazione"
  ) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +  # Mostra i numeri nella heatmap
  theme_minimal() +  # Applica un tema minimale
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 8),  # Ruota i nomi delle variabili
    axis.text.y = element_text(size = 8)  # Riduci dimensioni dei nomi sull'asse y
  ) +
  coord_fixed() +  # Mantieni le proporzioni
  labs(
    title = "Heatmap della Matrice di Correlazione (Dataset Finale filtraggio2 (>= 0.60 e <= -0.50))",
    x = "Variabili",
    y = "Variabili"
  )

# Salvo il dataset finale in formato CSV
write.csv(dataset_finale, " finale_filtraggio2.csv", row.names = FALSE)