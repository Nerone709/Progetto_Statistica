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
plan(sequential)  # Disabilita la parallelizzazione per evitare problemi

# Carico il dataset
dataset <- fread("Phishing_URL_Dataset_4.csv", sep = ';')

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

# Verifica che il dataset filtrato contenga almeno due colonne
if (ncol(filtered_dataset) < 2) {
  stop("Il dataset filtrato contiene meno di due colonne.")
}

# Assicurati che il dataset sia un data frame
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

# Assegna i nomi alle righe e colonne
rownames(cor_matrix) <- colnames(filtered_dataset)
colnames(cor_matrix) <- colnames(filtered_dataset)

# Converto la matrice in formato "long" con reshape2::melt
cor_matrix_long <- reshape2::melt(cor_matrix)

# Assicurati che il dataset per il plot non sia vuoto
if (nrow(cor_matrix_long) == 0) {
  stop("Il dataset per il plot è vuoto.")
}

# Filtraggio per correlazioni significative
cor_matrix_long <- cor_matrix_long %>%
  filter(!is.na(value))  # Mantieni tutti i valori non-NA

# Converto i fattori in caratteri per la stampa corretta
cor_matrix_long$Var1 <- as.character(cor_matrix_long$Var1)
cor_matrix_long$Var2 <- as.character(cor_matrix_long$Var2)

# Creo l'heatmap
g <- ggplot(cor_matrix_long, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +  # Aggiunge i valori numerici
  theme_minimal() +
  xlab("Variabili") +
  ylab("Variabili") +
  ggtitle("Heatmap della Matrice di Correlazione iniziale") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Stampa il plot
print(g)


# Filtro la matrice long in base ai criteri specificati
cor_matrix_long_filtered <- cor_matrix_long %>%
  filter((value >= 0.70 & value > 0) | (value < -0.50 & value < 0))

# Assicurati che il dataset per il plot non sia vuoto
if (nrow(cor_matrix_long_filtered) == 0) {
  stop("Il dataset filtrato per il plot è vuoto.")
}

# Converto i fattori in caratteri per la stampa corretta
cor_matrix_long_filtered$Var1 <- as.character(cor_matrix_long_filtered$Var1)
cor_matrix_long_filtered$Var2 <- as.character(cor_matrix_long_filtered$Var2)

# Creo l'heatmap con la matrice filtrata
g_filtered <- ggplot(cor_matrix_long_filtered, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +  # Aggiunge i valori numerici
  theme_minimal() +
  xlab("Variabili") +
  ylab("Variabili") +
  ggtitle("Heatmap della Matrice di Correlazione Filtrata") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Stampa il plot
print(g_filtered)


#Eliminazione colonne con valore positivo >= 0.70, e valore negativo >= 0.50
dataset_corr <- filtered_numeric_cols %>%
  select(-URLSimilarityIndex, -URLLength, -SpacialCharRatioInURL, -ObfuscationRatio, -NoOfOtherSpecialCharsInURL, -NoOfObfuscatedChar, -NoOfLettersInURL, -NoOfDegitsInURL, -HasSocialNet, -HasObfuscation, -HasCopyrightInfo, -DomainTitleMatchScore, -DegitRatioInURL)

# Calcola la matrice di correlazione
cor_matrix <- cor(dataset_corr, use = "complete.obs")
melted_cor_matrix <- melt(cor_matrix)

# Crea l'heatmap con i numeri
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +  # Colore dei bordi delle celle
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlazione") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +  # Aggiunge i numeri
  theme_minimal() +  # Tema grafico pulito
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_fixed() +
  labs(title = "Heatmap della Matrice di Correlazione dopo aver eliminato i valori maggiori di 0.70 e minori di -0.50", x = "Variabili", y = "Variabili")

# Calcola la matrice di correlazione per dataset_corr
cor_matrix <- cor(dataset_corr, use = "complete.obs")

# Converto la matrice di correlazione in formato "long" per ggplot
melted_cor_matrix <- melt(cor_matrix)

# Filtro i valori con correlazione >= 0.60
melted_cor_matrix_filtered <- melted_cor_matrix %>%
  filter(abs(value) >= 0.60)  # Mantieni solo valori assoluti >= 0.60

# Controllo che il dataset filtrato non sia vuoto
if (nrow(melted_cor_matrix_filtered) == 0) {
  stop("Nessun valore di correlazione soddisfa i criteri di filtraggio (>= 0.60 o <= -0.60).")
}

# Creo l'heatmap per i valori filtrati
ggplot(data = melted_cor_matrix_filtered, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +  # Aggiungi bordi bianchi alle celle
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlazione") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +  # Mostra i numeri nella heatmap
  theme_minimal() +  # Applica un tema minimale
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 8),  # Ruota i nomi delle variabili
    axis.text.y = element_text(size = 8)  # Riduci dimensioni dei nomi sull'asse y
  ) +
  coord_fixed() +
  labs(
    title = "Heatmap della Matrice di Correlazione (|Correlazione| >= 0.60)",
    x = "Variabili",
    y = "Variabili"
  )




