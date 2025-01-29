# Caricamento delle librerie necessarie
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(qcc)

# Caricamento del dataset
data2 <- read.csv("synthetic_dataset/gpt4o/dataset_sintetico_finale.csv")

# Funzione per analizzare le singole colonne e successivamente le coppie di colonne
analyze_groups <- function(dataset, dataset_name) {
  columns <- colnames(dataset)
  num_columns <- ncol(dataset)
  
  # Creazione della directory di base
  base_dir <- "statistica_descrittiva_plot_sintetico"
  dataset_dir <- file.path(base_dir, paste0("plots_", dataset_name))
  if(!dir.exists(dataset_dir)) dir.create(dataset_dir, recursive = TRUE)
  
  # Analisi delle singole colonne
  for (col in columns) {
    cat("\nAnalizzando la colonna", col, "nel dataset", dataset_name, "\n")
    
    # Boxplot e boxplot a intaglio
    if (is.numeric(dataset[[col]])) {
      png(filename = file.path(dataset_dir, paste0("boxplot_", col, ".png")), width = 800, height = 600)
      p1 <- ggplot(dataset, aes_string(y = col)) +
        geom_boxplot() +
        labs(title = paste("Boxplot per", col)) +
        theme_minimal()
      print(p1)
      dev.off()
      
      png(filename = file.path(dataset_dir, paste0("boxplot_notch_", col, ".png")), width = 800, height = 600)
      p2 <- ggplot(dataset, aes_string(y = col)) +
        geom_boxplot(notch = TRUE) +
        labs(title = paste("Boxplot a intaglio per", col)) +
        theme_minimal()
      print(p2)
      dev.off()
    }
    
    # Istogramma
    if (is.numeric(dataset[[col]])) {
      png(filename = file.path(dataset_dir, paste0("histogram_", col, ".png")), width = 800, height = 600)
      p <- ggplot(dataset, aes_string(x = col)) +
        geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
        labs(title = paste("Istogramma per", col), x = col, y = "Frequenza") +
        theme_minimal()
      print(p)
      dev.off()
    }
    
    # Diagramma di Pareto per colonne numeriche
    if (is.numeric(dataset[[col]])) {
      counts <- table(cut(dataset[[col]], breaks = 10))
      png(filename = file.path(dataset_dir, paste0("pareto_", col, ".png")), width = 800, height = 600)
      pareto.chart(counts, main = paste("Diagramma di Pareto per", col))
      dev.off()
    }
  }
  
  # Analisi delle coppie di colonne numeriche per QQplot
  num_cols <- columns[sapply(dataset, is.numeric)]
  
  
  for (i in seq_along(num_cols)) {
    for (j in seq((i + 1), length(num_cols))) {
      col1 <- num_cols[i]
      col2 <- num_cols[j]
      
      # Verifica che entrambe le colonne abbiano almeno 2 valori validi
      valid_col1 <- na.omit(dataset[[col1]])
      valid_col2 <- na.omit(dataset[[col2]])
      
      if (length(valid_col1) >= 2 && length(valid_col2) >= 2) {
        png(filename = file.path(dataset_dir, paste0("qqplot_", col1, "_", col2, ".png")), width = 800, height = 600)
        qqplot(valid_col1, valid_col2,
               main = paste("QQplot tra", col1, "e", col2),
               xlab = col1, ylab = col2)
        abline(0, 1, col = "red")
        dev.off()
      } else {
        cat("\nSaltato QQplot tra", col1, "e", col2, "perché uno o entrambi i vettori hanno meno di 2 valori validi.\n")
      }
    }
  }
  
  # Analisi delle colonne binarie per QQplot (opzionale)
  binary_cols <- columns[sapply(dataset, function(x) all(x %in% c(0, 1)))]
  
  for (i in seq_along(binary_cols)) {
    for (j in seq((i + 1), length(binary_cols))) {
      col1 <- binary_cols[i]
      col2 <- binary_cols[j]
      
      # Evitare il QQplot per colonne binarie, poiché non ha significato
      cat("\nSaltato QQplot tra", col1, "e", col2, "perché sono colonne binarie.\n")
    }
  }
}

# Analisi del dataset
analyze_groups(data2, "dataset_sintetico_finale")
