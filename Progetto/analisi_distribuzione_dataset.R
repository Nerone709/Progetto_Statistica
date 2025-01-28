# Carico le librerie
library(ggplot2)
library(moments)
library(tidyverse)
library (DescTools)

data <- read.csv("synthetic_dataset/gpt4o/dataset_sintetico.csv", sep = ",")

#Analisi indici di sintesi per capire simmetria
summary(data)

for(i in 1:ncol(data))
{
  mod_val <- Mode(data[, i])
  cat(i, ": ", mod_val, "\n")
}

# Funzione per filtrare le colonne con media non nulla
filter_columns_with_nonzero_mean <- function(data) 
  {
  
  # Calcola la media per ogni colonna numerica
  means <- sapply(data, function(col) {
    if (is.numeric(col)) {
      mean(col, na.rm = TRUE)
    } else {
      NA  # Se la colonna non è numerica, restituisci NA
    }
  })
  
  # Filtra le colonne con media diversa da zero (escludendo NA)
  selected_columns <- names(means)[!is.na(means) & means != 0]
  
  # Seleziona solo le colonne con media non nulla
  filtered_data <- data[, selected_columns, drop = FALSE]
  return(filtered_data)
}

# Applica la funzione al dataset
filtered_data <- filter_columns_with_nonzero_mean(data)

filtered_data_final <- filtered_data %>%
    select(-NoOfAmpersandInURL, -NoOfSelfRedirect, -HasExternalFormSubmit, -Crypto)


for(i in 1:ncol(filtered_data_final))
{
  cv_val <- CoefVar(filtered_data_final[, i])
  cat(i, ": ", cv_val , "\n")
}


for(i in 1:ncol(data))
{
  sk_val <- Skew(data[, i])
  kurt_val <- Kurt(data[, i])
  cat(i, " Skewness value: ", sk_val , "\n")
  cat(i, "Kurtosis value: ", kurt_val, "\n")
}


#for(i in 1:ncol(filtered_data_final))
#{
#  moda <- function(x)
#  {
#    ux <-unique(x)
#    ux[which.max(tabulate(match(x, ux)))]
#    plot_filtered <- ggplot()
#  }
#}

