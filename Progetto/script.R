# Carico il dataset
dataset <- read.csv("PhiUSIIL_Phishing_URL_Dataset_4.csv", sep=';')

# Trova le colonne con tutti valori zero
zero_columns <- names(dataset)[apply(dataset, 2, function(col) all(col == 0))]

# Verifica se ci sono colonne con tutti zeri
if (length(zero_columns) == 0) 
{
  print("Non ci sono colonne con tutti valori zero.")
}else 
  {
  # Plot di tutte le colonne con tutti zeri
  for (col in zero_columns) {
    plot(dataset[[col]], type='h', main=paste('Plot della colonna:', col), xlab='Indice', ylab='Valore')
  }
  }

# Ricavo le colonne desiderate
col_URL <- dataset$URL
col_DOMAIN <- dataset$Domain

# Seleziono le colonne desiderate
dati <- dataset[, c("URL", "Domain")]
dati$URL <- as.integer(dati$URL)
dati$Domain <- as.integer(dati$Domain)

# Crea un boxplot per le lunghezze degli URL e dei domini con colori specifici
boxplot(dati, main="Boxplot di URL e Domain", xlab="URL", ylab="Domain", col=c("blue", "red"))