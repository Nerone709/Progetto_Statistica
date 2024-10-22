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