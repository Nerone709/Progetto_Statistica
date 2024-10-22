library (utils)

# Carico il dataset
dataset <- read.csv("PhiUSIIL_Phishing_URL_Dataset_4.csv", sep=';')

# Trova le colonne con tutti valori zero
zero_columns <- names(dataset)[apply(dataset, 2, function(col) all(col == 0))]

# Stampa i nomi delle colonne con tutti zeri
print(zero_columns)