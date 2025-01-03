#Carico librerie necessarie ad uso di shap e lime
library(ggplot2)
library(data.table)
library(tidyverse)
library(iml)
library(caret)

#Carico csv per l'explenability
data <- read.csv(" finale_filtraggio.csv")

#Converto la colonna label in factor
#data$label = as.factor(data$label)

#Carico modello KNN
model <- train(label ~., data = data, method = "knn")

#Preparo il modello con i dati agli algoritmi di explenability 
model_iml <- Predictor$new(model, data = data, y = "label")

#Uso spiegabilità del modello a livello globale
importance <- FeatureImp$new(model_iml, loss = "f1")

#Plotto le feature che hanno contribuito di più all' abbassamento della loss
p <- plot(importance)

#Salva il grafico 
ggsave("feature_importance.png", plot = p)