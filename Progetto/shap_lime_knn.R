#Carico librerie necessarie ad uso di shap e lime
library(ggplot2)
library(data.table)
library(tidyverse)
library(iml)
library(caret)
library(glmnet)

#Carico csv per l'explenability
data <- read.csv(" finale_filtraggio.csv", sep = ",")
data1 <- read.csv(" finale_filtraggio2.csv", sep = ",")

#Converto la colonna label in factor
#data$label = as.factor(data$label)

#Carico modello KNN
model <- train(label ~., data = data, method = "knn")
model1 <- train(label ~., data = data1, method = "knn")

#Preparo il modello con i dati agli algoritmi di explenability 
model_iml <- Predictor$new(model, data = data, y = "label")
model_iml1 <- Predictor$new(model1, data = data1, y = "label") 

#Uso spiegabilità del modello a livello globale
#importance <- FeatureImp$new(model_iml, loss = "f1")

#Plotto le feature che hanno contribuito di più all' abbassamento della loss
#p <- plot(importance)

#Salva il grafico 
#ggsave("feature_importance.png", plot = p)

#Generazioni 10 osservazioni casuali da spiegare
random_index_obs <- sample(1:nrow(data), size = 10, replace = FALSE)

#Ciclo per estrarre e spiegare le 10 osservazioni casuali (usando lime e shap values)
for(i in random_index_obs)
{
  lime <- LocalModel$new(model_iml, x.interest = data[i,])
  lime1 <- LocalModel$new(model_iml1, x.interest = data1[i,])
  shap_explainer <- Shapley$new(model_iml, x.interest = data[i,], sample.size = 1000)
  shap_explainer1 <- Shapley$new(model_iml1, x.interest = data1[i,], sample.size = 1000)
  
  lime_p <- plot(lime)
  lime_p1 <- plot(lime1)
  ggsave(paste0("iml_plot/lime/","lime_obs", i, ".png"), plot = lime_p)
  ggsave(paste0("iml_plot/lime/", "lime1_obs", i, ".png"), plot = lime_p1)
  
  shap_p <- shap_explainer$plot()
  shap_p1 <- shap_explainer1$plot()
  ggsave(paste0("iml_plot/shap/", "shap_obs", i,".png"), plot = shap_p)
  ggsave(paste0("iml_plot/shap/" , "shap1_obs", i,".png"), plot = shap_p1)
}

