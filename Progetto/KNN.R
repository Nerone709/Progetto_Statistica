#Carico librerie necessarie
library(ggplot2)
library(tidyverse)
library(glmnet)
library(caret)
library(dplyr)

#Carico il csv con i dati
data <- read.csv("dataset_filtraggio_finale.csv", sep = ",")
data_no_dup <- data %>% distinct()

#Conversione della colonna label a factor per la classificazione binaria
data_no_dup$label <- factor(data_no_dup$label, levels = c("0", "1"))

#Splitting in train e testset
trainIndex <- createDataPartition(y = data_no_dup$label, p = 0.8, list = FALSE)
train <- data_no_dup[trainIndex, ]
testing <- data_no_dup[-trainIndex, ]

#DATA PRE-PROCESSING APPLICAZIONE SCALING PIU' CENTERING DEI DATI
pre_processor_values <- preProcess(train, method = c("center", "scale"))
train_pre_processed <- predict(pre_processor_values, train)
testing_pre_processed <- predict(pre_processor_values, testing)

#Train and tuning del modello KNN
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 4)
knn_model <- train(label ~., data = train_pre_processed, method = "knn", trControl = trctrl, tuneGrid = data.frame(k = c(3, 5, 7, 9, 11, 13)))
knn_model

#Generazione delle predizioni del modello addestrato
test_pred <- predict(knn_model, newdata = testing_pre_processed)

#Valutazione performance su testset tramite metodo di caret confusion matrix
cm <- confusionMatrix(test_pred, testing_pre_processed$label, mode = "everything")

#Plotting della confusion Matrix
cm_to_plot <- as.data.frame(cm$table)
ggplot(cm_to_plot, aes(Prediction, Reference, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq)) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "phishing", y = "nophishing") +
  scale_x_discrete(labels = c("Classe 0", "Classe 1")) +
  scale_y_discrete(labels = c("Classe 1", "Classe 0"))

