#Carico librerie necessarie
library(ggplot2)
library(tidyverse)
library(glmnet)
library(caret)
library(dplyr)
library(ROCR)

#Carico il csv con i dati, scelto dataset di filtraggio.R
data1 <- read.csv("synthetic_dataset/gpt4o/dataset_sintetico.csv", sep = ",")
data_no_dup1 <- data1 %>% distinct()

#Conversione della colonna label a factor per la classificazione binaria
data_no_dup1$label <- factor(data_no_dup1$label, levels = c("0", "1"))

#Splitting in train e testset
trainIndex1 <- createDataPartition(y = data_no_dup1$label, p = 0.8, list = FALSE)
train1 <- data_no_dup1[trainIndex1, ]
testing1 <- data_no_dup1[-trainIndex1, ]

#Data Pre-processing applicazione scaling piu' centering dei dati
pre_processor_values1 <- preProcess(train1, method = c("center", "scale"))
train_pre_processed1 <- predict(pre_processor_values1, train1)
testing_pre_processed1 <- predict(pre_processor_values1, testing1)

#Train and tuning del modello KNN
trctrl1 <- trainControl(method = "repeatedcv", number = 10, repeats = 4)
knn_model1 <- train(label ~., data = train_pre_processed1, method = "knn", trControl = trctrl1, tuneGrid = data.frame(k = c(3, 5, 7, 9, 11, 13)))
knn_model1

#Generazione delle predizioni del modello addestrato
test_pred1 <- predict(knn_model1, newdata = testing_pre_processed1)

#Valutazione performance su testset tramite metodo di caret confusion matrix
cm1 <- confusionMatrix(test_pred1, testing_pre_processed1$label, mode = "everything")
cm1

#Plotting della confusion Matrix
cm1_to_plot  <- as.data.frame(cm1$table)
plot_cm1 <- ggplot(cm1_to_plot, aes(Prediction, Reference, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq)) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "phishing", y = "nophishing") +
  scale_x_discrete(labels = c("Classe 0", "Classe 1")) +
  scale_y_discrete(labels = c("Classe 0", "Classe 1")) 

ggsave(paste0("model_performance_plot/", "confusionmatrix_k3_filtraggio_sintetico.png"), plot = plot_cm1)


#Conversione a vettore numerico delle predizioni e uso dell'oggetto prediction
#per il calcolo del tpr e fpr 
pred_num1 <- as.numeric(test_pred1)
pred1 <- prediction(pred_num1, testing_pre_processed1$label)

#Calcolo del true positive rate e false positive rate
performance_roc1 <- performance(pred1, "tpr", "fpr")

#Plot e salvataggio della roc curve 
png(paste0("model_performance_plot/", "roc_curve_k3_filtraggio_sintetico.png"), width = 800, height = 600)
plot(performance_roc1, col = "blue", lwd = 2, main = "roc curve")
abline(a = 0, b = 1, col = "red", lty = 2)
dev.off()


