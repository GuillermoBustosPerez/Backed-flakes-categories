library(tidyverse); library(caret)
load("Data/3D landmarks.RData")
Att <- read.csv("Data/Attributes data.csv")

IDs <- list.files(
  path = "GM csvs",
  pattern = '*.csv',
  recursive = TRUE)

IDs <- gsub('.{4}$', '', IDs)

# Change the Ids of the rotated files
dimnames(Land.BF$rotated)[[3]] <- IDs

TD.PCA_Coord <- as.data.frame(Land.BF$PCscores)
TD.PCA_Coord$ID <- dimnames(Land.BF$rotated)[[3]]

Comb.TD.Att <- left_join(TD.PCA_Coord, Att, by = "ID")

Comb.TD.Att <- Comb.TD.Att %>% mutate(
  New_Art.Type = 
    case_when(
      ARTIFACTTYPE == "Core edge flake" ~ "ED",
      ARTIFACTTYPE == "Core edge with limited back" ~ "EDlb",
      ARTIFACTTYPE == "pseudo-Levallois point" ~ "p_Lp"
    ))

Comb.TD.Att$New_Art.Type <- factor(
  Comb.TD.Att$New_Art.Type, 
  levels = c("ED", "EDlb", "p_Lp"),
  labels = c("ED", "EDlb", "p_Lp"))

# Set formula and train control
frmla <- as.formula(
  paste("New_Art.Type", paste(colnames(Comb.TD.Att[,1:7]), collapse = " + "), sep = " ~ "))

trControl <- trainControl(method  = "repeatedcv",
                          verboseIter = TRUE,
                          number  = 10,
                          repeats = 50,
                          savePredictions = "final",
                          classProbs = TRUE)

#### LDA model training ####
All_Results <- tibble()

repeat {
  Balanced <- groupdata2::balance(Comb.TD.Att,
                                  size = "mean",
                                  cat_col = "New_Art.Type")
  
  Model <- train(frmla, 
                 Balanced,
                 method = "lda",
                 trControl = trControl,
                 preProc = c("center", "scale"),
                 metric = "Accuracy",
                 importance = 'impurity')
  
  All_Results <- rbind(All_Results, 
                       tibble(
                         Accuracy = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[1]],
                         Kappa = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[2]],
                         AccuracyLower  = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[3]],
                         AccuracyUpper  = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[4]],
                         AccuracyNull = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[5]],Model = "LDA"))
  
  x = nrow(All_Results)
  if (x >= 30){
    break
  }
}

#### Knn model training ####
repeat {
  Balanced <- groupdata2::balance(Comb.TD.Att,
                                  size = "mean",
                                  cat_col = "New_Art.Type")
  
  Model <- train(frmla, 
                 Balanced,
                 method = "knn",
                 trControl = trControl,
                 preProc = c("center", "scale"),
                 tuneGrid   = expand.grid(k = 2:30),
                 metric = "Accuracy")
  
  All_Results <- rbind(All_Results, 
                       tibble(
                         Accuracy = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[1]],
                         Kappa = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[2]],
                         AccuracyLower  = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[3]],
                         AccuracyUpper  = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[4]],
                         AccuracyNull = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[5]],
                         Model = "KNN"))
  
  x = nrow(All_Results)
  if (x >= 60){
    break
  }
}

#### Logistic regression ####

repeat {
  Balanced <- groupdata2::balance(Comb.TD.Att,
                                  size = "mean",
                                  cat_col = "New_Art.Type")
  
  Model <- train(frmla, 
                 Balanced,
                 method = "glmnet",
                 family = 'multinomial',
                 trControl = trControl,
                 preProc = c("center", "scale"), 
                 metric = "Accuracy")
  
  All_Results <- rbind(All_Results, 
                       tibble(
                         Accuracy = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[1]],
                         Kappa = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[2]],
                         AccuracyLower  = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[3]],
                         AccuracyUpper  = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[4]],
                         AccuracyNull = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[5]],
                         Model = "Log. Reg."))
  
  x = nrow(All_Results)
  if (x >= 90){
    break
  }
}

save(All_Results, 
     file = "Data/3D Data Up and Down sampling Limited variables.RData")

#### Random forest ####
repeat {
  Balanced <- groupdata2::balance(Comb.TD.Att,
                                  size = "mean",
                                  cat_col = "New_Art.Type")
  
  Model <- train(frmla, 
                 Balanced,
                 method = "ranger",
                 trControl = trControl,
                 preProc = c("center", "scale"), 
                 metric = "Accuracy",
                 importance = 'impurity')
  
  All_Results <- rbind(All_Results, 
                       tibble(
                         Accuracy = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[1]],
                         Kappa = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[2]],
                         AccuracyLower  = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[3]],
                         AccuracyUpper  = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[4]],
                         AccuracyNull = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[5]],
                         Model = "Random Forest"))
  
  x = nrow(All_Results)
  if (x >= 120){
    break
  }
}


#### SVM linear ####
repeat {
  Balanced <- groupdata2::balance(Comb.TD.Att,
                                  size = "mean",
                                  cat_col = "New_Art.Type")
  
  Model <- train(frmla, 
                 Balanced,
                 method = "svmLinear",
                 trControl = trControl,
                 preProc = c("center", "scale"), 
                 metric = "Accuracy",
                 importance = 'impurity')
  
  All_Results <- rbind(All_Results, 
                       tibble(
                         Accuracy = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[1]],
                         Kappa = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[2]],
                         AccuracyLower  = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[3]],
                         AccuracyUpper  = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[4]],
                         AccuracyNull = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[5]],
                         Model = "SVM Linear"))
  
  x = nrow(All_Results)
  if (x >= 150){
    break
  }
}

#### SVM radial ####
repeat {
  Balanced <- groupdata2::balance(Comb.TD.Att,
                                  size = "mean",
                                  cat_col = "New_Art.Type")
  
  Model <- train(frmla, 
                 Balanced,
                 method = "svmRadial",
                 trControl = trControl,
                 preProc = c("center", "scale"), 
                 metric = "Accuracy",
                 importance = 'impurity')
  
  All_Results <- rbind(All_Results, 
                       tibble(
                         Accuracy = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[1]],
                         Kappa = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[2]],
                         AccuracyLower  = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[3]],
                         AccuracyUpper  = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[4]],
                         AccuracyNull = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[5]],
                         Model = "SVM Radial"))
  
  x = nrow(All_Results)
  if (x >= 180){
    break
  }
}

# Save 
save(All_Results, 
     file = "Data/Results 3D Data Limited variables.RData")

#### SVM Poly ####
repeat {
  Balanced <- groupdata2::balance(Comb.TD.Att,
                                  size = "mean",
                                  cat_col = "New_Art.Type")
  
  Model <- train(frmla, 
                 Balanced,
                 method = "svmPoly",
                 trControl = trControl,
                 preProc = c("center", "scale"), 
                 metric = "Accuracy",
                 importance = 'impurity')
  
  All_Results <- rbind(All_Results, 
                       tibble(
                         Accuracy = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[1]],
                         Kappa = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[2]],
                         AccuracyLower  = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[3]],
                         AccuracyUpper  = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[4]],
                         AccuracyNull = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[5]],
                         Model = "SVM Poly"))
  
  x = nrow(All_Results)
  if (x >= 210){
    break
  }
}

save(All_Results, 
     file = "Data/3D Data Up and Down sampling Limited variables.RData")

#### C5.0 Tree ####
repeat {
  Balanced <- groupdata2::balance(Comb.TD.Att,
                                  size = "mean",
                                  cat_col = "New_Art.Type")
  
  Model <- train(frmla, 
                 Balanced,
                 method = "C5.0",
                 trControl = trControl,
                 preProc = c("center", "scale"), 
                 metric = "Accuracy",
                 importance = 'impurity')
  
  All_Results <- rbind(All_Results, 
                       tibble(
                         Accuracy = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[1]],
                         Kappa = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[2]],
                         AccuracyLower  = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[3]],
                         AccuracyUpper  = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[4]],
                         AccuracyNull = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[5]],
                         Model = "C5.0 Tree"))
  
  x = nrow(All_Results)
  if (x >= 240){
    break
  }
}

#### Naïve Bayes ####
repeat {
  Balanced <- groupdata2::balance(Comb.TD.Att,
                                  size = "mean",
                                  cat_col = "New_Art.Type")
  
  Model <- train(frmla, 
                 Balanced,
                 method = "nb",
                 trControl = trControl,
                 preProc = c("center", "scale"), 
                 metric = "Accuracy",
                 importance = 'impurity')
  
  All_Results <- rbind(All_Results, 
                       tibble(
                         Accuracy = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[1]],
                         Kappa = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[2]],
                         AccuracyLower  = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[3]],
                         AccuracyUpper  = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[4]],
                         AccuracyNull = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[5]],
                         Model = "Naïve Bayes"))
  
  x = nrow(All_Results)
  if (x >= 270){
    break
  }
}

# Save 
save(All_Results, 
     file = "Data/3D Data Up and Down sampling Limited variables.RData")

load("Data/Results 3D Data Limited variables.RData")

#### Boosted Tree ####
repeat {
  Balanced <- groupdata2::balance(Comb.TD.Att,
                                  size = "mean",
                                  cat_col = "New_Art.Type")
  
  Model <- train(frmla, 
                 Balanced,
                 method = "gbm",
                 trControl = trControl,
                 preProc = c("center", "scale"), 
                 metric = "Accuracy")
  
  All_Results <- rbind(All_Results, 
                       tibble(
                         Accuracy = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[1]],
                         Kappa = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[2]],
                         AccuracyLower  = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[3]],
                         AccuracyUpper  = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[4]],
                         AccuracyNull = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[5]],
                         Model = "GBM"))
  
  x = nrow(All_Results)
  if (x >= 300){
    break
  }
}

save(All_Results, 
     file = "Data/Results 3D Data Limited variables.RData")


Models.3D <- All_Results
Models.3D$Model <- factor(Models.3D$Model,
                          levels = c(
                            "LDA", "KNN", "Log. Reg.",
                            "C5.0 Tree", "Random Forest", "GBM",
                            "SVM Linear", "SVM Radial",
                            "SVM Poly",
                            "Naïve Bayes"
                          ))

save(
  Models.3D,
  file = "Data/3D Results Up and Down sampling Limited variables.RData")
rm(list = ls())


Models.3D %>% 
  ggplot(aes(Model, Accuracy, fill = Model)) +
  geom_violin(position = position_dodge(1), width = 0.4, alpha = 0.5) +
  geom_boxplot(width = 0.4,
               outlier.shape = NA, alpha = 0.5) +
  geom_jitter(width = 0.15, alpha = 0.9, size = 0.9, shape = 23, aes(fill = Model)) +
  scale_y_continuous(breaks = seq(0.4, 1, 0.2), lim = c(0.4, 1)) +
  theme_light() +
  ylab("Accuracy after each cycle of up and down sampling") +
  ggtitle(label = "3D data") +
  ggsci::scale_fill_aaas() +
  scale_x_discrete(labels = c(
    "LDA", "KNN", "Log. Reg.",
    "C5.0\nTree", "Random\nForest", "GBM",
    "SVM\nLinear", "SVM\nRadial",
    "SVM\nPoly",
    "Naïve\nBayes")) +
  xlab(NULL) +
  theme(
    legend.position = "none",
    axis.text = element_text(color = "black", size = 8),
    axis.title = element_text(color = "black", size = 8.5),
    plot.title = element_text(hjust = 0, vjust = 1, size = 9))

All_Results %>% group_by(Model) %>% 
  summarise(Mean = mean(Accuracy))




