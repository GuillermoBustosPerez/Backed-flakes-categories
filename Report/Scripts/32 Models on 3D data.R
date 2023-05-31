library(tidyverse); library(caret)
load("Data/3D landmarks.RData")
Att <- read.csv("Data/Attributes data.csv")

TD.PCA_Coord <- as.data.frame(Coord.3D$PCscores)
TD.PCA_Coord$ID <- rownames(TD.PCA_Coord)

Comb.TD.Att <- left_join(TD.PCA_Coord, Att, by = "ID")

Comb.TD.Att <- Comb.TD.Att %>% mutate(
  New_Art.Type = 
    case_when(
      ARTIFACTTYPE == "Core Edge Flake" ~ "ED",
      ARTIFACTTYPE == "Core edge with limited back" ~ "EDlb",
      ARTIFACTTYPE == "pseudo-Levallois Point" ~ "p_Lp"
    ))

Comb.TD.Att$New_Art.Type <- factor(
  Comb.TD.Att$New_Art.Type, 
  levels = c("ED", "EDlb", "p_Lp"),
  labels = c("ED", "EDlb", "p_Lp"))

# Set formula and train control
frmla <- as.formula(
  paste("New_Art.Type", paste(colnames(Comb.TD.Att[,1:20]), collapse = " + "), sep = " ~ "))

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
                         Model = "Boosted Tree"))
  
  x = nrow(All_Results)
  if (x >= 300){
    break
  }
}

Models.3D <- All_Results

save(
  Models.3D,
  "Report/Data/3D Results Up and Down sampling.RData")
rm(list = ls())