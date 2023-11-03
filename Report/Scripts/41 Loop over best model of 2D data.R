library(tidyverse);library(Rvcg); library(Morpho); library(caret)
load("Report/Data/2D data.RData")
Att <- read.csv("Report/Data/Attributes data.csv")

TD.PCA_Coord <- as.data.frame(Coord.2D$PCscores)
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
  paste("New_Art.Type", paste(colnames(Comb.TD.Att[,1:9]), collapse = " + "), sep = " ~ "))

library(caret)
trControl <- trainControl(method  = "repeatedcv",
                          verboseIter = TRUE,
                          number  = 10,
                          repeats = 50,
                          savePredictions = "final",
                          classProbs = TRUE)

RF.Predictions <- tibble()
RF.varImport <- tibble()

repeat {
  # Balance the data prior to resampling
  Balanced <- groupdata2::balance(Comb.TD.Att,
                                  size = "mean",
                                  cat_col = "New_Art.Type")
  
  #Train NB model
  RF.2D.Model <- train(frmla, 
                       Balanced,
                       method = "ranger",
                       trControl = trControl,
                       preProc = c("center", "scale"), 
                       metric = "Accuracy",
                       importance = 'impurity')
  
  # Get predictions after each resampling and store into a data frame
  temp1 <- RF.2D.Model$pred
  RF.Predictions <- rbind(RF.Predictions, temp1)
  rm(temp1)
  
  # Get PC and their importance for the model and storwe into a tibble
  PCs <- rownames(varImp(RF.2D.Model)$importance)
  temp2 <- as_tibble(varImp(RF.2D.Model)$importance)
  temp2 <- cbind(PCs, temp2)
  RF.varImport <- rbind(RF.varImport, temp2)
  rm(PCs, temp2)
  
  x <- nrow(RF.Predictions)
  if (x >= 207000){
    break
  }
}

save(RF.Predictions,
     RF.varImport, 
     file = "Data/Best model 2D varimp and cm.RData")
