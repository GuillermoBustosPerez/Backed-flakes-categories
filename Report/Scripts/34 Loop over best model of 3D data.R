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
  paste("New_Art.Type", paste(colnames(PC.scores.3D[,1:20]), collapse = " + "), sep = " ~ "))

trControl <- trainControl(method  = "repeatedcv",
                          verboseIter = TRUE,
                          number  = 10,
                          repeats = 50,
                          savePredictions = "final",
                          classProbs = TRUE)

SVMP.Predictions <- tibble()
SVMP.varImport <- tibble()

repeat {
  # Balance the data prior to resampling
  Balanced <- groupdata2::balance(PC.scores.3D,
                                  size = "mean",
                                  cat_col = "New_Art.Type")
  
  #Train NB model
  SVMP.3D.Model <- train(frmla, 
                         Balanced,
                         method = "svmPoly",
                         trControl = trControl,
                         preProc = c("center", "scale"), 
                         metric = "Accuracy",
                         importance = 'impurity')
  
  # Get predictions after each resampling and store into a data frame
  temp1 <- SVMP.3D.Model$pred
  SVMP.Predictions <- rbind(SVMP.Predictions, temp1)
  rm(temp1)
  
  # Get PC and their importance for the model and storwe into a tibble
  PCs <- rownames(varImp(SVMP.3D.Model)$importance)
  temp2 <- as_tibble(varImp(SVMP.3D.Model)$importance)
  temp2 <- cbind(PCs, temp2)
  SVMP.varImport <- rbind(SVMP.varImport, temp2)
  rm(PCs, temp2)
  
  x <- nrow(SVMP.Predictions)
  if (x >= 207000){
    break
  }
}
