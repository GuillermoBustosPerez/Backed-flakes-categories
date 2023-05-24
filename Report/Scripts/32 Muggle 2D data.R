library(geomorph); library(tidyverse); library(Morpho)

# Load data using momocs (it can read tps files in which elements have different number of landmarks)
Imported.tps <- Momocs::import_tps("Report/Data/2D-Upper-view.TPS")

IDs <- names(Imported.tps$coo)

# start empty element into which to store resampled landmarks
Data.2D <- c()

# loop over each elelemnt in the list to resample the length of the landmarks
for (element in Imported.tps$coo) {
  
  # resample the length
  element <- Morpho::resampleCurve(element, 
                                   n = 100,
                                   open = FALSE)
  
  # add it to a list
  element <- list(element)
  
  # append to the new list. Each element is made of matrixes containing the resampled coordinates
  Data.2D <- append(Data.2D, element, after = length(Data.2D))
  rm(element)
}

# 
Data.2D <- str2str::lm2a(Data.2D)
dimnames(Data.2D)[[3]] <- IDs

Coord.2D <- Morpho::procSym(Data.2D,
                            outlines = c(1:100)) 

save(Coord.2D,
     file = "Report/Data/2D data.RData")



















Upper.view <- c()

# loop over each elelemnt in the list to resample the length of the landmarks
for (element in df.list) {
  # resample to the desired legth
  element <- resampleCurve(matrix(element, ncol = 2), n = 100,
                           open = FALSE)
  
  # make into a list element
  element <- list(element)
  
  # append to the new list. Each element is made of matrixes containing the resampled coordinates
  Upper.view <- append(Upper.view, element, after = length(Upper.view))
}

# unlist into a 3D array


# Procrustes alignment





dimnames(Upper.view) <- IDs

test2 <- list(resampleCurve(matrix(test$ID_B2_109, ncol = 2), n = 100, open = FALSE))
test3 <- resampleCurve(matrix(test$ID_B2_110, ncol = 2), n = 100, open = FALSE)

list(test2)



Upper.view <- append(Upper.view, test2, after = length(Upper.view))

bindArr(test2, test3)



plot(Upper.view[1:100, 1:2, 4]) 
       

plot(test3)