library(tidyverse);library(Rvcg); library(Morpho); library(caret)

# Read in all data from the csv folder
Land.BF <- Morpho::read.csv.folder(
  folder = "Report/Data/GM csvs",
  x = 1:508,
  y = 1:3,
  pattern = ".csv",
  dec = ",",
  header = TRUE)

# Morpho doesent read correctly the file IDs. get ID's from file names
# the order in which Morpho reads the csvs is the same than list.files()
IDs <- list.files(
  path = "Report/Data/GM csvs",
  pattern = '*.csv',
  recursive = TRUE)

IDs <- gsub('.{4}$', '', IDs)

# Perform procrustes
Coord.3D <- procSym(Land.BF$arr)

# Change the Ids of the rotated files
dimnames(Coord.3D$rotated)[[3]] <- IDs

# Save 
save(Coord.3D, 
     file = "Report/Data/3D landmarks.RData")
