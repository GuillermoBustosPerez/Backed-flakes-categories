load("Report/Data/2D Data.RData")
library(tidyverse); library(Morpho)

# Add 3rd dimension to make flat
Flat.2D <- aperm(apply(Coord.2D$orpdata, c(1, 3), c, 0), c(2, 1, 3))
Flat.2D <- Morpho::procSym(Flat.2D)

# PC's are the same but represented as different
# They differ by a very small amount due to the computer's 
# representation of numbers
pcaplot3d(
  Flat.2D,
  pcshow = 1,
  mag = 3,
  type = "spheres",
  color = "purple",
  lwd = 2)
