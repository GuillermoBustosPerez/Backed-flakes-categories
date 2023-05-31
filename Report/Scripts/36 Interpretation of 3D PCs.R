load("Report/Data/3D Data.RData")
library(tidyverse); library(Morpho)

# PC's are the same but represented as different
pcaplot3d(
  Coord.3D,
  pcshow = 1,
  mag = 3,
  type = "spheres",
  color = "blue",
  lwd = 2)
