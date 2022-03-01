# A Geometric Morphometric approach to lithic backed flake categories

Guillermo Bustos-Pérez<sup>(1)</sup>  
Francesca Romagnoli<sup>(1)</sup>

<sup>(1)</sup>Universidad Autónoma de Madrid. Departamento de
Prehistoria y Arqueología, Campus de Cantoblanco, 28049 Madrid, Spain

**Abstract**  
Paleolithic lithic assemblages are usually dominated by flakes which
present a high morphological variability. When analyzing Paleolithic
lithic assemblages, it is common to classify flakes into categories
based on their morphological and technological features which result
from the position of the flake in a knapping sequence and from the
organization of removals following a knapping strategy. For the analysis
of Middle Paleolithic lithic assemblages two categories of flakes are
commonly used: core edge flakes and pseudo-Levallois points. A third
type, core edge flakes with a limited back, is also commonly employed in
the archaeological literature providing an alternative category with a
definition that does not match the two previous types but shares many of
their morphological and technological features. The present study
addresses whether these flakes constitute discrete categories based on
their morphological and technological attributes. Geometric
morphometrics are employed on an experimental set composed of the three
categories of flakes to quantify morphological variation. Machine
Learning models and Principal Components biplots are employed to test
for the discreteness of the categories. Results indicate that Geometric
Morphometrics succeed in capturing the morphological and technological
features which characterize each type of product. Pseudo-Levallois
points present the highest discreteness of the three technological
products and while some degree of mixture exists between core edge
flakes and core edge flakes with a limited back, they are also highly
distinguishable between each other. It can be concluded that the three
categories are discrete and can be employed in technological lists of
products for the analysis of lithic assemblages, and that Geometric
Morphometrics can be employed to test for the validity of categories.

**Key words**: lithic analysis; lithic technology; geometric
morphometrics; Machine Learning

## 1. Introduction

Lithic artifacts constitute some of the most important and abundant
remains in Paleolithic sites. When analyzing lithic assemblages, in
addition to taking metric measurements and attributes it is common to
classify unretouched flakes according to their morphology and
technological features. This classification is a crucial part of lithic
analysis since it classifies flakes into technological categories. These
categories are technological because the retained features and
morphology are indicative of the knapping method from which they were
generated. These technological products usually reflect different
knapping strategies, different stages of reduction, but also variations
in the organization of removals and exploitation of the surface. Well
known examples of technological classifications of flakes are the
bipolar/on anvil flakes ([Callahan, 1996](#ref-callahan_bipolar_1996);
[Hayden, 1980](#ref-hayden_confusion_1980)) (Callahan, 1996; Hayden,
1980);

### 1.1 Load packages, data and procrustes analysis

``` r
# Load packages
list.of.packages <- c("tidyverse", "caret", "Morpho")
lapply(list.of.packages, library, character.only = TRUE)
```

    ## [[1]]
    ##  [1] "forcats"   "stringr"   "dplyr"     "purrr"     "readr"     "tidyr"    
    ##  [7] "tibble"    "ggplot2"   "tidyverse" "stats"     "graphics"  "grDevices"
    ## [13] "utils"     "datasets"  "methods"   "base"     
    ## 
    ## [[2]]
    ##  [1] "caret"     "lattice"   "forcats"   "stringr"   "dplyr"     "purrr"    
    ##  [7] "readr"     "tidyr"     "tibble"    "ggplot2"   "tidyverse" "stats"    
    ## [13] "graphics"  "grDevices" "utils"     "datasets"  "methods"   "base"     
    ## 
    ## [[3]]
    ##  [1] "Morpho"    "caret"     "lattice"   "forcats"   "stringr"   "dplyr"    
    ##  [7] "purrr"     "readr"     "tidyr"     "tibble"    "ggplot2"   "tidyverse"
    ## [13] "stats"     "graphics"  "grDevices" "utils"     "datasets"  "methods"  
    ## [19] "base"

``` r
rm(list.of.packages)

load("Data/Flakes LM rotated.RData")
Att <- read.csv("Data/Attributes data.csv")
```

``` r
# PCA on rotated landmarks
pca <- prcomp(LM.DF, scale. = TRUE)
summary(pca)
```

    ## Importance of components:
    ##                            PC1     PC2     PC3      PC4      PC5    PC6     PC7
    ## Standard deviation     18.0529 16.6278 12.8309 10.83128 10.42072 8.2993 7.73039
    ## Proportion of Variance  0.2139  0.1814  0.1080  0.07698  0.07125 0.0452 0.03921
    ## Cumulative Proportion   0.2139  0.3953  0.5033  0.58028  0.65153 0.6967 0.73594
    ##                            PC8     PC9   PC10    PC11    PC12    PC13    PC14
    ## Standard deviation     7.43990 6.71091 6.1730 5.36875 4.77302 4.56215 4.44228
    ## Proportion of Variance 0.03632 0.02955 0.0250 0.01891 0.01495 0.01366 0.01295
    ## Cumulative Proportion  0.77226 0.80181 0.8268 0.84573 0.86067 0.87433 0.88728
    ##                           PC15    PC16    PC17    PC18    PC19    PC20    PC21
    ## Standard deviation     3.80303 3.75086 3.54599 3.23142 2.95620 2.70170 2.64992
    ## Proportion of Variance 0.00949 0.00923 0.00825 0.00685 0.00573 0.00479 0.00461
    ## Cumulative Proportion  0.89677 0.90600 0.91425 0.92110 0.92684 0.93163 0.93624
    ##                           PC22    PC23    PC24    PC25    PC26    PC27    PC28
    ## Standard deviation     2.51642 2.46608 2.38239 2.32728 2.23837 1.98545 1.95624
    ## Proportion of Variance 0.00416 0.00399 0.00372 0.00355 0.00329 0.00259 0.00251
    ## Cumulative Proportion  0.94039 0.94438 0.94811 0.95166 0.95495 0.95753 0.96004
    ##                           PC29    PC30    PC31   PC32    PC33    PC34    PC35
    ## Standard deviation     1.85690 1.80740 1.79245 1.7475 1.69007 1.60025 1.48351
    ## Proportion of Variance 0.00226 0.00214 0.00211 0.0020 0.00187 0.00168 0.00144
    ## Cumulative Proportion  0.96231 0.96445 0.96656 0.9686 0.97044 0.97212 0.97356
    ##                           PC36    PC37    PC38    PC39    PC40    PC41   PC42
    ## Standard deviation     1.46400 1.43407 1.38600 1.32645 1.27593 1.24941 1.2320
    ## Proportion of Variance 0.00141 0.00135 0.00126 0.00115 0.00107 0.00102 0.0010
    ## Cumulative Proportion  0.97497 0.97632 0.97758 0.97873 0.97980 0.98082 0.9818
    ##                           PC43    PC44    PC45    PC46    PC47    PC48    PC49
    ## Standard deviation     1.19579 1.11127 1.09391 1.04242 1.00707 0.98962 0.97095
    ## Proportion of Variance 0.00094 0.00081 0.00079 0.00071 0.00067 0.00064 0.00062
    ## Cumulative Proportion  0.98276 0.98357 0.98435 0.98507 0.98573 0.98638 0.98699
    ##                           PC50    PC51    PC52    PC53    PC54    PC55    PC56
    ## Standard deviation     0.94111 0.93054 0.91101 0.88253 0.84890 0.82787 0.82302
    ## Proportion of Variance 0.00058 0.00057 0.00054 0.00051 0.00047 0.00045 0.00044
    ## Cumulative Proportion  0.98758 0.98814 0.98869 0.98920 0.98967 0.99012 0.99057
    ##                           PC57    PC58    PC59    PC60    PC61    PC62    PC63
    ## Standard deviation     0.79862 0.76858 0.72708 0.71449 0.70931 0.69762 0.66775
    ## Proportion of Variance 0.00042 0.00039 0.00035 0.00033 0.00033 0.00032 0.00029
    ## Cumulative Proportion  0.99098 0.99137 0.99172 0.99205 0.99238 0.99270 0.99300
    ##                           PC64    PC65    PC66    PC67    PC68    PC69    PC70
    ## Standard deviation     0.66140 0.64548 0.62334 0.61568 0.60868 0.59213 0.58089
    ## Proportion of Variance 0.00029 0.00027 0.00025 0.00025 0.00024 0.00023 0.00022
    ## Cumulative Proportion  0.99328 0.99356 0.99381 0.99406 0.99430 0.99453 0.99476
    ##                           PC71   PC72    PC73    PC74    PC75    PC76    PC77
    ## Standard deviation     0.56387 0.5507 0.53107 0.53005 0.52537 0.51141 0.49765
    ## Proportion of Variance 0.00021 0.0002 0.00019 0.00018 0.00018 0.00017 0.00016
    ## Cumulative Proportion  0.99496 0.9952 0.99535 0.99553 0.99571 0.99588 0.99605
    ##                           PC78    PC79    PC80    PC81    PC82    PC83    PC84
    ## Standard deviation     0.49078 0.48715 0.47669 0.46900 0.45334 0.44163 0.43322
    ## Proportion of Variance 0.00016 0.00016 0.00015 0.00014 0.00013 0.00013 0.00012
    ## Cumulative Proportion  0.99621 0.99636 0.99651 0.99665 0.99679 0.99692 0.99704
    ##                           PC85    PC86    PC87    PC88   PC89   PC90   PC91
    ## Standard deviation     0.41526 0.41073 0.40543 0.40214 0.3919 0.3905 0.3850
    ## Proportion of Variance 0.00011 0.00011 0.00011 0.00011 0.0001 0.0001 0.0001
    ## Cumulative Proportion  0.99715 0.99726 0.99737 0.99748 0.9976 0.9977 0.9978
    ##                           PC92    PC93    PC94    PC95    PC96    PC97    PC98
    ## Standard deviation     0.37974 0.37519 0.36035 0.35588 0.35204 0.34513 0.33304
    ## Proportion of Variance 0.00009 0.00009 0.00009 0.00008 0.00008 0.00008 0.00007
    ## Cumulative Proportion  0.99787 0.99796 0.99805 0.99813 0.99821 0.99829 0.99836
    ##                           PC99   PC100   PC101   PC102   PC103   PC104   PC105
    ## Standard deviation     0.32839 0.32177 0.32060 0.31732 0.31202 0.30397 0.29939
    ## Proportion of Variance 0.00007 0.00007 0.00007 0.00007 0.00006 0.00006 0.00006
    ## Cumulative Proportion  0.99843 0.99850 0.99857 0.99864 0.99870 0.99876 0.99882
    ##                          PC106   PC107   PC108   PC109   PC110   PC111   PC112
    ## Standard deviation     0.29515 0.29361 0.29195 0.28392 0.27984 0.27632 0.27384
    ## Proportion of Variance 0.00006 0.00006 0.00006 0.00005 0.00005 0.00005 0.00005
    ## Cumulative Proportion  0.99888 0.99893 0.99899 0.99904 0.99909 0.99914 0.99919
    ##                          PC113   PC114   PC115   PC116   PC117   PC118   PC119
    ## Standard deviation     0.26986 0.26656 0.26252 0.25584 0.24753 0.24704 0.24518
    ## Proportion of Variance 0.00005 0.00005 0.00005 0.00004 0.00004 0.00004 0.00004
    ## Cumulative Proportion  0.99924 0.99929 0.99933 0.99938 0.99942 0.99946 0.99950
    ##                          PC120   PC121   PC122   PC123   PC124   PC125   PC126
    ## Standard deviation     0.23857 0.23741 0.22967 0.22712 0.22289 0.21794 0.21572
    ## Proportion of Variance 0.00004 0.00004 0.00003 0.00003 0.00003 0.00003 0.00003
    ## Cumulative Proportion  0.99953 0.99957 0.99960 0.99964 0.99967 0.99970 0.99973
    ##                          PC127   PC128   PC129   PC130   PC131   PC132   PC133
    ## Standard deviation     0.20959 0.20509 0.20068 0.19717 0.18892 0.18743 0.18204
    ## Proportion of Variance 0.00003 0.00003 0.00003 0.00003 0.00002 0.00002 0.00002
    ## Cumulative Proportion  0.99976 0.99979 0.99982 0.99984 0.99986 0.99989 0.99991
    ##                          PC134   PC135   PC136   PC137   PC138     PC139
    ## Standard deviation     0.17869 0.17423 0.16876 0.15802 0.15265 8.293e-15
    ## Proportion of Variance 0.00002 0.00002 0.00002 0.00002 0.00002 0.000e+00
    ## Cumulative Proportion  0.99993 0.99995 0.99997 0.99998 1.00000 1.000e+00

``` r
# store PCA values in a dataframe and add ID's
PCA_Coord <- as.data.frame(pca$x)
PCA_Coord$ID <- filenames
PCA_Coord$Core <- str_sub(PCA_Coord$ID, end = 2)
```

``` r
# Left joined with the attribute database
PCA_Coord <- left_join(PCA_Coord, Att, by = "ID")
```

## 2. Methods

### 2.1 Experimental assemblage

``` r
# Count artifact type per class 
PCA_Coord %>% group_by(ARTIFACTTYPE) %>% 
  summarise(COunt = n())
```

    ## # A tibble: 3 x 2
    ##   ARTIFACTTYPE                COunt
    ##   <chr>                       <int>
    ## 1 Core Edge Flake                30
    ## 2 Core edge with limited back    93
    ## 3 pseudo-Levallois Point         16

## 7. References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-callahan_bipolar_1996" class="csl-entry">

Callahan, E., 1996. The bipolar technique: The simplest way to make
stone tools for survival. Bulletin of Primitive Technology 12, 16–20.

</div>

<div id="ref-hayden_confusion_1980" class="csl-entry">

Hayden, B., 1980. Confusion in the Bipolar World: Bashed Pebbles and
Splintered Pieces. Lithic Technology 9, 2–7.
<https://doi.org/10.1080/01977261.1980.11754456>

</div>

</div>
