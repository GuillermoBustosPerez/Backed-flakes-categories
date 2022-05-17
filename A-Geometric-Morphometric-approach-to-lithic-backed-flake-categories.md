# A geometric morphometric approach to testing discrete categories of backed flakes from recurrent centripetal core reduction.

Guillermo Bustos-Pérez<sup>(1)</sup>  
Brad Gravina<sup>(2,3)</sup> Michel Brenet<sup>(3,4)</sup> Francesca
Romagnoli<sup>(1)</sup>

<sup>(1)</sup>Universidad Autónoma de Madrid. Departamento de
Prehistoria y Arqueología, Campus de Cantoblanco, 28049 Madrid, Spain  
<sup>(2)</sup>Musée national de Préhistoire, MC, 1 rue du Musée, 24260
Les Eyzies de Tayac, France  
<sup>(3)</sup>UMR-5199 PACEA, Université de Bordeaux, Bâtiment B8, Allée
Geoffroy Saint Hilaire, CS 50023, 33615 PESSAC CEDEX, France  
<sup>(4)</sup>INRAP Grand Sud-Ouest, Centre mixte de recherches
archéologiques, Domaine de Campagne, 242460 Campagne, France

**Abstract**

Paleolithic lithic assemblages are usually dominated by flakes, which
display a high degree of morphological variability. When analyzing
Paleolithic lithic assemblages, it is common to classify flakes into
categories based on their morphological and technological features,
which are linked to the position of the flake in a reduction sequence
and how removals are organized in a given production method. For the
analysis of Middle Paleolithic lithic assemblages, two categories of
flakes are commonly used: core edge flakes and pseudo-Levallois points.
A third type, core edge flakes with a limited back, is also commonly
found in the archaeological literature, providing an alternative
category with a definition that does not match the two previous types
but shares many of their morphological and technological features. The
present study addresses whether these three flakes constitute discrete
categories based on their morphological and technological attributes.
Geometric morphometrics are employed on an experimental set composed of
the three categories of flakes to quantify morphological variation.
Machine learning models and principal components biplots are used to
test the discreteness of the categories. The results indicate that
geometric morphometrics succeed in capturing the morphological and
technological features that characterize each type of product.
Pseudo-Levallois points have the highest discreteness of the three
technological products, and while some degree of mixture exists between
core edge flakes and core edge flakes with a limited back, they are also
highly distinguishable. We conclude that the three categories are
discrete and can be employed in technological lists of products for the
analysis of lithic assemblages and that geometric morphometrics is
useful for testing for the validity of categories.

**Key words**: lithic analysis; lithic technology; geometric
morphometrics; machine learning; Middle Paleolithic; Levallois;
discoidal

## 1. Introduction

Lithic artifacts regularly constitute the most important and abundant
remains found on Paleolithic sites. When analyzing lithic assemblages,
in addition to taking metric measurements and noting attributes, it is
common to classify unmodified flakes according to their morphologies and
technological features. This is a crucial part of lithic analysis
because it classifies flakes into technological categories in the sense
that the retained features and morphology are indicative of the
production method by which they were generated. These technological
products usually reflect different knapping strategies, stages of
reduction as well as variations in the organization of removals and
surface exploitation. Well-known examples of technological
classifications of flakes include bipolar/on anvil flakes ([Callahan,
1996](#ref-callahan_bipolar_1996); [Hayden,
1980](#ref-hayden_confusion_1980)), overshot flakes ([Cotterell and
Kamminga, 1987](#ref-cotterell_formation_1987)), bifacial-thinning
flakes ([Raab et al., 1979](#ref-raab_debitage_1979)), byproducts of
blade production, such as core tablets or crested blades ([Pelegrin,
1995](#ref-pelegrin_technologie_1995); [Shea,
2013a](#ref-shea_upper_2013)), kombewa flakes ([Tixier et al.,
1980](#ref-tixier_prehistoire_1980); [Tixier and Turq,
1999](#ref-tixier_kombewa_1999)) and lateral tranchet blows
([Bourguignon, 1992](#ref-bourguignon_analyse_1992)). While the use of
technological categories is common and helps increase the resolution of
lithic analysis, it is important to bear in mind that lithic artifacts
are characterized by a high degree of morphological variability, which,
in many cases, results in overlapping features. Consequently, several
categories remain underused because of this high morphological
variability, their overlapping features, and their similar roles in the
volumetric management of the core along the reduction process.

The Middle Paleolithic in Western Europe is characterized by the
diversification of an increase in knapping methods, resulting in what
are generally flake-dominated assemblages ([Delagnes and Meignen,
2006](#ref-hovers_diversity_2006); [Kuhn, 2013](#ref-kuhn_roots_2013)).
For the analysis of Middle Paleolithic lithic assemblages, lists of
technological products are common and tend to reflect individual
knapping methods, the organization of flake removals, and their
morphology ([Duran and Abelanet, 2004](#ref-duran_mousterien_2004);
[Duran and Soler, 2006](#ref-duran_variabilite_2006); [Geneste,
1988](#ref-rigaud_les_1988); [Shea, 2013b](#ref-shea_middle_2013)).
These technological lists are usually dominated by categories of
technological products related to Levallois and discoidal knapping
methods ([Boëda, 1995](#ref-dibble_levallois:_1995),
[1993](#ref-boeda_debitage_1993); [Boëda et al.,
1990](#ref-boeda_identification_1990)), which constitute an important
part of Middle Paleolithic lithic variability. Various discoidal and
Levallois products have been identified and first appear approximately
at 400,000 ka, in a vast area from eastern Asia to the Atlantic coast of
Western Europe through Siberia and Central Asia, the Levant, Eastern and
Central Europe (see bibliography in [Romagnoli et al.,
2022](#ref-romagnoli_neanderthal_2022)), and Africa ([Adler et al.,
2014](#ref-adler_early_2014); [Blinkhorn et al.,
2021](#ref-blinkhorn_directional_2021)). The identification of discoidal
and Levallois products therefore appears widespread in lithic analysis
across various traditions of lithic studies and is designed to create a
comparable dataset, explore specific technological adaptations in
different ecological contexts, and discuss long-term techno-cultural
traditions and technological change. One special category of such
products are backed flakes that exhibit remnants of the core on one of
their lateral edges. Backed flakes are usually classified into two
technological categories: “core edge flakes” (*eclat débordant*) and
“pseudo-Levallois points.” A third category, “core edge flakes with a
limited back” (*éclat débordant à dos limité*), has also been defined
([Meignen, 1996](#ref-meignen_persistance_1996); [Meignen,
1993](#ref-meignen_les_1993); [Pasty et al.,
2004](#ref-pasty_etude_2004)), although its use is not widespread
([Duran and Abelanet, 2004](#ref-duran_mousterien_2004); [Duran and
Soler, 2006](#ref-duran_variabilite_2006); [Geneste,
1988](#ref-rigaud_les_1988); [Shea, 2013b](#ref-shea_middle_2013)). One
reason for this may be their overlapping features, morphology, and
having a similar role in core reduction compared to classic core edge
flakes. This usually results in their absorption into the group of core
edge flakes when technological lists of products are employed.

The present study seeks to evaluate whether “core edge flakes with a
limited back” are a discrete category that can be easily separated from
classic core edge flakes and pseudo-Levallois points based on their
morphological features. To test the discreteness of these categories, an
experimental sample of backed flakes produced by discoidal and recurrent
centripetal Levallois reduction is classified, following their
technological definitions. Geometric morphometrics on 3D meshes are
employed to quantify the morphological variability of the experimental
assemblage. To test for the discreteness of these categories, machine
learning algorithms are employed to classify the flakes according to
their technological category. Our hypothesis is that, although some
degree of overlap is expected due to the high degree of morphological
variability among lithic artifacts, machine learning models should
easily differentiate the above-mentioned categories from one another.
Testing this hypothesis would support the use of these backed flakes
categories in the classification of lithic assemblages.

The following code loads attribute data from the experimental assemblage
and packages employed for the analysis.

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

# Load attribute data
Att <- read.csv("Data/Attributes data.csv")
```

## 2. Method

### 2.1 Experimental assemblage

The present study uses an experimental assemblage that was the result of
nine knapping sequences. Seven cores were knapped on Bergerac flint
([Fernandes et al., 2012](#ref-fernandes_silex_2012)) and two cores were
knapped on Miocene flint from South of Madrid ([Bustillo et al.,
2012](#ref-bustillo_caracterizacion_2012); [Bustillo and Pérez-Jiménez,
2005](#ref-bustillo_caracteristicas_2005)). Five cores were knapped
following the Discoid “sensu stricto” concept, which strongly
corresponds to Boëda’s original technological definition of the knapping
system ([Boëda, 1995](#ref-dibble_levallois:_1995),
[1994](#ref-boeda_concept_1994), [1993](#ref-boeda_debitage_1993)), and
five experimental cores were knapped following the Levallois recurrent
centripetal system ([Boëda, 1995](#ref-dibble_levallois:_1995),
[1994](#ref-boeda_concept_1994), [1993](#ref-boeda_debitage_1993);
[Lenoir and Turq, 1995](#ref-dibble_recurrent_1995)).

Six technological characteristics define the Levallois concept ([Boëda,
1994](#ref-boeda_concept_1994), [1993](#ref-boeda_debitage_1993)): (1)
the volume of the core is conceived as two convex asymmetric surfaces;
(2) these two surfaces are hierarchical and not interchangeable. They
maintain their roles as striking platforms and debitage (or
exploitation) surfaces, respectively, throughout the entire reduction
process; (3) the distal and lateral convexities of the debitage surface
are maintained to obtain predetermined flakes; (4) the fracture plane of
the predetermined products is parallel to the intersection between both
surfaces; (5) the striking platform is perpendicular to the overhang
(the core edge, at the intersection between the two core surfaces); (6)
the technique employed during the knapping process is direct hard-hammer
percussion. Depending on the organization of the debitage surface
Levallois cores are usually classified into the preferential method
(where a single predetermined Levallois flake is obtained from the
debitage surface) or recurrent methods (where several predetermined
flakes are produced from the debitage surface), with removals being
either unidirectional, bidirectional, or centripetal ([Boëda,
1995](#ref-dibble_levallois:_1995); [Boëda et al.,
1990](#ref-boeda_identification_1990); [Delagnes,
1995](#ref-dibble_variability_1995); [Delagnes and Meignen,
2006](#ref-hovers_diversity_2006)).

According to Boëda ([1995](#ref-dibble_levallois:_1995),
[1994](#ref-boeda_concept_1994), [1993](#ref-boeda_debitage_1993)),
there are six technological criteria that define the Discoid “sensu
stricto” method: (1) the volume of the core is conceived as two oblique
asymmetric convex surfaces delimited by an intersecting plane; (2) these
two surfaces are not hierarchical, with it being possible to alternate
between the roles of striking platforms and exploitation surfaces; (3)
the peripheral convexity of the debitage surface is managed to control
lateral and distal extractions, thus allowing for a degree of
predetermination; (4) the surfaces of the striking platforms are
oriented in such a way that the core edge is perpendicular to the
predetermined products; (5) the fracture planes of the products are
secant; (6) the technique employed is direct hard-hammer percussion.

A total of 139 unretouched backed flakes (independent of the type of
termination) were obtained from the different experimental reduction
sequences, 70 from discoidal reduction sequences and 69 from Levallois
recurrent centripetal reduction sequences. The following criteria were
followed for the classification of backed flakes:

Core edge flakes / *eclat débordant* ([Beyries and Boëda,
1983](#ref-beyries_etude_1983); [Boëda, 1993](#ref-boeda_debitage_1993);
[Boëda et al., 1990](#ref-boeda_identification_1990)) have a cutting
edge opposite and usually (although not always) parallel to an abrupt
margin. This abrupt margin, or backed edge (dos), commonly results from
the removal of a portion of the periphery of the core and can be plain,
bear the scars from previous removals, be cortical, or present a mix of
the three. Classic “core edge flakes” ([Boëda,
1993](#ref-boeda_debitage_1993); [Boëda et al.,
1990](#ref-boeda_identification_1990)), which are sometimes referred as
“core edge flakes with non-limited back” / *“éclat débordant à dos non
limité”* ([Duran, 2005](#ref-duran_lindustrie_2005); [Duran and Soler,
2006](#ref-duran_variabilite_2006)) have a morphological axis that
follows the axis of percussion, although it may deviate slightly
([Beyries and Boëda, 1983](#ref-beyries_etude_1983)).  
“Core edge flakes with a limited back” / *“éclat débordant à dos
limité”* share with core edge flakes the morphological feature of having
a cutting edge opposite a back. However, the main difference resides in
their having a morphological axis clearly offset in respect to the axis
of percussion ([Meignen, 1996](#ref-meignen_persistance_1996); [Meignen,
1993](#ref-meignen_les_1993); [Pasty et al.,
2004](#ref-pasty_etude_2004)). Because of this deviation from the axis
of percussion, the backed edge is usually not completely parallel and
does not span the entire length of the sharp edge.  
Pseudo-Levallois points ([Boëda, 1993](#ref-boeda_debitage_1993); [Boëda
et al., 1990](#ref-boeda_identification_1990); [Bordes,
1961](#ref-bordes_typologie_1961), [1953](#ref-bordes_notules_1953);
[Slimak, 2003](#ref-peresani_les_2003)) are backed products in which the
edge opposite the back has a triangular morphology. This triangular
morphology is usually the result of the convergence of two or more
removals. As with core edge flakes, the back usually results from the
removal of one of the lateral edges of the core and can be plain, retain
the scars from previous removals, or more rarely be cortical.
Pseudo-Levallois points share with core edge flakes with a limited back
the deviation of symmetry from the axis of percussion, but they are
clearly differentiable due to their triangular off-axis morphology.

The following table presents the distribution of backed flakes types,
following the previously established definitions. Due to the centripetal
nature of the knapping methods employed to generate the experimental
assemblage, most of the backed flakes fall within the definition of core
edge flakes with a limited back (66.91%). Cortex distribution according
to backed flake category shows that slightly (\~25%) or non-cortical
products dominate among the three categories, adding up to more than 65%
in the three cases (90% core edge flakes, 68.82% core edge flakes with a
limited back, and 87.5% pseudo-Levallois points).

``` r
Att %>% group_by(Strategy, ARTIFACTTYPE) %>% 
  summarise(
    Count = n(),
    Percent = (Count/139)*100)
```

    ## # A tibble: 6 x 4
    ## # Groups:   Strategy [2]
    ##   Strategy  ARTIFACTTYPE                Count Percent
    ##   <chr>     <chr>                       <int>   <dbl>
    ## 1 Discoid   Core Edge Flake                11    7.91
    ## 2 Discoid   Core edge with limited back    47   33.8 
    ## 3 Discoid   pseudo-Levallois Point         12    8.63
    ## 4 Levallois Core Edge Flake                19   13.7 
    ## 5 Levallois Core edge with limited back    46   33.1 
    ## 6 Levallois pseudo-Levallois Point          4    2.88

![Figure 1. Backed artifacts classification types from the experimental
assemblage and their
classification](Figures/Backed%20flakes%20types.png)

### 1.1 Load packages, data and procrustes analysis

``` r
load("Data/Flakes LM rotated.RData")
```

``` r
# PCA on rotated landmarks
pca <- prcomp(LM.DF, scale. = TRUE)
summary(pca)$importance[1:3, 1:25]
```

    ##                             PC1      PC2      PC3      PC4      PC5      PC6
    ## Standard deviation     18.05288 16.62783 12.83087 10.83128 10.42072 8.299316
    ## Proportion of Variance  0.21385  0.18142  0.10803  0.07698  0.07125 0.045200
    ## Cumulative Proportion   0.21385  0.39527  0.50330  0.58028  0.65153 0.696730
    ##                            PC7      PC8      PC9     PC10     PC11     PC12
    ## Standard deviation     7.73039 7.439897 6.710911 6.173021 5.368746 4.773021
    ## Proportion of Variance 0.03921 0.036320 0.029550 0.025000 0.018910 0.014950
    ## Cumulative Proportion  0.73594 0.772260 0.801810 0.826810 0.845730 0.860670
    ##                            PC13    PC14     PC15     PC16    PC17    PC18
    ## Standard deviation     4.562145 4.44228 3.803028 3.750857 3.54599 3.23142
    ## Proportion of Variance 0.013660 0.01295 0.009490 0.009230 0.00825 0.00685
    ## Cumulative Proportion  0.874330 0.88728 0.896770 0.906000 0.91425 0.92110
    ##                            PC19    PC20     PC21     PC22     PC23    PC24
    ## Standard deviation     2.956198 2.70170 2.649925 2.516422 2.466077 2.38239
    ## Proportion of Variance 0.005730 0.00479 0.004610 0.004160 0.003990 0.00372
    ## Cumulative Proportion  0.926840 0.93163 0.936240 0.940390 0.944380 0.94811
    ##                            PC25
    ## Standard deviation     2.327281
    ## Proportion of Variance 0.003550
    ## Cumulative Proportion  0.951660

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
  summarise(Count = n())
```

    ## # A tibble: 3 x 2
    ##   ARTIFACTTYPE                Count
    ##   <chr>                       <int>
    ## 1 Core Edge Flake                30
    ## 2 Core edge with limited back    93
    ## 3 pseudo-Levallois Point         16

``` r
# Cortex per artefact type
PCA_Coord %>% group_by(ARTIFACTTYPE) %>% 
  count(CORTEX) %>% 
  
  mutate(Percentage = round(n/sum(n)*100, 2)) %>% 
  
  ggplot(aes(CORTEX, Percentage, fill = ARTIFACTTYPE)) +
  geom_col(position = "dodge") +
  ggsci::scale_fill_aaas() +
  xlab(NULL) +
  geom_text(aes(label = paste0(Percentage, "%")), 
            vjust= -0.2, size = 2.5,
            position = position_dodge(.9)) +
  geom_text(aes(label = paste("n =", n)), 
            vjust= "top", size = 2.5,
            position = position_dodge(.9)) +
  labs(fill = NULL) +
  
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(color = "black", size = 8))
```

![](A-Geometric-Morphometric-approach-to-lithic-backed-flake-categories_files/figure-markdown_github/Cortex%20distribution%20per%20artefact%20type-1.png)

### 2.2 Geometric Morphometrics

### 2.3 Machine Learning and resampling techniques

### 2.4 Pre processing and training the models

``` r
#### Pre processing data
# Change syntax of output
PCA_Coord <- PCA_Coord %>% mutate(
  New_Art.Type = 
    case_when(
      ARTIFACTTYPE == "Core Edge Flake" ~ "ED",
      ARTIFACTTYPE == "Core edge with limited back" ~ "EDlb",
      ARTIFACTTYPE == "pseudo-Levallois Point" ~ "p_Lp"
    ))

# Set factors
PCA_Coord$New_Art.Type <- factor(
  PCA_Coord$New_Art.Type, 
  levels = c("ED", "EDlb", "p_Lp"),
  labels = c("ED", "EDlb", "p_Lp"))
```

``` r
# Set formula and validation
# Formula
frmla <- as.formula(
  paste("New_Art.Type", paste(colnames(PCA_Coord[,1:25]), collapse = " + "), sep = " ~ "))

# Validation
trControl <- trainControl(method  = "repeatedcv",
                          verboseIter = TRUE,
                          number  = 10,
                          repeats = 50,
                          savePredictions = "final",
                          classProbs = TRUE)
```

As mentioned previously, this is a unbalanced data set. Balancing to
train the models correctly is done using the `groupdata2` package using
the parameter **“mean”** for size. This means that each group will be up
or down sampled to the result of dividing data set size between number
of groups (46.3333333).

-   A `tibble()` called `All_Results` is set to store the results from
    each model training.

Each loop works in the following steps:

1.  The original data set is randomly up and down sampled for each
    target category.  
2.  Model is trained following the provided hyperparameters and data
    pre-processing.  
3.  Results from the trained model are extracted and binned to the
    `All_Results` tibble.  
4.  Steps 1 to 3 are repeated 30 times per model.

``` r
#### Train the models
# Set tibble in which results of each model will be stored
All_Results <- tibble()

# LDA model training 
repeat {
  # Balance the dataset
  Balanced <- groupdata2::balance(PCA_Coord,
                                  size = "mean",
                                  cat_col = "New_Art.Type")
  # Train the model
  Model <- train(frmla, 
                     Balanced,
                     method = "lda",
                     trControl = trControl,
                     preProc = c("center", "scale"),
                     metric = "Accuracy",
                     importance = 'impurity')
  
  # Bind model results and type of model
  All_Results <- rbind(All_Results, 
                       tibble(
                         Accuracy = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[1]],
                         Kappa = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[2]],
                         AccuracyLower  = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[3]],
                         AccuracyUpper  = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[4]],
                         AccuracyNull = confusionMatrix(Model$pred$pred, Model$pred$obs)$overall[[5]],
                         Model = "LDA"))
  
  x = nrow(All_Results)
  if (x >= 30){
    break
  }
}

# Knn model training 
repeat {
  Balanced <- groupdata2::balance(PCA_Coord,
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

# Logistic regression 
repeat {
  Balanced <- groupdata2::balance(PCA_Coord,
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

# Random forest 
repeat {
  Balanced <- groupdata2::balance(PCA_Coord,
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

# SVM linear 
repeat {
  Balanced <- groupdata2::balance(PCA_Coord,
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
  if (x >= 180){
    break
  }
}

# SVM radial 
repeat {
  Balanced <- groupdata2::balance(PCA_Coord,
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
  if (x >= 210){
    break
  }
}

# SVM Poly 
repeat {
  Balanced <- groupdata2::balance(PCA_Coord,
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
  if (x >= 240){
    break
  }
}

# C5.0 Tree 
repeat {
  Balanced <- groupdata2::balance(PCA_Coord,
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
  if (x >= 270){
    break
  }
}

# Naïve Bayes 
repeat {
  Balanced <- groupdata2::balance(PCA_Coord,
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
  if (x >= 300){
    break
  }
}

# Boosted Tree 
repeat {
  Balanced <- groupdata2::balance(PCA_Coord,
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
```

## 3. Results

## 7. References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-adler_early_2014" class="csl-entry">

Adler, D.S., Wilkinson, K.N., Blockley, S., Mark, D.F., Pinhasi, R.,
Schmidt-Magee, B.A., Nahapetyan, S., Mallol, C., Berna, F., Glauberman,
P.J., Raczynski-Henk, Y., Wales, N., Frahm, E., Jöris, O., MacLeod, A.,
Smith, V.C., Cullen, V.L., Gasparian, B., 2014. Early Levallois
technology and the Lower to Middle Paleolithic transition in the
Southern Caucasus. Science 345, 1609–1613.
<https://doi.org/10.1126/science.1256484>

</div>

<div id="ref-beyries_etude_1983" class="csl-entry">

Beyries, S., Boëda, E., 1983. Étude technoloogique et traces
d’utilisation des éclats débordants de Corbehem (Pas-de-Calais).
Bulletin de la Société préhistorique française 80, 275–279.
https://doi.org/<https://doi.org/10.3406/bspf.1983.5455>

</div>

<div id="ref-blinkhorn_directional_2021" class="csl-entry">

Blinkhorn, J., Groucutt, H.S., Scerri, E.M.L., Petraglia, M.D.,
Blockley, S., 2021. Directional changes in Levallois core technologies
between Eastern Africa, Arabia, and the Levant during MIS 5. Scientific
Reports 11, 11465. <https://doi.org/10.1038/s41598-021-90744-z>

</div>

<div id="ref-dibble_levallois:_1995" class="csl-entry">

Boëda, E., 1995. Levallois: A Volumetric Construction, Methods, A
Technique, in: Dibble, H.L., Bar-Yosef, O. (Eds.), The Definition and
Interpretation of Levallois Technology, Monographs in World Archaeology.
Prehistory Press, Madison, Wisconsin, pp. 41–68.

</div>

<div id="ref-boeda_concept_1994" class="csl-entry">

Boëda, E., 1994. Le concept Levallois: Variabilité des méthodes, CNRS
éditions. CNRS.

</div>

<div id="ref-boeda_debitage_1993" class="csl-entry">

Boëda, E., 1993. Le débitage discoïde et le débitage Levallois récurrent
centripède. Bulletin de la Société Préhistorique Française 90, 392–404.
<https://doi.org/10.3406/bspf.1993.9669>

</div>

<div id="ref-boeda_identification_1990" class="csl-entry">

Boëda, E., Geneste, J.-M., Meignen, L., 1990. Identification de chaînes
opératoires lithiques du Paléolithique ancien et moyen. Paléo 2, 43–80.

</div>

<div id="ref-bordes_typologie_1961" class="csl-entry">

Bordes, F., 1961. Typologie du Paléolithique ancien et moyen,
Publications de l’institut de préhistoire de l’université de Bordeaux.
CNRS Editions, Bordeaux.

</div>

<div id="ref-bordes_notules_1953" class="csl-entry">

Bordes, F., 1953. Notules de typologie paléolithique II : Pointes
Levalloisiennes et pointes pseudo-levalloisiennes. Bulletin de la
Société préhistorique de France 50, 311–313.
<https://doi.org/10.3406/bspf.1953.3057>

</div>

<div id="ref-bourguignon_analyse_1992" class="csl-entry">

Bourguignon, L., 1992. Analyse du processus opératoire des coups de
tranchet latéraux dans l’industrie moustérienne de l’abri du Musée (Les
Eyzies-de-Tayac, Dordogne). Paléo 4, 69–89.
<https://doi.org/10.3406/pal.1992.1195>

</div>

<div id="ref-bustillo_caracteristicas_2005" class="csl-entry">

Bustillo, M.A., Pérez-Jiménez, J.L., 2005. Características diferenciales
y génesis de los niveles silíceos explotados en el yacimiento
arqueológico de Casa Montero (Vicálvaro, Madrid). Geogaceta 38, 243–246.

</div>

<div id="ref-bustillo_caracterizacion_2012" class="csl-entry">

Bustillo, M.Á., Pérez-Jiménez, J.L., Bustillo, M., 2012. Caracterización
geoquímica de rocas sedimentarias formadas por silicificación como
fuentes de suministro de utensilios líticos (Mioceno, cuenca de Madrid).
Revista Mexicana de Ciencias Geológicas 29, 233–247.

</div>

<div id="ref-callahan_bipolar_1996" class="csl-entry">

Callahan, E., 1996. The bipolar technique: The simplest way to make
stone tools for survival. Bulletin of Primitive Technology 12, 16–20.

</div>

<div id="ref-cotterell_formation_1987" class="csl-entry">

Cotterell, B., Kamminga, J., 1987. The Formation of Flakes. American
Antiquity 52, 675–708.

</div>

<div id="ref-dibble_variability_1995" class="csl-entry">

Delagnes, A., 1995. Variability within Uniformity: Three Levels of
Variability within the Levallois System, in: Dibble, H.L., Bar-Yosef, O.
(Eds.), The Definition and Interpretation of Levallois Technology,
Monographs in World Archaeology. Prehistory Press, Madison, Wisconsin,
pp. 201–211.

</div>

<div id="ref-hovers_diversity_2006" class="csl-entry">

Delagnes, A., Meignen, L., 2006. Diversity of Lithic Production Systems
During the Middle Paleolithic in France. Are There Any Chronological
Trends?, in: Hovers, E., Kuhn, S.L., Jochim, M. (Eds.), Transitions
Before the Transition Evolution and Stability in the Middle Paleolithic
and Middle Stone Age. Springer, pp. 85–107.

</div>

<div id="ref-duran_lindustrie_2005" class="csl-entry">

Duran, J.-P., 2005. L’industrie moustérienne des ànecs (Rodès,
Pyrénées-orientales, France). PYRENAE 36, 11–39.

</div>

<div id="ref-duran_mousterien_2004" class="csl-entry">

Duran, J.-P., Abelanet, J., 2004. Un Moustérien Méditerranéen à bifaces:
Le gisement de Moutou-la-Joliette. Préhistoire Anthropologie
Méditerranéennes 13, 7–27.

</div>

<div id="ref-duran_variabilite_2006" class="csl-entry">

Duran, J.-P., Soler, N., 2006. Variabilité des modalités de débitage et
des productions lithiques dans les industries moustériennes de la grotte
de l’Arbreda, secteur alpha (Serinyà, Espagne). Bulletin de la Société
Préhistorique Française 103, 241–262.

</div>

<div id="ref-fernandes_silex_2012" class="csl-entry">

Fernandes, P., Morala, A., Schmidt, P., Séronie-Vivien, M.-R., Turq, A.,
2012. Le silex du Bergeracois: État de la question. Quaternaire
Continental d’Aquitaine, excursion AFEQ, ASF 2012 2012, 22–33.

</div>

<div id="ref-rigaud_les_1988" class="csl-entry">

Geneste, J.-M., 1988. Les Industries De La Grotte Vaufrey: Technologie
du debitage, economie et circulation de la matiere premiere lithique,
in: Rigaud, J.-P. (Ed.), La Grotte Vaufrey à Cenac Et Saint-Julien
(Dordogne) : Paléoenvironnements, Chronologie Et Activités Humaines,
Mémoires de La Société Préhistorique Française (Revue). Société
préhistorique française, Paris, pp. 441–517.

</div>

<div id="ref-hayden_confusion_1980" class="csl-entry">

Hayden, B., 1980. Confusion in the Bipolar World: Bashed Pebbles and
Splintered Pieces. Lithic Technology 9, 2–7.
<https://doi.org/10.1080/01977261.1980.11754456>

</div>

<div id="ref-kuhn_roots_2013" class="csl-entry">

Kuhn, S.L., 2013. Roots of the Middle Paleolithic in Eurasia. Current
Anthropology 54, S255–S268. <https://doi.org/10.1086/673529>

</div>

<div id="ref-dibble_recurrent_1995" class="csl-entry">

Lenoir, M., Turq, A., 1995. Recurrent Centripetal Debitage (Levallois
and Discoidal): Continuity or Discontinuity?, in: Dibble, H.L.,
Bar-Yosef, O. (Eds.), The Definition and Interpretation of Levallois
Technology, Monographs in World Archaeology. Prehistory Press, Madison,
Wisconsin, pp. 249–256.

</div>

<div id="ref-meignen_persistance_1996" class="csl-entry">

Meignen, L., 1996. Persistance des traditions techniques dans l’abri des
Canalettes (Nant-Aveyron). Quaternaria Nova 6, 449–64.

</div>

<div id="ref-meignen_les_1993" class="csl-entry">

Meignen, L., 1993. Les industries lithiques de l’abri des Canalettes:
Cuche 2, in: Meignen, L. (Ed.), L’abri Des Canalettes. Un Habitat
Moustérien Sur Les Grands Causses (Nant-Aveyron), Monographie Du CRA.
CNRS Ed., Paris, pp. 238–328.

</div>

<div id="ref-pasty_etude_2004" class="csl-entry">

Pasty, J.-F., Liegard, S., Alix, P., 2004. Étude de l’industrie lithique
du site paléolithique moyen des Fendeux (Coulanges, Allier). Bulletin de
la Société préhistorique française 101, 5–25.
<https://doi.org/10.3406/bspf.2004.12945>

</div>

<div id="ref-pelegrin_technologie_1995" class="csl-entry">

Pelegrin, J., 1995. Technologie lithique: Le Châtelperronien de
Roc-de-Combe (Lot) et de la Côte (Dordogne). CNRS, Paris.

</div>

<div id="ref-raab_debitage_1979" class="csl-entry">

Raab, L.M., Cande, R.F., Stahle, D.W., 1979. Debitage graphs and archaic
settlement patterns in the Arkansas Ozarks. Midcontinental Journal of
Archaeology 4, 167–182.

</div>

<div id="ref-romagnoli_neanderthal_2022" class="csl-entry">

Romagnoli, F., Chabai, V., Hérisson, D., Hovers, E., Moncel, M.-H.,
Peresani, M., Uthmeier, T., Bourguignon, L., Chacón, M.G., Di Modica,
K., Faivre, J.-P., Malinsky-Buller, A., Neruda, P., Rios Garaizar, J.,
Weiss, Ma., Wiśniewski, A., Wragg Sykes, R., 2022. Neanderthal
technological variability: A wide-range geographical perspective of the
final Middle Palaeolithic, in: Romagnoli, F., Rivals, F., Benazzi, S.
(Eds.), Updating Neanderthals. Understanding Behavioral Complexity in
the Late Middle Paleolithic. Academic Press.

</div>

<div id="ref-shea_middle_2013" class="csl-entry">

Shea, J., J., 2013b. The Middle Paleolithic, in: Stone Tools in the
Paleolithic and Neolithic Near East : A Guide. Cambridge University
Press, New York, pp. 82–116.

</div>

<div id="ref-shea_upper_2013" class="csl-entry">

Shea, J., J., 2013a. The Upper Paleolithic, in: Stone Tools in the
Paleolithic and Neolithic Near East : A Guide. Cambridge University
Press, New York, pp. 117–160.

</div>

<div id="ref-peresani_les_2003" class="csl-entry">

Slimak, L., 2003. Les Debitages discoïdes mousteriens: Evaluation d’un
concept technologique, in: Peresani, M. (Ed.), Discoid Lithic
Technology. Advances and Implications, BAR International Series.
Archaeopress, Oxford, pp. 33–65.

</div>

<div id="ref-tixier_prehistoire_1980" class="csl-entry">

Tixier, J., Inizan, M.-L., Roche, H., 1980. Préhistoire de la pierre
taillée. 1.Terminologie et Technologie, 2nd ed. Cercle de Recherches et
d’études Préhistoriques.

</div>

<div id="ref-tixier_kombewa_1999" class="csl-entry">

Tixier, J., Turq, A., 1999. Kombewa et alii. Paléo 11, 135–143.
<https://doi.org/10.3406/pal.1999.1174>

</div>

</div>
