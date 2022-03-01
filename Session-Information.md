## Session information

``` r
list.of.packages <- c("tidyverse", "caret",  "ranger", "knitr", 
                      "groupdata2")
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
    ##  [1] "ranger"    "caret"     "lattice"   "forcats"   "stringr"   "dplyr"    
    ##  [7] "purrr"     "readr"     "tidyr"     "tibble"    "ggplot2"   "tidyverse"
    ## [13] "stats"     "graphics"  "grDevices" "utils"     "datasets"  "methods"  
    ## [19] "base"     
    ## 
    ## [[4]]
    ##  [1] "knitr"     "ranger"    "caret"     "lattice"   "forcats"   "stringr"  
    ##  [7] "dplyr"     "purrr"     "readr"     "tidyr"     "tibble"    "ggplot2"  
    ## [13] "tidyverse" "stats"     "graphics"  "grDevices" "utils"     "datasets" 
    ## [19] "methods"   "base"     
    ## 
    ## [[5]]
    ##  [1] "groupdata2" "knitr"      "ranger"     "caret"      "lattice"   
    ##  [6] "forcats"    "stringr"    "dplyr"      "purrr"      "readr"     
    ## [11] "tidyr"      "tibble"     "ggplot2"    "tidyverse"  "stats"     
    ## [16] "graphics"   "grDevices"  "utils"      "datasets"   "methods"   
    ## [21] "base"

``` r
rm(list.of.packages)
```

``` r
sessionInfo()
```

    ## R version 4.1.2 (2021-11-01)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 19043)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=Spanish_Spain.1252  LC_CTYPE=Spanish_Spain.1252   
    ## [3] LC_MONETARY=Spanish_Spain.1252 LC_NUMERIC=C                  
    ## [5] LC_TIME=Spanish_Spain.1252    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] groupdata2_2.0.0 knitr_1.36       ranger_0.13.1    caret_6.0-90    
    ##  [5] lattice_0.20-45  forcats_0.5.1    stringr_1.4.0    dplyr_1.0.7     
    ##  [9] purrr_0.3.4      readr_2.1.1      tidyr_1.1.4      tibble_3.1.6    
    ## [13] ggplot2_3.3.5    tidyverse_1.3.1 
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] nlme_3.1-153         fs_1.5.2             lubridate_1.8.0     
    ##  [4] httr_1.4.2           tools_4.1.2          backports_1.4.1     
    ##  [7] utf8_1.2.2           R6_2.5.1             rpart_4.1-15        
    ## [10] DBI_1.1.1            colorspace_2.0-2     nnet_7.3-16         
    ## [13] withr_2.4.3          tidyselect_1.1.1     compiler_4.1.2      
    ## [16] cli_3.1.0            rvest_1.0.2          xml2_1.3.3          
    ## [19] scales_1.1.1         digest_0.6.29        rmarkdown_2.11      
    ## [22] pkgconfig_2.0.3      htmltools_0.5.2      parallelly_1.29.0   
    ## [25] dbplyr_2.1.1         fastmap_1.1.0        rlang_0.4.12        
    ## [28] readxl_1.3.1         rstudioapi_0.13      generics_0.1.1      
    ## [31] jsonlite_1.7.2       ModelMetrics_1.2.2.2 magrittr_2.0.1      
    ## [34] Matrix_1.3-4         Rcpp_1.0.7           munsell_0.5.0       
    ## [37] fansi_0.5.0          lifecycle_1.0.1      stringi_1.7.6       
    ## [40] pROC_1.18.0          yaml_2.2.1           MASS_7.3-54         
    ## [43] plyr_1.8.6           recipes_0.1.17       grid_4.1.2          
    ## [46] parallel_4.1.2       listenv_0.8.0        crayon_1.4.2        
    ## [49] haven_2.4.3          splines_4.1.2        hms_1.1.1           
    ## [52] pillar_1.6.4         future.apply_1.8.1   reshape2_1.4.4      
    ## [55] codetools_0.2-18     stats4_4.1.2         reprex_2.0.1        
    ## [58] glue_1.5.1           evaluate_0.14        data.table_1.14.2   
    ## [61] modelr_0.1.8         vctrs_0.3.8          tzdb_0.2.0          
    ## [64] foreach_1.5.1        cellranger_1.1.0     gtable_0.3.0        
    ## [67] future_1.23.0        assertthat_0.2.1     xfun_0.29           
    ## [70] gower_0.2.2          prodlim_2019.11.13   broom_0.7.10        
    ## [73] class_7.3-19         survival_3.2-13      timeDate_3043.102   
    ## [76] iterators_1.0.13     lava_1.6.10          globals_0.14.0      
    ## [79] ellipsis_0.3.2       ipred_0.9-12
