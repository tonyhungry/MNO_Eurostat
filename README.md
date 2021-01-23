# MNO Eurostat

## Description

This repository serves as a reseearch repository used for the Eurostat Summer Coding Labs - MNO Project 2020. MNO stands for Mobile Network Operating Data. The main goal is to recreate, test and gain new insights about current estimation methods for **estimating present population based on MNO data**. To do this, we have developed our own toy world (simulation framework) with multiple parameters to exactly measure estimation error. The structure of this project as well as this research repository is designed in a modular way, in order to increase future flexibility. Currently, we use as a basis for geographic data and population numbers, a small part of South East Germany, which is based on Census Data. The modular structure allows that if new data is available or if one is interested in a different part of the world, one only needs to change the data source at the beginning.

The authors of this repository include: Giulia Regazzoni (UniBg, IT), Marco Ramljak* (UU, NL), Stefania Scrofani (UniBg, IT), and Tony Hung (VŠE, CZ).

## Important links

HTML version of our working notebook *(currently under development)*: https://r-ramljak.github.io/MNO_Eurostat/ or https://bookdown.org/tonyhung_th/bookdown/executive-summary.html

For a full overview of our work, please visit our book *(currently under development)*: https://bookdown.org/tonyhung_th/bookdown/

This link provides more information regarding the topic of the MNO Project (p8-9): http://mse.ec.unipi.it/wp-content/uploads/2020/06/Eurostat-coding-lab_call_final-040620.pdf 

## Reproducibility

As this is a simulation study, no data needs to be protected and everything can be shared upon request. Currently, the only *real* data source used, which was not simulated, is the population generation which is based on German Census Data. Result objects based on the current workflow with the specified parameters used in our notebook can be provided upon request and therefore fully reproduced. As some of the modeled results objects are very large (>5gb each) they are currently uploaded in the student version of our Microsoft One drive account. As these clouds are university bound it is difficult to share them with non-university members, therefore, if objects are needed, they will be sent via compressed files.

To reproduce our results, run the numbered R-scripts, which also resemble the modules in our modular design, in [code](https://github.com/R-ramljak/MNO_Eurostat/tree/master/code) in corresponding order. Scripts that contain results of random nature (random variable, etc.) have a `set.seed()` command implemented. If the seed is changed, results will most likely differ.

Content Code relies on the R-language and the following session info
```r
sessionInfo()

R version 4.0.3 (2020-10-10)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19041)

Matrix products: default

locale:
[1] LC_COLLATE=German_Germany.1252  LC_CTYPE=German_Germany.1252   
[3] LC_MONETARY=German_Germany.1252 LC_NUMERIC=C                   
[5] LC_TIME=German_Germany.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.5       rstudioapi_0.11  knitr_1.30       raster_3.3-7    
 [5] magrittr_1.5     tidyselect_1.1.0 lattice_0.20-41  R6_2.4.1        
 [9] rlang_0.4.6      stringr_1.4.0    dplyr_1.0.0      tools_4.0.3     
[13] grid_4.0.3       xfun_0.18        htmltools_0.5.0  ellipsis_0.3.1  
[17] yaml_2.2.1       digest_0.6.25    tibble_3.0.1     lifecycle_0.2.0 
[21] crayon_1.3.4     purrr_0.3.4      vctrs_0.3.1      codetools_0.2-16
[25] rsconnect_0.8.16 glue_1.4.1       evaluate_0.14    rmarkdown_2.5   
[29] sp_1.4-2         stringi_1.4.6    pander_0.6.3     compiler_4.0.3  
[33] pillar_1.4.4     generics_0.0.2   jsonlite_1.7.0   pkgconfig_2.0.3 
```

\* corresponding author
