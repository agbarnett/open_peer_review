# Who chooses open peer review?

Research Questions: 

1. What are the characteristics of articles at _PLOS ONE_ where the authors chose open peer review?

2. Does choosing open peer review predict research quality via citations and retractions?

XML data downloaded from [PLOS](https://plos.org/resource/open-peer-review/). Before publication, authors can choose to have the peer review published alongside their paper, providing the binary outcome of opting in to open peer review (yes/no).

The _R_ files are in order of execution with a prefix of:

* 0_ read in the XML files and exclude papers that were not peer reviewed
* 1_ process the authors' countries and papers' topics
* 2_ run the statistical models for the quality outcomes (citations and retractions); add author paper counts; random sample of reviews
* 3_ prepare data and run stability selection
* 4_ report main model and run model checks
* 99_ other files, including some functions

The subfolders are:

* `R` for R functions.
* `checks` for random checks of the data.
* `data` data in _R_ format.
* `figures` for figures.
* `results` for results in RData format, latex-ready tables, rmarkdown reports.
* `reviews` a random selection of the open peer reviews

<details><summary>R version and packages</summary>
<p>

```
R version 4.4.3 (2025-02-28 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 11 x64 (build 26100)

Matrix products: default


locale:
[1] LC_COLLATE=English_Australia.utf8  LC_CTYPE=English_Australia.utf8   
[3] LC_MONETARY=English_Australia.utf8 LC_NUMERIC=C                      
[5] LC_TIME=English_Australia.utf8    

time zone: Australia/Brisbane
tzcode source: internal

attached base packages:
[1] parallel  stats     graphics  grDevices utils     datasets  methods  
[8] base     

other attached packages:
[1] broom_1.0.10  stabs_0.6-4   lars_1.3      stringr_1.6.0 dplyr_1.1.4  

loaded via a namespace (and not attached):
 [1] backports_1.5.0    tidyr_1.3.1        R6_2.6.1          
 [4] tidyselect_1.2.1   magrittr_2.0.4     glue_1.8.0        
 [7] tibble_3.3.0       pkgconfig_2.0.3    generics_0.1.4    
[10] lifecycle_1.0.4    cli_3.6.5          vctrs_0.6.5       
[13] withr_3.0.2        compiler_4.4.3     purrr_1.2.0       
[16] rstudioapi_0.17.1  tools_4.4.3        pillar_1.11.1     
[19] TeachingDemos_2.13 rlang_1.1.6        stringi_1.8.7 
```

</p>
</details>