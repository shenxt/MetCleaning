# MetCleaning
******************************************
### *MetCleaning* provides a comprehensive pipeline for data cleaning and statistical analysis of large-scale mass spectrometry (MS) based-metabolomics data. It includes missing value (MV) filtering and imputation, zero value filtering, detection of sample outliers, data normalization, data integration, data quality assessment, and common statistical analysis such as univariate and multivariate statistical analysis. This document describes the step-by-step processing metabolomics data using *MetCleaning*.

## **Installation and help**
******************************************
### *MetCleaning* is published in github [(link)](https://github.com/jaspershen/MetCleaning). So you can install it via to github. 

```
##pcaMethods, pathifier and impute should be installed form bioconductor
##pcaMethos
source("http://bioconductor.org/biocLite.R")
    biocLite("pcaMethods")
##pathifier
source("http://bioconductor.org/biocLite.R")
    biocLite("pathifier")
##impute
source("http://bioconductor.org/biocLite.R")
    biocLite("impute")
 if(!require(devtools)) {
  install.packages("devtools")
 }
 library(devtools)
 install_github("jaspershen/MetCleaning")
 library(MetCleaning)
 help(package = "MetCleaning")
```

## **Note of version0.99.0**
1. Add *m2p* function which is used to transform metabolite information to pathway information. 
2. Fix some bugs to improve speed.(20161219)
3. Add the check.names = FALSE (argument) in all the read.csv function. (20161221)
4. New feature for ms1 and ms2 matching only.
5. Add the step selection function n **MetCleaning**.