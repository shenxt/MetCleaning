# MetCleaning
******************************************
### *MetCleaning* provides an integrated and automatic pipeline for data cleaning and statistical analysis of large scale mass spectrometry (MS) based-metabolomic data. It includes missing value (MV) filtering and imputation, zero value filtering, data normalization, data integration, data quality assessment, univariate statistical analysis, multivariate statistical analysis such as PCA and PLS-DA, potential marker selection and show. This document describes how to use the integrated functions, *MetClean* and *MetStat* in *MetCleaning* utilizing demo data.

## **Installation and help**
******************************************
### *MetCleaning* is published in github [(link)](https://github.com/jaspershen/MetCleaning). So you can install it via to github. 

```
##pcaMethods and impute should be installed form bioconductor
##pcaMethos
source("http://bioconductor.org/biocLite.R")
    biocLite("pcaMethods")
##impute
source("http://bioconductor.org/biocLite.R")
    biocLite("impute")
 if(!require(devtools)) {
  install.packages("devtools")
 }
 library(devtools)
 install_github("jaspershen/MetCleaning"，ref = "version1.1.0")
 library(MetCleaning)
 help(package = "MetCleaning")
```

## **Note of version1.0.0**
## **Note of version1.1.0**
1. Add *m2p* function which is used to transform metabolite information to pathway information. 
2. Fix some bugs to improve speed.(20161219)
3. Add the check.names = FALSE (argument) in all the read.csv function. (20161221)
4. New feature for ms1 and ms2 matching only.
5. Add the step selection function n **MetClean**.