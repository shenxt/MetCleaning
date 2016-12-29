test_GetWorklist=function(){
  file <- system.file("extdata/batch.design.csv",package = "MetCleaning")
  x <- read.csv(file, check.names = FALSE, stringsAsFactors = FALSE)
  checkEquals(GetWorklist(x),TRUE)
}
