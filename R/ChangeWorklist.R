#' @title ChangeWorklist
#' @description Change date in worklist from GetWorklist.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param date Date you want to change in you worklist. Default is today.
#' @return Return a new worklist.
#' @examples
#' \dontrun{
#' #demo data
#' batch.design <- paste("A",c(1:100), sep = "")
#' ##create a folder for demo
#' dir.create("Demo")
#' setwd("Demo")
#' write.csv(batch.design, "batch.design.csv", row.names = FALSE)
#' #get worklist
#' GetWorklist()
#' ##remove other information
#' file.remove(dir()[dir()!="worklist POS.csv"])
#' #run ChangeWorklist
#' ChangeWorklist()
#' }

ChangeWorklist <-
  function(date = gsub("-", "", as.character(Sys.Date()))) {
    #used to change the date in worklist
    options(warn = -1)
    #change the date
    file <- dir()
    file <- file[!file.info(file)$isdir]

      worklist <- read.csv(file, check.names = FALSE, stringsAsFactors = FALSE)



    Data.File <- worklist[, grep("Data.File", colnames(worklist))]
    date.old <- substr(file, 1, 8)
    Data.File <- gsub(date.old, date, Data.File)
    worklist[, grep("Data.File", colnames(worklist))] <- Data.File
    worklistname <- paste(date, substr(file, 9, (nchar(file) - 5)), sep =
                            "")

    write.csv(worklist, sprintf('%s.csv', worklistname), row.names = FALSE)
    cat("The date has been changed.\n")
  }