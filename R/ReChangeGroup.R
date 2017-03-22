#' @title ReChangeGroup
#' @description Change the group information in MetFlowData for statistical
#' analysis.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param MetFlowData MetFlowData.
#' @param new.group New Group information name. Default is new.group.csv.
#' It can be use the sample.information which is only changed the group column.
#' @return Return a standard MetProcesser data which is changed the
#' group informatio.
#' @examples
#' \donttest{
#' #load the demo data
#' data(data, package = "MetCleaning")
#' data(sample.information, package = "MetCleaning")
#' data(new.group, package = "MetCleaning")
#'
#' ##create a folder for demo
#' dir.create("demo")
#' setwd("demo")
#'
#' # export the demo data as csv
#' write.csv(data, "data.csv", row.names = FALSE)
#' write.csv(sample.information, "sample.information.csv", row.names = FALSE)
#' write.csv(new.group, "new.group.csv", row.names = FALSE)
#'
#' # MetCleaning process
#' MetCleaning(#ImportData para
#' data = "data.csv",
#' sample.information = "sample.information.csv",
#' polarity = "positive",
#' #DataNormalization
#' method = "svr",
#' threads = 2)
#'
#' #run
#' new.met.data <- ReChangeGroup(met.data.after.pre)
#' }

ReChangeGroup <- function(MetFlowData,
                          new.group = "new.group.csv") {
  # browser()
  new.group <-
    read.csv(new.group, stringsAsFactors = FALSE, check.names = FALSE)

  subject.info <- MetFlowData@subject.info
  subject <- MetFlowData@subject
  subject.order <- MetFlowData@subject.order

  ##which sample you want to remove from the dataset
  remove.name <- as.character(new.group[,1][which(is.na(new.group[,"group"]))])
  if(length(remove.name) != 0) {
  cat("The samples you want to remove from dataset are:\n")
  cat(remove.name)
  right <- readline("Right(y) or wrong(n)?")
  if (right == "n") {
    cat("Please change your new group information again.\n")
    return(MetFlowData)}
  }

# browser()

  ##remove the NA from new.group inforamtion
  new.group <- new.group[which(!is.na(new.group[,"group"])), ]
  new.subject.info <- new.group[new.group[, "class"] == "Subject", ]
  new.subject.name <- new.subject.info[, 1]

  ##remove samples from MetFlowData
  if (length(remove.name) != 0) {
    remove.idx <- match(remove.name, subject.info[,1])
    remove.idx <- remove.idx[!is.na(remove.idx)]
    subject <- subject[,-remove.idx]
    subject.info <- subject.info[-remove.idx,]
    subject.order <- subject.order[-remove.idx]
  }

  subject.name <- subject.info[, 1]

  ##change group information
  index <- match(subject.name, new.subject.name)
  new.subject.info <- new.subject.info[index, ]

  MetFlowData@subject.info <- as.matrix(new.subject.info)
  MetFlowData@subject <- as.matrix(subject)
  MetFlowData@subject.order <- as.numeric(subject.order)
  return(MetFlowData)
}