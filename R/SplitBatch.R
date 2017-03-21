#' @title SplitBatch
#' @description Split MetFlowData accoding to different batch.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param MetFlowData MetFlowData.
#' @return Return a data (list), subject, qc, subject.info and qc.info.
#' @export
#' @examples
#' \donttest{
#' #load the demo data
#' data(data, package = "MetCleaning")
#' data(sample.information, package = "MetCleaning")
#'
#' ##create a folder for demo
#' dir.create("demo")
#' setwd("demo")
#'
#' # export the demo data as csv
#' write.csv(data, "data.csv", row.names = FALSE)
#' write.csv(sample.information, "sample.information.csv", row.names = FALSE)
#'
#' # MetCleaning process
#' MetCleaning(#ImportData para
#' data = "data.csv",
#' sample.information = "sample.information.csv",
#' polarity = "positive",
#' #DataNormalization
#' method = "svr",
#' threads = 2)
#'new.data <- SplitBatch(met.data.after.pre)
#' }

SplitBatch <- function(MetFlowData) {
  ## split batch
  # browser()
  hasQC <- MetFlowData[["hasQC"]]
  qc <- MetFlowData[["qc"]]
  subject <- MetFlowData[["subject"]]
  qc.info <- MetFlowData[["qc.info"]]
  subject.info <- MetFlowData[["subject.info"]]
  tags <- MetFlowData[["tags"]]

  subject.name <- as.character(subject.info[, 1])
  if (hasQC != "no") {
    qc.name <- as.character(qc.info[, 1])
  }
  else {
    qc.name <- NULL
  }

  subject.batch <- as.numeric(subject.info[, 4])
  if (hasQC != "no") {
    qc.batch <- as.numeric(qc.info[, 4])
  }
  else {
    qc.batch <- NULL
  }

  subject1 <- list()
  qc1 <- list()
  subject.info1 <- list()
  qc.info1 <- list()
  # browser()
  for (i in 1:length(unique(subject.batch))) {
    subject.name.for.this.batch <- subject.name[subject.batch == i]
    if (hasQC == "no") {
      qc.name.for.this.batch <- NULL
    }
    else {
      qc.name.for.this.batch <- qc.name[qc.batch == i]
    }
    subject.index.for.this.batch <-
      match(subject.name.for.this.batch, colnames(subject))
    subject.index.for.this.batch <-
      subject.index.for.this.batch[!is.na(subject.index.for.this.batch)]
    if (hasQC == "no") {
      qc.index.for.this.batch <- NULL
    }
    else {
      qc.index.for.this.batch <-
        match(qc.name.for.this.batch, colnames(qc))
      qc.index.for.this.batch <-
        qc.index.for.this.batch[!is.na(qc.index.for.this.batch)]
    }

    subject1[[i]] <- subject[, subject.index.for.this.batch]
    subject.info1[[i]] <- subject.info[subject.batch == i, ]
    if (hasQC == "no") {
      qc1[[i]] <- NULL
      qc.info1[[i]] <- NULL
    }
    else {
      qc1[[i]] <- qc[, qc.index.for.this.batch]
      qc.info1[[i]] <- qc.info[qc.batch == i, ]
    }
  }

  data <- list(subject1, qc1, subject.info1, qc.info1)
  return(data)
}