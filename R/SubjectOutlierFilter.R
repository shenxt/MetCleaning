#' @title SubjectOutlierFilter
#' @description Using PCA to filter subject outliers.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param MetFlowData MetFlowData.
#' @param CI Confidence interva.
#' @param path Work directory.
#' @return MetFlowData whose subject outliers have been removed.
#' @seealso \code{\link{QCOutlierFilter}}
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
#'## run
#'new.met.data <- SubjectOutlierFilter(met.data.after.pre,
#'                                     path = "Demo for SubjectOutlierFilter")
#' }

SubjectOutlierFilter <- function(MetFlowData,
                                 CI = 0.95,
                                 path = ".") {
  SubjectOutlierFiderData <- SubjectOutlierFinder(MetFlowData = MetFlowData,
                                        CI = CI,
                                        path = path)

  metData <- SubjectOutlierFiderData[[1]]
  obs.remove <- SubjectOutlierFiderData[[2]]

  data <- SplitBatch(MetFlowData = metData)
  subject1 <- data[[1]]

  for (i in seq_along(subject1)) {
    cat(paste("Batch",i))
    cat("\n")
    cat("-------------------------------------------\n")
    temp.subject <- subject1[[i]]
    temp.idx <- obs.remove[[i]]
    if (length(temp.idx) != 0) {
      # browser()
       cat("Subject shoulde be removed are:")
      cat(temp.idx)
      cat("\n")
      temp.idx <-
        readline(
    "Which subject you want to remove(please type the index of subject sample,
          and separate them using comma,
          if you don't want to remove any subject, please type n):"
        )

      if (temp.idx == "n") {
        temp.subject <- temp.subject
      } else {
        temp.idx <- strsplit(temp.idx, split = ",")
        temp.idx <- as.numeric(temp.idx[[1]])
        temp.idx <- as.numeric(temp.idx)
        temp.subject <- temp.subject[, -temp.idx]
      }
    } else {
      temp.subject <- temp.subject
    }
    subject1[[i]] <- temp.subject
  }


  subject2 <- subject1[[1]]
  if (length(subject1) > 1) {
    for (i in 2:length(subject1)) {
      subject2 <- cbind(subject2, subject1[[i]])
    }
  }

  ##remove subject information who have been removed from data
  subject.name <- colnames(subject2)
  subject.info <- metData@subject.info
  subject.order <- metData@subject.order
  subject.index <- which(is.na(match(subject.info[, 1], subject.name)))

  if (length(subject.index) != 0) {
    subject.info <- subject.info[-subject.index, ]
    subject.order <- subject.order[-subject.index]
  }

  metData@subject.info <- as.matrix(subject.info)
  metData@subject <- as.matrix(subject2)
  metData@subject.order <- as.numeric(subject.order)
  metData@subject.outlier.filter <- "yes"
  return(metData)

  }
