#' @title Print.MetFlowData
#' @description Print method for class in MetProcesser.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param object MetFlowData.
#' @return Print some information of class object in your screen.
#' @export
#' @seealso \code{\link{print}}
#' @examples
#' data("met.data", package = "MetCleaning")
#' met.data


## MetFlowData
setMethod("show", "MetFlowData",
          function(object){
            hasQC <- object@"hasQC"
            subject <- object@"subject"
            qc <- object@"qc"
            tags <- object@"tags"
            subject.info <- object@"subject.info"
            qc.info <- object@"qc.info"
            subject.order <- object@"subject.order"
            qc.order <- object@"qc.order"

            subject.batch <- as.numeric(subject.info[,4])
            subject.name <- subject.info[,1]

            if(hasQC == "no") {qc.batch <- NULL; qc.name <- NULL}
            else {
              qc.batch <- as.numeric(qc.info[,4])
              qc.name <- qc.info[,1]
            }

            unique.batch <- unique(subject.batch)
            summary.subject.batch <- table(subject.batch)
            if(hasQC == "no") {summary.qc.batch <- NULL}
            else {
              summary.qc.batch <- table(qc.batch)
            }
            summary.batch <- cbind(summary.subject.batch, summary.qc.batch)

            ## batch information
            cat("There are", length(unique.batch),
                ifelse(length(unique.batch)==1, "batch", "batches"))
            cat("\n")

            for (i in 1:length(unique.batch)){
              cat(paste("Batch",i,":\n"))
              cat(paste("Subject sample number:",summary.batch[i,1]))
              cat("\n")
              if (hasQC == "no") {cat(paste("QC sample number:",0))}
              else {
                cat(paste("QC sample number:",summary.batch[i,2]))
              }
              cat("\n")
            }

            ## peak information
            cat("---------------\n")
            cat(paste("Peak number:",nrow(subject)))
            cat("\n")
            cat("The tags information contains:\n")
            cat(colnames(tags))
            cat('\n')

            ## subject sample information
            cat("---------------\n")
            cat("Subject sample info:\n")
            cat(paste("Subject sample number:", ncol(subject)))
            cat("\n")
            cat("The Subject information contains:\n")
            cat(colnames(subject.info))
            cat('\n')

            ## QC sample information
            if (hasQC != "no") {
              cat("---------------\n")
              cat("QC sample info:\n")
              cat(paste("QC sample number:", ncol(qc)))
              cat("\n")
              cat("---------------\n")
            }

            ## other processing information
            cat("MV imputation:",object@"mv.imputation")
            cat("\n")
            cat("Imputation method:",object@"imputation.method")
            cat("\n")
            cat("Zero filter:",object@"zero.filter")
            cat("\n")
            cat("Zero filter criteria:",object@"zero.filter.criteria")
            cat("\n")
            cat("QC outlier filter:",object@"qc.outlier.filter")
            cat("\n")
            cat("Normalization:",object@"normalization")
            cat("\n")
            cat("Normalization method:",object@"normalization.method")
            cat("\n")
            cat("Data integration:",object@"data.integration")
            cat("\n")
            cat("Data integration method:",object@"data.integration.method")
            cat("\n")
            cat("Has IS:",object@"hasIS")
            cat("\n")
            cat("Has QC:",object@"hasQC")
            cat("\n")
            cat("Peak identification:",object@"peak.identification")
            cat("\n")
          })

