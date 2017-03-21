#' @title ExportData
#' @description Export MetFlowData as csv.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param MetFlowData MetFlowData.
#' @param data.name The name of the data you want to output. Default is
#' "data_new".
#' @param subject.info.name The name for subject information you want to
#' output. Default is "subject.info".
#' @param qc.info.name The name for QC information you want to output.
#' Default is "qc.info".
#' @param path Work directory.
#' @return Write csv data.
#' @seealso \code{\link{ImportData}}
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
#'## run
#'ExportData(MetFlowData = met.data.after.pre, path = "Demo for ExportData")
#'}

ExportData <- function(MetFlowData,
                       data.name = "data_new",
                       subject.info.name = "subject.info",
                       qc.info.name = "qc.info",
                       path = NULL){
  if (is.null(path)) {path <- getwd()}
  else {dir.create(path)}

  subject <- MetFlowData[["subject"]]
  qc <- MetFlowData[["qc"]]
  tags <- MetFlowData[["tags"]]
  subject.info <- MetFlowData[["subject.info"]]
  qc.info <- MetFlowData[["qc.info"]]

  has.qc <- MetFlowData[["hasQC"]]

  if(has.qc == "yes"){
  write.csv(cbind(tags, subject, qc), file.path(path,paste(data.name,".csv", sep = "")), row.names = FALSE)
  write.csv(subject.info, file.path(path,paste(subject.info.name,".csv", sep = "")), row.names = FALSE)
  write.csv(qc.info, file.path(path,paste(qc.info.name,".csv", sep = "")), row.names = FALSE)
  }else{
    write.csv(cbind(tags, subject), file.path(path,paste(data.name,".csv", sep = "")), row.names = FALSE)
    write.csv(subject.info, file.path(path,paste(subject.info.name,".csv", sep = "")), row.names = FALSE)
  }
}