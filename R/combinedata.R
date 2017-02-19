#' @title combinedata
#' @description Combine several MetFlowData into one data.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param ... MetFlowData that you want to combine.
#' @return Return a combination MetFlowData.
#' @export

combinedata <- function(...){
  data <- list(...)
  subject <- data[[1]][["subject"]]
  qc <- data[[1]][["qc"]]
  tags <- data[[1]][["tags"]]

  for (i in 2:length(data)) {
    subject <- rbind(subject, data[[i]][["subject"]])
    qc <- rbind(qc, data[[i]][["qc"]])
    tags <- rbind(tags, data[[i]][["tags"]])
  }

  new.data <- data[[1]]
  new.data[["subject"]] <- subject
  new.data[["qc"]] <- qc
  new.data[["tags"]] <- tags
  return(new.data)
}