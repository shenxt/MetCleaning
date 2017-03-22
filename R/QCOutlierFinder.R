
#QC outlier filtering according to zero ratio and PCA
QCOutlierFinder <- function(MetFlowData,
                            CI = 0.95,
                            path = ".") {
  # browser()
  options(warn = -1)
  if (path != ".") {
    dir.create(path)
  }
  qc <- MetFlowData@qc
  qc.info <- MetFlowData@qc.info
  tags <- MetFlowData@tags

  data <- SplitBatch(MetFlowData = MetFlowData)
  qc1 <- data[[2]]

  obs.remove <- list()
  ## PCA analysis
  for (i in seq_along(qc1)) {
    info <- list("QC" = colnames(qc1[[i]]))
    SXTpcaData <-
      SXTpca(
        subject = qc1[[i]],
        info = info,
        QC = FALSE,
        scale.method = "auto"
      )
    index2 <- SXTpcaFindOutlier(
      SXTpcaData = SXTpcaData,
      CI = CI,
      plot.name = paste("Batch", i, "outliers"),
      output.plot = TRUE,
      path = path
    )
    # browser()
    ## The first and last QC shouldn't be removed
    if (any(c(1, ncol(qc1[[i]])) %in% index2)) {
      index2 <-
  index2[-match(c(1, ncol(qc1[[i]])), index2)[!is.na(match(c(1, ncol(qc1[[i]])), index2))]]
    }

    obs.remove[[i]] <- index2
  }
  QCOutlierFinderData <- list(MetFlowData = MetFlowData,
                              obs.remove = obs.remove)
  class(QCOutlierFinderData) <- "QCOutlierFinderData"
  options(warn = 1)
  return(QCOutlierFinderData)
}
