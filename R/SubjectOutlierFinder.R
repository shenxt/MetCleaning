
#subject outlier filtering according to zero ratio and PCA
SubjectOutlierFinder <- function(MetFlowData,
                                 CI = 0.95,
                                 path = "."){
  options(warn = -1)
  # browser()
  if (path != ".") {
    dir.create(path)
  }
  subject <- MetFlowData[["subject"]]
  subject.info <- MetFlowData[["subject.info"]]
  tags <- MetFlowData[["tags"]]

data <- SplitBatch(MetFlowData = MetFlowData)
subject1 <- data[[1]]

obs.remove <- list()
  ## PCA analysis
  for (i in 1:seq_along(subject1)) {
    info <- list("Subject" = colnames(subject1[[i]]))
    SXTpcaData <- SXTpca(subject = subject1[[i]], info = info, QC = FALSE, scale.method = "auto")
    index2 <- SXTpcaFindOutlier(SXTpcaData = SXTpcaData,
                                CI = CI,
                                plot.name = paste("Batch",i,"outliers"),
                                output.plot = TRUE,
                                path = path)
    obs.remove[[i]] <- index2
  }
SubjectOutlierFinderData <- list(MetFlowData = MetFlowData,
                            obs.remove = obs.remove)
class(SubjectOutlierFinderData) <- "SubjectOutlierFinderData"
options(warn = 0)
return(SubjectOutlierFinderData)
}
