#' @title MarkerShow
#' @description Draw boxplot for markers.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param MetFlowData MetFlowData.
#' @param path The directory you want to write results.
#' @param beeswarm Do you want draw beeswarm on the boxplot? Deafult is TRUE.
#' @return Box plot of markers.
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
#'# FoldChange
#'met.data <- FoldChange(MetFlowData = met.data.after.pre,
#'                       to = c("1", "0"))
#'##PLSanalysis
#'PLSanalysis(met.data,
#'            plsmethod = "plsr",
#'            path = "Demo for PLSanalysis")
#'
#'load("Demo for PLSanalysis/vip")
#'vip <- apply(vip, 2, mean)
#'##VIP
#'tags <- met.data[["tags"]]
#'tags <- data.frame(tags, vip)
#'met.data[["tags"]] <- tags
#'##UnivariateTest
#'met.data <- UnivariateTest(met.data)
#'## run
#'new.met.data <- MarkerSelection(met.data)
#'run
#'MarkerShow(new.met.data, path = "Demo for MarkerShow")
#'}

MarkerShow <- function(MetFlowData,
                       beeswarm = TRUE,
                       path = ".") {
  # browser()
  options(warn = -1)

  if (path != ".") {
    dir.create(path)
  }

  # library(beeswarm)
  tags <- MetFlowData@tags
  subject <- MetFlowData@subject
  subject.info <- MetFlowData@subject.info
  if (all(colnames(tags) != "is.marker")) {
    stop("Please select marker first(use MarkerSelection function).")
  }

  is.marker <- tags[, "is.marker"]
  marker.index <- which(is.marker == "yes")
  feature.name <- tags[, "name"]
  group <- subject.info[, "group"]
  subject.name <- subject.info[, 1]
  group.unique <- sort(unique(group))

  info <- list()
  for (i in 1:seq_along(group.unique)) {
    info[[i]] <- subject.name[which(group == group.unique[i])]
  }
  names(info) <- group.unique

  for (i in marker.index) {
    temp.data <- list()
    for (j in 1:seq_along(info)) {
      temp.data[[j]] <-
        as.numeric(subject[i,])[match(info[[j]], subject.name)]
    }
    names(temp.data) <- names(info)
    pdf(file.path(path, paste(feature.name[i], "boxplot.pdf")))
    par(mar = c(5, 5, 4, 2))
    boxplot(
      temp.data,
      xlab = "Group",
      ylab = "Intensity",
      cex.lab = 1.5,
      cex.axis = 1.3
    )
    if (beeswarm) {
      beeswarm::beeswarm(temp.data,
               pch = 19,
               add = TRUE,
               col = "grey")
    }
    dev.off()
  }
  options(warn = 0)
}
