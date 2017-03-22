#' @title UnivariateTest
#' @description Calculate p value and AUC for each feature.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param MetFlowData MetFlowData.
#' @param test.method Which test you want to use? "t" means stutent t test and "wilcox" mean wilcoxon test.
#' @param adjust.method p value correction method. See p.adjust function.
#' @param log.scale Data transformation method, defaulst is FALSE.
#' @param class Class used to do test.
#' @return MetFlowData which has been added p and AUC information in tags.
#' @seealso The details of univariate test can be found in \code{\link[stats]{t.test}},
#' \code{\link[stats]{p.adjust}} and \code{\link[stats]{wilcox.test}}.
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
#'new.met.data <- UnivariateTest(met.data.after.pre)
#' }

UnivariateTest <- function(MetFlowData,
                           test.method = "t",
                           adjust.method = "fdr",
                           log.scale = FALSE,
                           class = c("control", "case")) {
  # browser()
  subject <- MetFlowData@subject
  subject.info <- MetFlowData@subject.info
  group <- subject.info[, "group"]
  tags <- MetFlowData@tags

  group.unique <- sort(unique(group))

  if (length(class) != 2 &
      test.method == "t" | test.method == "wilcox") {
    stop("Not two class data.")
  }

  if (length(class) <= 2 & test.method == "anova") {
    stop("Not multiple (>2) class data.")
  }

  if (test.method == "t" | test.method == "wilcox") {
    group1.index <- which(group == group.unique[1])
    group2.index <- which(group == group.unique[2])
    Y <- NULL
    Y[group1.index] <- 0
    Y[group2.index] <- 1

    ##log transformation
    if (log.scale == FALSE) {
      subject <- subject
    }

    if (log.scale == "e") {
      subject <- log(subject + 1)
    }

    if (log.scale != FALSE & log.scale != "e") {
      subject <- log(subject + 1, as.numeric(log.scale))
    }

    if (test.method == "t") {
      subject.test <-
        apply(subject, 1, function(x) {
          t.test(x[group1.index], x[group2.index])
        })
    }
    if (test.method == "wilcox") {
      subject.test <-
        apply(subject, 1, function(x) {
          wilcox.test(x[group1.index], x[group2.index])
        })
    }


    p <- unlist(lapply(subject.test, function(x)
      x$p.value))
    p.correct <- p.adjust(p = p, method = adjust.method)
  }

  if (test.method == "anova") {
    ##info
    info <- list()
    for (i in seq_along(class)) {
      info[[i]] <- subject.info[, 1][group == class[i]]
    }
    names(info) <- class

    subject.test <- list()
    aov.p <- NULL
    subject.tuk <- list()

    tuk.p <-
      matrix(nrow = nrow(subject), ncol = choose(length(info), 2))

    Y <- NULL
    name <- colnames(subject)

    for (i in seq_along(info)) {
      Y[match(info[[i]], name)] <- i - 1
    }

    for (i in 1:nrow(subject)) {
      subject.test[[i]] <- aov(as.numeric(subject[i, ]) ~ as.factor(Y))
      aov.p[i] <- summary(subject.test[[i]])[[1]][, 5][1]
      subject.tuk[[i]] <- TukeyHSD(subject.test[[i]])
      tuk.p[i, ] <- subject.tuk[[i]][[1]][, 4]
      colnames(tuk.p) <- names(subject.tuk[[i]][[1]][, 4])
    }
    p <- aov.p
    p.corr <- aov.p
    p.each <- tuk.p
  }

  feature.auc <- apply(subject, 1, function(x) {pROC::auc(Y, x)})

  if (any(colnames(tags) == "p")) {
    tags[, "p"] <- p
  }
  else {
    tags <- cbind(tags, p)
  }

  if (any(colnames(tags) == "p.correct")) {
    tags[, "p.correct"] <- p.correct
  }
  else {
    tags <- cbind(tags, p.correct)
  }
  if (test.method == "anova") {
    tags <- cbind(tags, tuk.p)
  }

  if (any(colnames(tags) == "feature.auc")) {
    tags[, "feature.auc"] <- feature.auc
  }
  else {
    tags <- cbind(tags, feature.auc)
  }

  MetFlowData@tags <- as.matrix(tags)
  MetFlowData@univariate.test <- test.method
  return(MetFlowData)
}