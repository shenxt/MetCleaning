#' @title SXTscale
#' @description Scale method.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param x data for scale.
#' @param method scale method.
#' @return Return x after scaling.
#' @export
#' @examples
#' x <- matrix(rnorm(1000),nrow = 10,ncol = 100)
#' x1 <- SXTscale(x)

SXTscale<-function(x,method=c("pareto","auto")) {
  if (method=="pareto") {x<-apply(x,2, function(x) {(x-mean(x))/sqrt(sd(x))})}
  if (method=="auto") {x<-apply(x,2, function(x) {(x-mean(x))/sd(x)})}
  return(x)
}