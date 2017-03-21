#' @title SXTpaste
#' @description Paste for sequence.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param x vector.
#' @param sep seperate.
#' @return x after paste
#' @export
#' @examples
#' x <- c("a", "b", "c")
#' SXTpaste(x, sep = "")

SXTpaste<-function(x,sep=" ") {
  y<-NULL
  for (i in 1:length(x)) {
    if (i==1) {y<-paste(y,x[i],sep="")}
    else {y<-paste(y,x[i],sep=sep)}
  }
  y
}