#' @title SXTvip
#' @description Get VIP from pls object.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param object PLS object.
#' @return Return VIP.
#' @export
#' @examples
#' library(pls)
#' x <- matrix(rnorm(1000),nrow = 10,ncol = 100)
#' y <- rep(0:1,5)
#' res <- plsr(y~x, method = "oscorespls")
#' SXTvip(res)


SXTvip <- function(object) {
  if (object$method != "oscorespls")
    stop("Only implemented for orthogonal scores algorithm.  Refit with 'method = \"oscorespls\"'")
  if (nrow(object$Yloadings) > 1)
    stop("Only implemented for single-response models")
  SS <- c(object$Yloadings)^2 * colSums(object$scores^2)
  Wnorm2 <- colSums(object$loading.weights^2)
  SSW <- sweep(object$loading.weights^2, 2, SS / Wnorm2, "*")
  sqrt(nrow(SSW) * apply(SSW, 1, cumsum) / cumsum(SS))
}