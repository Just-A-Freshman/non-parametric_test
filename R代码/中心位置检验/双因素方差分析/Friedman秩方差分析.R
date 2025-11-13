Friedman.test <- function(mat) {
  # X轴为处理，Y轴为区块
  orig_result <- friedman.test(mat)
  b <- nrow(mat)
  k <- ncol(mat)
  N <- b * k

  result <- list(
    statistic = c(orig_result$statistic),
    parameter = c(df = k - 1),
    p.value = orig_result$p.value,
    method = orig_result$method,
    data.name = orig_result$data.name,
    ranks = t(apply(mat, 1, rank)),
    dimensions = c(treatments = k, blocks = b, observations = N)
  )

  class(result) <- "htest"
  return(result)
}


if (sys.nframe() == 0) {
  A <- c(20.3, 21.2, 18.2, 18.6, 18.5)
  B <- c(25.6, 24.7, 19.3, 19.3, 20.7)
  C <- c(24.0, 23.1, 20.6, 19.8, 21.4)
  mat <- cbind(A, B, C)
  rownames(mat) <- c('I', 'II', 'III', 'IV', 'V')

  result = Friedman.test(mat)
  result
}

