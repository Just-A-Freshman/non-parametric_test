library(DescTools)

Cochran.Qtest = function(data) {
  N = sum(data)
  k = ncol(data)
  sumSquareOj = sum(apply(data, 2, sum)^2)
  sumSquareOi = sum(apply(data, 1, sum)^2)
  Q = (k - 1) * (sumSquareOj - N^2 / k) /
      (N - sumSquareOi / k)
  p.value <- pchisq(Q, k - 1, lower.tail = FALSE)

  result <- list(
    statistic = c(Q = Q),
    parameter = c(df = k - 1),
    p.value = p.value,
    method = "Cochran's Q test",
    data.name = deparse(substitute(data))
  )
  class(result) <- "htest"
  return(result)
}


if (sys.nframe() == 0) {
  A <- c(1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0)
  B <- c(0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0)
  C <- c(1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1)
  mat = cbind(A, B, C)
  # CochranQTest(mat)
  Cochran.Qtest(mat)
}
