HL.test = function(mat, center = c("mean", "median")){
  # X轴为处理，Y轴为区组
  center <- match.arg(center)
  centerFunc <- if (center == "mean") mean else median

  # 减掉区组效应
  outCenterMat <- mat - apply(mat, 1, centerFunc)
  rankMat <- matrix(rank(outCenterMat), ncol = ncol(mat))

  # 计算统计量
  b <- nrow(mat)
  k <- ncol(mat)
  N <- k * b
  riSumSq <- sum(apply(rankMat, 1, sum)^2)
  rjSumSq <- sum(apply(rankMat, 2, sum)^2)
  rijSumSq <- sum(rankMat^2)

  Q <- (k - 1) * (rjSumSq - (k * b^2 * (k * b + 1)^2) / 4) /
       (rijSumSq - riSumSq / k)

  p.value <- pchisq(Q, k - 1, lower.tail = FALSE)

  result <- list(
    statistic = c(Q = Q),
    parameter = c(df = k - 1),
    p.value = p.value,
    method = "Hodges-Lehmann Rank Sum Test",
    data.name = deparse(substitute(mat)),
    ranks = rankMat,
    squared.sums = c(Ri2 = riSumSq, Rj2 = rjSumSq, Rij2 = rijSumSq),
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
  result = HL.test(mat, "mean")
  result
}




