# 使用内置模块


if (sys.nframe() == 0) {
  ways <- list(
    A = c(3.7, 3.7, 3.0, 3.9, 2.7),
    B = c(7.3, 5.2, 5.3, 5.7, 6.5),
    C = c(9.0, 4.9, 7.1, 8.7, 8.0)
  )
  x <- unlist(ways)
  groups <- rep(names(ways), lengths(ways))


  A = c(3.7, 3.7, 3.0, 3.9, 2.7)
  B = c(7.3, 5.2, 5.3, 5.7, 6.5)
  C = c(9.0, 4.9, 7.1, 8.7, 8.0)
  mat = cbind(A, B, C)
  N = nrow(mat) * ncol(mat)
  rankMat = matrix(rank(mat), ncol = ncol(mat))
  kruskal_q = (12 / (N*(N+1))) * sum(apply(rankMat, 2, sum)^2) / 5 - 3*(N+1)
  result <- kruskal.test(x ~ groups)
  result
  # DescTools::DunnTest(x, groups, method = "holm")
}





