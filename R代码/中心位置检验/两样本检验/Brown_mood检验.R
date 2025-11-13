BM.test = function(
    x,
    y,
    alternative = c("two.sided", "less", "greater")
){
  alternative <- match.arg(alternative)
  combine = c(x, y)

  # 计算原始混合中位数
  Mxy = median(combine)

  # 处理等于中位数的情况
  equal_to_median = (combine == Mxy)
  if (sum(equal_to_median) > 0) {
    first_equal_idx = which(equal_to_median)[1]
    if (first_equal_idx <= length(x)) {x = x[-first_equal_idx]}
    else {y = y[-(first_equal_idx - length(x))]}
    combine = c(x, y)
    Mxy = median(combine)
  }

  # 统计量 A ~ h(n, N, M)
  A = sum(x > Mxy)
  n = sum(combine > Mxy)
  N = length(combine)
  M = length(x)

  # 这里lower_tail_p != 1 - upper_tail_p
  # 因为这是离散分布，取等概率不为0, 要考虑进去
  lower_tail = phyper(A, M, N - M, n)
  upper_tail = phyper(A - 1, M, N - M, n, lower.tail = FALSE)
  p.value <- switch(
    alternative,
    "less" = lower_tail,
    "greater" = upper_tail,
    "two.sided" = 2 * min(lower_tail, upper_tail)
  )

  result <- list(
      statistic = c(A = A),
      parameter = c(median = Mxy),
      p.value = p.value,
      method = "Mood's median test",
      data.name = "response by fact",
      alternative = alternative,
      sample.size = c(Nx = M, Ny = N - M, N = N)
  )

  class(result) <- "htest"
  return(result)
}




if (sys.nframe() == 0) {
  # 备注: Brown_mood检验本质上就是符号检验的双样本扩展
  X = c(18, 15, 9, 10, 14, 16, 11, 13, 19, 20, 6)
  Y = c(12, 13, 9, 8, 1, 2, 7, 5, 3, 2, 4)
  response <- c(X, Y)
  fact <- c(rep('A',length(X)),rep('B',length(Y))) |> as.factor()

  # 方法1：直接调库
  RVAideMemoire::mood.medtest(response~fact)

  # 方法2：个人实现
  BM.test(X, Y, alternative = "two.sided")
}
