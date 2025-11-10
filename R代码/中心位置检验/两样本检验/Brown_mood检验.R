# 备注: Brown_mood检验本质上就是符号检验的双样本扩展

X = c(18, 15, 9, 10, 14, 16, 11, 13, 19, 20, 6)
Y = c(12, 13, 9, 8, 1, 2, 7, 5, 3, 2, 4)

# 方法一：直接调用函数
response <- c(X, Y)
fact <- c(rep('A',length(X)),rep('B',length(Y))) |> as.factor()
RVAideMemoire::mood.medtest(response~fact)


# 方法二：个人实现
BM.test = function(
    x,
    y,
    alternative = c("two.sided", "less", "greater")
){
  alternative <- match.arg(alternative)
  combine = c(x, y)

  # 计算原始混合中位数
  Mxy_original = median(combine)

  # 处理等于中位数的情况
  equal_to_median = (combine == Mxy_original)
  n_equal = sum(equal_to_median)

  if (n_equal > 0) {
    first_equal_idx = which(equal_to_median)[1]
    if (first_equal_idx <= length(x)) {
      x_clean = x[-first_equal_idx]
      y_clean = y
    } else {
      x_clean = x
      y_clean = y[-(first_equal_idx - length(x))]
    }
    combine_clean = c(x_clean, y_clean)
    Mxy = median(combine_clean)
  } else {
    x_clean = x
    y_clean = y
    combine_clean = combine
    Mxy = Mxy_original
  }

  # 统计量 A ~ h(n, N, M)
  A = sum(x_clean > Mxy)
  n = sum(combine_clean > Mxy)
  N = length(combine_clean)
  M = length(x_clean)

  # 这里lower_tail_p != 1 - upper_tail_p
  # 因为这是离散分布，取等概率不为0, 要考虑进去
  lower_tail_p = phyper(A, M, N - M, n)
  upper_tail_p = phyper(A - 1, M, N - M, n, lower.tail = FALSE)
  if (alternative == "less") {p_value = lower_tail_p}
  if (alternative == "greater") {p_value = upper_tail_p}
  if (alternative == "two.sided") {
    p_value = 2 * min(lower_tail_p, upper_tail_p)
  }
  result = list(
    `A统计量` = A,
    `p值` = p_value,
    `有效样本数` = N,
    `X样本数` = M,
    `Y样本数` = N - M,
    `混合中位数` = Mxy
  )
  return(result)
}


BM.test(X, Y, alternative = "greater")

