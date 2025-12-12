# 与它实现基本相同的还有卡方同质性检验
# 给定一个m * n 的列联表，利用独立时的公式计算期望频数；
# 随后套用卡方检验的公式即可。

chisqTest = function(data_matrix){
  row_num = nrow(data_matrix)
  col_num = ncol(data_matrix)
  df = (row_num - 1) * (col_num - 1)
  row_sum = matrix(rep(apply(data_matrix, 1, sum), col_num), row_num)
  col_sum = t(matrix(rep(apply(data_matrix, 2, sum), row_num), col_num))

  expected_matrix = row_sum * col_sum / sum(data_matrix)
  x_chisq = sum((data_matrix - expected_matrix)^2 / expected_matrix)
  p.value = pchisq(x_chisq, df, lower.tail = FALSE)

  result <- list(
    statistic = setNames(x_chisq, "X-squared"),
    parameter = c(df = df),
    p.value = p.value,
    method = "Pearson's Chi-squared test",
    data.name = deparse(substitute(data)),
    dimensions = c(row = row_num, col = col_num)
  )
  class(result) <- "htest"
  return(result)
}



if (sys.nframe() == 0) {
  data = matrix(c(
    15, 53, 25, 27,
    8, 48, 57, 47
  ), nrow = 4)
  chisqTest(data)
}


