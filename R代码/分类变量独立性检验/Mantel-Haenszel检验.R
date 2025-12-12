# 创建一个3层的2x2表数据
data <- array(c(
  43, 87, 65, 77,
  9, 15, 73, 30,
  7, 9, 18, 11
), dim = c(2, 2, 3))

data <- array(c(
   43, 65, 87, 77,
   9, 73, 15, 30,
   7, 18, 9, 11
), dim = c(2, 2, 3))

dimnames(data) <- list(
  暴露 = c("是", "否"),
  疾病 = c("病例", "对照"),
  层 = c("层1", "层2", "层3")
)


mantelhaen.test(data)



mantelhaenTest = function(data) {

}


easyCalcu = function(data) {
  row_sum = apply(data, 1, sum)
  col_sum = apply(data, 2, sum)
  all_sum = sum(data)
  E = row_sum[1] * col_sum[1] / all_sum
  D = prod(as.vector(cbind(row_sum, col_sum))) / (all_sum^2 * (all_sum - 1))
  return(list(
    "期望" = E,
    "方差" = D
  ))
}


if (sys.nframe() == 0) {
  data = matrix(
    c(7, 9, 18, 11), nrow = 2
  )
  easyCalcu(data)
}
