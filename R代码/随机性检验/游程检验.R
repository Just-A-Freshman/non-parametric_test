binMixData <- function(x, y){
  combined <- c(x, y)
  group <- c(rep(0, length(x)), rep(1, length(y)))
  sorted_group <- group[order(combined)]
  return(sorted_group)
}


runTest <- function(
    x,
    alternative = c("less", "greater", "two.sided"),
    alpha = 0.05,
    exact = FALSE
    ) {
    alternative <- match.arg(alternative)

    # 计算基本参数
    n0 <- sum(x == 0)
    n1 <- sum(x == 1)
    n <- n0 + n1

    # 计算均值和方差
    mu_r <- 1 + 2 * n0 * n1 / n
    var_r <- (2 * n0 * n1 * (2 * n0 * n1 - n)) / (n^2 * (n - 1))

    # 计算游程数
    runs <- length(rle(x)$lengths)

    if (exact) {
      max_runs <-  2 * min(n0, n1) + (n0 != n1)
        prob <- rep(0, max_runs)
        for (r in 2:max_runs) {
          k <- r %/% 2
          if (r %% 2 == 0) {
            prob[r] <- 2 * choose(n0 - 1, k - 1) * choose(n1 - 1, k - 1) / choose(n0 + n1, n0)
          } else {
            term1 <- choose(n0 - 1, k - 1) * choose(n1 - 1, k)
            term2 <- choose(n0 - 1, k) * choose(n1 - 1, k - 1)
            prob[r] <- (term1 + term2) / choose(n0 + n1, n0)
          }
        }
        lower_tail <-  sum(prob[2:runs])
        upper_tail <-  sum(prob[runs:max_runs])
        if (alternative == "less") {p_value <- lower_tail}
        else if (alternative == "greater") {p_value <- upper_tail}
        else {p_value <- 2 * min(lower_tail, upper_tail)}
      }
    else{
      z <- (runs - mu_r) / sqrt(var_r)
      if (alternative == "less") {p_value <- pnorm(z)}
      else if (alternative == "greater") {p_value <- 1 - pnorm(z)}
      else {p_value <- 2 * min(pnorm(z), 1 - pnorm(z))}
    }

    if (p_value < alpha) {judgement = "拒绝原假设"}
    else {judgement = "不能拒绝原假设"}

    # 返回结果
    list(
      `p-value` = p_value,
      `游程数` = runs,
      `样本数` = sprintf("n0 = %d, n1 = %d, n = %d", n0, n1, n),
      `均值` = mu_r,
      `方差` = var_r,
      `结论` = judgement
    )
}




X = c(18, 15, 9, 10, 14, 16, 11, 13, 19, 20, 6)
Y = c(12, 13, 9, 8, 1, 2, 7, 5, 3, 2, 4)
test_data = binMixData(X, Y)
print(test_data)

# test_data应该形如c(1, 0, 1, 1, 1, 0)这种二元序列
runTest(test_data, "two.sided", exact = T)

