if (sys.nframe() == 0) {
  # 一、卡方拟合优度检验
  # 经典例子：掷骰子
  observed <- c(10, 9, 11, 8, 12, 10)
  chisq.test(observed, p = rep(1/6, 6))

  # 二、卡方独立性检验
  ## 每组观测频数至少大于5
  data <- matrix(c(8, 2, 7, 23), nrow = 2)
  chisq.test(data)

  ## 如果观测频数太小，使用fisher检验
  fisher.test(data)
}
