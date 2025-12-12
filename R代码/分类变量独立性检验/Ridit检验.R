ridit.test = function(
    data,
    std_group_ratio = 3,
    conf = 0.95
  ) {
  # 使用双侧检验，比较不同组请看图
  if (is.null(nrow(data))) {
    stop("检验数据中至少包含两个对象！")
  }

  sample_size <- sum(data)
  group_sample_sum <- rowSums(data)
  std_group_idx <- -1
  df <- nrow(data) - 1
  group_names <- if (!is.null(rownames(data))) {
    rownames(data)
  } else {
    paste("G", 1:nrow(data))
  }

  # 1. 确定标准组
  sort_sample_sum <- sort(group_sample_sum, decreasing = TRUE)
  if (sort_sample_sum[1] >= 3 * sort_sample_sum[2]) {
    std_group_idx <- which.max(group_sample_sum)
    group_sample_sum <- group_sample_sum[-std_group_idx]
    std_group <- data[std_group_idx, ]
    control_group <- data[-std_group_idx, ]
    std_group_name <- group_names[std_group_idx]
    control_names <- group_names[-std_group_idx]
  } else {
    std_group <- colSums(data)
    control_group <- data
    std_group_name <- "merged"
    control_names <- group_names
  }

  # 2. 计算每个评分映射到的Ridit值
  N_std <- sum(std_group)
  cum_freq <- c(0, cumsum(std_group)[-length(std_group)])
  std_ridit <- (cum_freq + std_group / 2) / N_std

  # 3. 计算对照组的平均Ridit值
  control_avg_ridit <- (control_group / group_sample_sum) %*% std_ridit

  # 4. 计算标准组的方差
  std_variance <- ((1 / N_std) * sum(std_group * std_ridit^2) - 1/4) *
                 (N_std / (N_std - 1))

  control_SE <- sqrt(std_variance / group_sample_sum)

  # 5. 根据对照组数量的不同安排不同检验
  if (is.null(nrow(control_group)) || nrow(control_group) == 2) {
    if (is.null(nrow(control_group))) {
      Z <- (control_avg_ridit - 0.5) / control_SE
    }
    else {
      avg_R_diff <- abs(control_avg_ridit[1] - control_avg_ridit[2])
      combine_SE <- sqrt(sum(1 / group_sample_sum) * std_variance)
      Z <- avg_R_diff / combine_SE
    }
    statistic <- c(Z = Z)
    p.value <- pnorm(Z, lower.tail = FALSE) * 2
  } else {
    Z_list <- (control_avg_ridit - 0.5) / control_SE
    W <- sum(Z_list^2)
    statistic <- c(W = W)
    p.value <- pchisq(W, df, lower.tail = FALSE)
  }

  # 6. 画图
  Z_line <- qnorm(1 - (1 - conf) / 2)
  control_ci_lower <- control_avg_ridit - Z_line * control_SE
  control_ci_upper <- control_avg_ridit + Z_line * control_SE
  control_groups_num <- max(nrow(control_group), 1)

  margin <- 0.2
  if (control_groups_num == 1) {
    x_positions <- 1.5
    xlim_left <- 0.5
    xlim_right <- 2.5
  } else {
    # 计算位置，让线条均匀分布但不在最边缘
    total_width <- control_groups_num - 1
    start <- 1 + margin
    end <- control_groups_num - margin
    x_positions <- seq(start, end, length.out = control_groups_num)
    xlim_left <- 0.5
    xlim_right <- control_groups_num + 0.5
  }

  plot(
    0, 0,
    ylim = c(0, 1),
    xlim = c(xlim_left, xlim_right),
    xlab = "group",
    ylab = "R score",
    main = "Ridit value confidence interval",
    col = "gray7",
    xaxt = "n",
    yaxt = "n"
  )
  abline(h = 0.5, col = "black", lty = 2, lwd = 1.5)
  for (i in 1:control_groups_num) {
    lines(
      c(i, i),
      c(control_ci_lower[i], control_ci_upper[i]),
      col = "black",
      lwd = 2
    )
  }

  # 添加X轴标签
  axis(1, at = 1:control_groups_num, labels = control_names)
  axis(2, at = 0.5, labels = std_group_name)

  result <- list(
    statistic = statistic,
    df = df,
    p.value = p.value,
    parameter = c(df = df),
    data.name = deparse(substitute(data)),
    std_variance = std_variance,
    std_redit = std_ridit,
    control_avg_ridit = control_avg_ridit,
    method = "ridit Test",
    control_group = control_group,
    std_group = std_group
  )

  class(result) <- "htest"
  return(result)
}



if (sys.nframe() == 0) {
  # 测试数据集
  ## 1. 单样本检验
  test1 = matrix(
    c(
      99, 193, 65, 28, 12,
      14, 45, 17, 20, 7
  ), nrow = 2, byrow = TRUE)

  ## 2. 双样本检验
  test2 = matrix(
    c(
      6, 18, 19, 27, 25,
      15, 31, 31, 32, 19
  ), nrow = 2, byrow = TRUE)

  ## 3. 多样本检验
  test3 = matrix(
    c(
      15, 45, 153, 231, 56,
      39, 89, 198, 126, 48,
      89, 177, 134, 88, 12,
      13, 23, 86, 257, 121
  ), nrow = 4, byrow = TRUE)

  test4 = matrix(
    c(
      90, 23, 53, 21, 13,
      47, 34, 28, 18, 5,
      20, 13, 10, 5, 2,
      28, 32, 33, 45, 16,
      34, 28, 52, 40, 10
  ), nrow = 5, byrow = TRUE)

  result <- ridit.test(test3)
  result
}
