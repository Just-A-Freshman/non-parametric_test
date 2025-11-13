moses.test <- function(
    X,
    Y,
    K = 3,
    alternative = c("two.sided", "less", "greater"),
    replicates = 500
) {
  alternative <- match.arg(alternative)

  m1 <- floor(length(X) / K)
  m2 <- floor(length(Y) / K)

  if (m1 == 0 || m2 == 0) {
    stop("Sample size too small for grouping with K = ", K)
  }

  p_value_sum = 0
  for (i in 1:replicates) {
    shuffled_X <- sample(X)[1:(m1 * K)]
    shuffled_Y <- sample(Y)[1:(m2 * K)]

    X_groups <- split(shuffled_X, rep(1:m1, each = K))
    Y_groups <- split(shuffled_Y, rep(1:m2, each = K))

    calc_ss <- function(x) sum((x - mean(x))^2)
    X_ss <- vapply(X_groups, calc_ss, numeric(1))
    Y_ss <- vapply(Y_groups, calc_ss, numeric(1))

    test_result <- wilcox.test(
      X_ss, Y_ss, alternative = alternative, exact = FALSE
    )
    p_value_sum = p_value_sum + test_result$p.value
  }


  # 构建过程数据表
  data_cols <- paste0("G", 1:K)

  create_group_table <- function(groups, ss, prefix) {
    group_data <- do.call(rbind, lapply(groups, function(group) {
      as.data.frame(t(round(group, 2)))
    }))
    colnames(group_data) <- data_cols

    data.frame(
      Group = paste0(prefix, "-", names(groups)),
      group_data,
      SS = round(ss, 2),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }

  X_table <- create_group_table(X_groups, X_ss, "X")
  Y_table <- create_group_table(Y_groups, Y_ss, "Y")
  summary_table <- rbind(X_table, Y_table)

  result <- list(
    statistic = test_result$statistic,
    parameter = c(stable.p.value = p_value_sum / replicates),
    p.value = test_result$p.value,
    null.value = c("difference in scale" = 0),
    alternative = alternative,
    method = "Moses Test of Homoscedasticity",
    data.name = paste(deparse(substitute(X)), "and", deparse(substitute(Y))),
    sample.size = c(
      K = K, X_groups = m1, Y_groups = m2,
      X_obs = length(X), Y_obs = length(Y)
    ),
    process.data = summary_table
  )

  class(result) <- "htest"
  return(result)
}

# 测试代码
if (sys.nframe() == 0) {
  X <- c(8.8, 8.2, 5.6, 4.9, 8.9, 4.2, 3.6, 7.1, 5.5, 8.6, 6.3, 3.9)
  Y <- c(13.0, 14.5, 16.5, 22.8, 20.7, 19.6, 18.4, 21.3, 24.2, 19.6, 11.7, 18.9, 14.6, 19.8, 14.5)
  result <- moses.test(X, Y)
  result
}
