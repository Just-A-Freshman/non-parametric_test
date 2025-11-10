moses.test <- function(
    X,
    Y,
    K=3,
    alternative = c("two.sided", "less", "greater"),
    conf.level = 0.95
  ){
  m1 <-  floor(length(X) / K)
  m2 <-  floor(length(Y) / K)

  if (m1 == 0 || m2 == 0) {
    stop("样本量太少，无法进行分组!")
  }

  shuffled_X <- sample(X)[1:(m1 * K)]
  shuffled_Y <- sample(Y)[1:(m2 * K)]

  X_groups <- split(shuffled_X, rep(1:m1, each = K))
  Y_groups <- split(shuffled_Y, rep(1:m2, each = K))

  calc_ss = function(x){sum((x - mean(x))^2)}
  X_ss = vapply(X_groups, calc_ss, numeric(1))
  Y_ss = vapply(Y_groups, calc_ss, numeric(1))

  # 下面为信息打印
  data_cols <- paste0("G", 1:K)
  X_data <- do.call(rbind, lapply(X_groups, function(group) {
    as.data.frame(t(round(group, 2)))
  }))
  colnames(X_data) <- data_cols

  Y_data <- do.call(rbind, lapply(Y_groups, function(group) {
    as.data.frame(t(round(group, 2)))
  }))
  colnames(Y_data) <- data_cols

  X_table <- data.frame(
    `Groups` = paste0("X-", names(X_groups)),
    X_data,
    `SS` = round(X_ss, 2),
    stringsAsFactors = FALSE
  )

  Y_table <- data.frame(
    `Groups` = paste0("Y-", names(Y_groups)),
    Y_data,
    `SS` = round(Y_ss, 2),
    stringsAsFactors = FALSE
  )

  summary_table <- rbind(X_table, Y_table)
  wilcox_result = wilcox.test(X_ss, Y_ss, alternative, conf.level)

  return(list(
    `test_result` = wilcox_result,
    `summary` = summary_table,
    `groups_info` = sprintf("K = %d, X_groups = %d, Y_groups = %d", K, m1, m2)
  ))
}




X = c(8.8, 8.2, 5.6, 4.9, 8.9, 4.2, 3.6, 7.1, 5.5, 8.6, 6.3, 3.9)
Y = c(13.0, 14.5, 16.5, 22.8, 20.7, 19.6, 18.4, 21.3, 24.2, 19.6, 11.7, 18.9, 14.6, 19.8, 14.5)
moses.test(X, Y)

