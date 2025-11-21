library(PMCMRplus)



Durbin.test = function(data, use_F = FALSE) {
  # X轴为处理，Y轴为区组
  k <- ncol(data)
  b <- nrow(data)

  bool_mat <- is.na(data)
  check_t <- apply(!bool_mat, 1, sum)
  check_r <- apply(!bool_mat, 2, sum)

  t <- as.integer(check_t[1])
  r <- as.integer(check_r[1])
  if (any(check_t != t)) {stop("处理数不均衡！")}
  if (any(check_r != r)) {stop("区组数不均衡！")}

  rank_mat <-  t(apply(data, 1, rank))
  rank_mat[bool_mat] <- 0

  A <- sum(rank_mat^2)
  C <- b * t * (t + 1)^2 / 4

  tmp <- apply(rank_mat, 2, sum) - r * (t + 1) / 2
  D <- (k - 1) / (A - C) * sum(tmp^2)

  if (use_F) {
    d1 <- k - 1
    d2 <- b * t - b - k + 1
    df <- c(d1, d2)
    D_F <- (D / d1) / ((b * (t - 1) - D) / d2)
    p.value <- pf(D_F, d1, d2, lower.tail = FALSE)
    statistic <- D_F
    stat_name <- "F"
  }
  else {
    df <- k - 1
    p.value <- pchisq(D, df, lower.tail = FALSE)
    statistic <- D
    stat_name <- "Durbin chi-squared"
  }

  result <- list(
    statistic = setNames(statistic, stat_name),
    parameter = c(df = df),
    p.value = p.value,
    method = paste0(c(
        "Durbin's rank sum test for a two-way",
        "balanced incomplete block design"), sep = "\n"
    ),
    data.name = deparse(substitute(data)),
    dimensions = c(k = k, b = b, r = r, t = t)
  )

  class(result) <- "htest"
  return(result)
}


if (sys.nframe() == 0) {
  data_matrix <- matrix(c(
    2, 3, NA, 1, NA, NA, NA,
    NA, 3, 1, NA, 2, NA, NA,
    NA, NA, 2, 1, NA, 3, NA,
    NA, NA, NA, 1, 2, NA, 3,
    3, NA, NA, NA, 1, 2, NA,
    NA, 3, NA, NA, NA, 1, 2,
    3, NA, 1, NA, NA, NA, 2
  ), nrow = 7, byrow = TRUE)

  rownames(data_matrix) <- paste0("测试者", 1:7)
  colnames(data_matrix) <- paste0("冰淇淋", 1:7)

  Durbin.test(data_matrix)

  # 第三方库之力
  durbinTest(data_matrix)
}






