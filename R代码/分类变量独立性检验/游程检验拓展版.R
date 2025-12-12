run.test <- function(
    x,
    alternative = c("less", "greater", "two.sided"),
    exact = FALSE
) {
  alternative <- match.arg(alternative)

  x <- as.factor(x)
  categories <- levels(x)
  k <- length(categories)

  n_i <- table(x)
  n <- length(x)

  if (k < 2) {
    stop("At least two categories are required for runs test")
  }

  runs <- length(rle(as.numeric(x))$lengths)
  p_i <- n_i / n
  E_R <- n * (1 - sum(p_i^2)) + 1

  if (k == 2) {
    n0 <- n_i[1]
    n1 <- n_i[2]
    var_R <- (2 * n0 * n1 * (2 * n0 * n1 - n)) / (n^2 * (n - 1))
  } else {
    var_R <- n * (sum(p_i^2 - 2 * p_i^3) + (sum(p_i^2))^2)
  }

  # 处理精确检验
  if (exact) {
    if (k > 2) {
      warning("Exact test not available for more than 2 categories. Using normal approximation.")
      exact <- FALSE
    } else {
      n0 <- n_i[1]
      n1 <- n_i[2]
      max_runs <- 2 * min(n0, n1) + (n0 != n1)
      cases <- numeric(max_runs)

      for (r in 2:max_runs) {
        k_val <- r %/% 2


        if (r %% 2 == 0) {
          cases[r] <- 2 * choose(n0 - 1, k_val - 1) * choose(n1 - 1, k_val - 1)
        } else {
          term1 <- choose(n0 - 1, k_val - 1) * choose(n1 - 1, k_val)
          term2 <- choose(n0 - 1, k_val) * choose(n1 - 1, k_val - 1)
          cases[r] <- term1 + term2
        }
      }

      sample_spaces <- choose(n0 + n1, n0)
      lower_tail <- sum(cases[2:runs]) / sample_spaces
      upper_tail <- sum(cases[runs:max_runs]) / sample_spaces
    }
  }

  # 如果非精确检验或多元情况，使用正态近似
  if (!exact) {
    z <- (runs - E_R) / sqrt(var_R)
    lower_tail <- pnorm(z)
    upper_tail <- pnorm(z, lower.tail = FALSE)
  }

  # 计算p值
  p.value <- switch(
    alternative,
    "less" = lower_tail,
    "greater" = upper_tail,
    "two.sided" = 2 * min(lower_tail, upper_tail)
  )

  result <- list(
    statistic = c(R = runs),
    parameter = c(mu = E_R, var = var_R, categories = k),
    p.value = p.value,
    method = if (exact) {
      "Exact Runs Test (Binary)"
    } else if (k == 2) {
      "Runs Test (Normal Approximation)"
    } else {
      "Multi-Category Runs Test (Barton-David)"
    },
    alternative = alternative,
    null.value = c("number of runs" = E_R),
    sample.size = c(total = n, table = as.list(n_i)),
    categories = categories
  )

  class(result) <- "htest"
  return(result)
}


if (sys.nframe() == 0) {
  x = c(1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1)
  result <- run.test(x)
  result
}
