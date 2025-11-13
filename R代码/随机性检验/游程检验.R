run.test <- function(
      x,
      alternative = c("less", "greater", "two.sided"),
      exact = FALSE
    ) {
    alternative <- match.arg(alternative)

    # 计算基本参数
    n0 <- sum(x == 0)
    n1 <- sum(x == 1)
    n <- n0 + n1

    if (n0 == 0 || n1 == 0) {
      stop("0和1都必须在序列中出现至少1次！")
    }

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
          sample_space <-  choose(n0 + n1, n0)
          if (r %% 2 == 0) {
            term = 2 * choose(n0 - 1, k - 1) * choose(n1 - 1, k - 1)
            prob[r] <- term / sample_space
          } else {
            term1 <- choose(n0 - 1, k - 1) * choose(n1 - 1, k)
            term2 <- choose(n0 - 1, k) * choose(n1 - 1, k - 1)
            prob[r] <- (term1 + term2) / sample_space
          }
        }
        lower_tail <-  sum(prob[2:runs])
        upper_tail <-  sum(prob[runs:max_runs])
      }
    else{
      z <- (runs - mu_r) / sqrt(var_r)
      lower_tail = pnorm(z)
      upper_tail = pnorm(z, lower.tail = FALSE)
      print(lower_tail)
    }

    p.value <- switch(
      alternative,
      "less" = lower_tail,
      "greater" = upper_tail,
      "two.sided" = 2 * min(lower_tail, upper_tail)
    )

    result <- list(
        statistic = c(runs = runs),
        parameter = c(mu = mu_r, var = var_r),
        p.value = p.value,
        method = ifelse(exact, "Exact Runs Test", "Runs Test (Normal Approximation)"),
        alternative = alternative,
        data.name = deparse(substitute(x)),
        null.value = c("number of runs" = mu_r),
        sample.size = c(n0 = n0, n1 = n1, n = n)
    )

    class(result) <- "htest"
    return(result)
}


binMixData <- function(x, y){
  combined <- c(x, y)
  group <- c(rep(0, length(x)), rep(1, length(y)))
  sorted_group <- group[order(combined)]
  return(sorted_group)
}


if (sys.nframe() == 0) {
  x = c(1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1)
  result <- run.test(x)
}


