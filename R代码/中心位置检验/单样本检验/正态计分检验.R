NScoreTest <- function(wd, med.val=0, alternative = 'two.sided', conf.level = 0.95){
  n <- length(wd)
  # 计算正态计分
  wd.diff <- wd - med.val
  wd.absdiff <- abs(wd.diff)
  wd.rank <- rank(wd.absdiff)
  wd.quantile <- (1 + wd.rank / (n + 1)) / 2
  wd.nscore <- qnorm(wd.quantile, 0, 1) * sign(wd.diff)
  wd.var <- wd.nscore^2

  # 整理结果
  result <- data.frame(
    `X` = wd,
    `x-θ` = wd.diff,
    `正秩` = wd.rank,
    `分位数` = wd.quantile,
    `计分(S+)` = wd.nscore,
    `(S+)²` = wd.var,
    check.names = FALSE
  )
  index <- order(wd.absdiff)
  result <- result[index,]
  w <- sum(wd.nscore)
  var <-  sum(wd.var)
  Zvalue <- (w - 0) / sqrt(var)
  alpha <- 1 - conf.level

  if (alternative == 'two.sided') {
    p.val <- 2 * (1 - pnorm(abs(Zvalue), 0, 1))
  } else if (alternative == 'less') {
    p.val <- pnorm(Zvalue, 0, 1)
  } else if (alternative == 'greater') {
    p.val <- 1 - pnorm(Zvalue, 0, 1)
  }
  return(list(
    `运算结果` = result,
    `正态计分` = w,
    `方差` = var,
    `Z值` = Zvalue,
    `p值` = p.val,
    `是否通过检验` = p.val < alpha
    ))
}


DeadNum <- c(4, 6, 9, 15, 31, 33, 36, 65, 77, 88)
NScoreTest(DeadNum,34,'less')

