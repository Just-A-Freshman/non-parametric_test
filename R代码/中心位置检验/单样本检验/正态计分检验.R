NScoreTest <- function(
    x,
    med.val=0,
    alternative = c("two.sided", "less", "greater")
  ){
  alternative <- match.arg(alternative)
  n <- length(x)
  xDiff <- x - med.val
  xAbsDiff <- abs(xDiff)
  rankVec <- rank(xAbsDiff)
  xQuantile <- (1 + rankVec / (n + 1)) / 2
  NScoreVec <- qnorm(xQuantile) * sign(xDiff)
  NScoreSq <- NScoreVec^2

  processMat <- data.frame(
    `x` = x,
    `x-θ` = xDiff,
    `|x-θ|` = xAbsDiff,
    `rank` = rankVec,
    `Q` = round(xQuantile, 4),
    `NS` = round(NScoreVec, 4),
    check.names = FALSE
  )
  index <- order(xAbsDiff)
  processMat <- processMat[index, ]
  rownames(processMat) <- NULL

  NScore <- sum(NScoreVec)
  variance <- sum(NScoreSq)
  z <- NScore / sqrt(variance)

  p.value <- switch(
    alternative,
    "less" = pnorm(z),
    "greater" = pnorm(z, lower.tail = FALSE),
    "two.sided" = 2 * pnorm(-abs(z))
  )

  result <- list(
    statistic = c(Z = z),
    parameter = c(NScore = NScore, variance = variance),
    p.value = p.value,
    alternative = alternative,
    method = "Normal Score Test for Median",
    data.name = deparse(substitute(x)),
    null.value = c(median = med.val),
    processMat = processMat
  )

  class(result) <- "htest"
  return(result)
}


if (sys.nframe() == 0) {
  DeadNum <- c(4, 6, 9, 15, 31, 33, 36, 65, 77, 88)
  result <- NScoreTest(DeadNum,34,'less')
  result
}


