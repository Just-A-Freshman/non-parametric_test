# 使用第三方库


if (sys.nframe() == 0) {
  x = list(
    `1` = c(3.7, 3.7, 3.0, 3.9, 2.7),
    `2` = c(7.3, 5.2, 5.3, 5.7, 6.5),
    `3` = c(9.0, 4.9, 7.1, 8.7)
  )
  data = unlist(x)
  groups = factor(
    c(rep(names(x), lengths(x))),
    ordered = TRUE
  )

  result = clinfun::jonckheere.test(
    data,
    groups,
    alternative = "increasing",
    nperm = 5000
  )
  result
}


