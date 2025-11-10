data = c(
  3.7, 3.7, 3.0, 3.9, 2.7,
  7.3, 5.2, 5.3, 5.7, 6.5,
  9.0, 4.9, 7.1, 8.7
)

groups = factor(c(rep(1, 5), rep(2, 5), rep(3, 4)), ordered = TRUE)



clinfun::jonckheere.test(
  data,
  groups,
  alternative = "increasing",
  nperm = 5000
)



