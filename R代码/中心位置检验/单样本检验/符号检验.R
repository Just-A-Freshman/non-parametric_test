sign.test = function(
    x,
    med.val = 0,
    p = 0.5,
    alternative = c("two.sided", "less", "greater"),
    conf.level = 0.95
  ){
  x <-  x - med.val
  S.pos = sum(x > 0)
  S.neg = sum(x < 0)
  n <- S.pos + S.neg
  return(binom.test(S.pos, n, p, alternative, conf.level))
}


if (sys.nframe() == 0) {
  test_med = 10
  test_data = c(9.8, 10.1, 9.7, 9.9, 9.8, 10.0, 9.7, 10.0, 9.9, 9.8)
  sign.test(test_data, test_med)
}


