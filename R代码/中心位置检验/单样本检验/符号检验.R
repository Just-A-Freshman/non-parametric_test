# weighted data
SignTest = function(
    wd.val,
    med.val = 0,
    p = 0.5,
    alternative = c("two.sided", "less", "greater"),
    conf.level = 0.95
  ){
  wd.diff <-  wd.val - med.val
  S.pos = sum(wd.diff > 0)
  S.neg = sum(wd.diff < 0)
  n <- S.pos + S.neg
  return(binom.test(S.pos, n, p, alternative, conf.level))
}


test_med = 10
test_data = c(9.8, 10.1, 9.7, 9.9, 9.8, 10.0, 9.7, 10.0, 9.9, 9.8)
SignTest(test_data, test_med)

