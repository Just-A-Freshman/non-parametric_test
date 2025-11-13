if (sys.nframe() == 0) {
  x <- rnorm(50)
  y <- rnorm(60)

  # 单样本，判断是否来自已知分布
  ks.test(x, "pnorm")

  # 双样本，看是否来自相同分布
  ks.test(x, y)
}

