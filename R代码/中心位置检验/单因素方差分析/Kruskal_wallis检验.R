# 使用内置模块


if (sys.nframe() == 0) {
  x = c(
        90, 23, 53, 21, 13,
        47, 34, 28, 18, 5,
        20, 13, 10, 5, 2,
        28, 32, 33, 45, 16,
        34, 28, 52, 40, 10
    )
  groups = rep(1:5, each = 5)

  result <- kruskal.test(x ~ groups)
  result
  # DescTools::DunnTest(x, groups, method = "holm")
}





