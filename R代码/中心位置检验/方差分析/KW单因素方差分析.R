# 单因素方差分析
data = c(
  3.7, 3.7, 3.0, 3.9, 2.7,
  7.3, 5.2, 5.3, 5.7, 6.5,
  9.0, 4.9, 7.1, 8.7
)

groups = c(rep("生活方式1", 5), rep("生活方式2", 5), rep("生活方式3", 4))

kruskal.test(data ~ groups)

# 多重比较
DescTools::DunnTest(data, groups, method = "holm")

