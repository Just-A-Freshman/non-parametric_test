# 方法1(U检验)
U_test = function(x, y){
  combine = c(x, y)
  labels = c(rep("A", length(x)), rep("B", length(y)))
  index = order(combine)
  combine = combine[index]
  labels = labels[index]
  result = data.frame(
    `样本` = combine,
    `标签` = labels,
    `秩` = rank(combine)
  )
  print(result)
  return(wilcox.test(x, y, exact = FALSE))
}


wilcox_paired_test = function(X, Y){
  # 方法2(配对样本Wilcox检验)
  # 先随机从y中剔除两个样本
  remove_indices <- sample(length(Y), length(Y) - length(X))
  y <- y[-remove_indices]
  return(wilcox.test(X, Y, paired = TRUE, exact = TRUE))
}


A = c(221, 166, 156, 186, 130, 129, 103, 134, 199, 121, 265, 150, 158, 242, 243, 198, 138, 117)
B = c(448, 593, 507, 428, 807, 342, 512, 350, 672, 589, 665, 549, 451, 492, 514, 391, 366, 469)
U_test(A, B)



