U.test = function(
    x, y, alternative = c("two.sided", "less", "greater")
  ){
  combine = c(x, y)
  labels = c(rep("A", length(x)), rep("B", length(y)))
  index = order(combine)
  combine = combine[index]
  labels = labels[index]
  rankMat = data.frame(
    `sample` = combine,
    `label` = labels,
    `rank` = rank(combine)
  )
  result = wilcox.test(x, y, exact = FALSE)
  result$ranks <- rankMat
  return(result)
}




if (sys.nframe() == 0) {
  A = c(221, 166, 156, 186, 130, 129, 103, 134, 199, 121, 265, 150, 158, 242, 243, 198, 138, 117)
  B = c(448, 593, 507, 428, 807, 342, 512, 350, 672, 589, 665, 549, 451, 492, 514, 391, 366, 469)
  result <- U.test(A, B, alternative = "two.sided")
  result
}





