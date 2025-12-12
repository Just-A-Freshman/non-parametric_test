# Hodges_Lehmmann检验
## 一、概述
我们知道Friedman检验为了让不同区组的水平“对齐”，采用的策略是“区组内编秩”。然而，当处理数$k$太小($k \leq 3$)，每个区组内的秩次范围将十分有限。例如$k=2$时，秩次只有1和2，不同处理的秩和差异最大只能达到b。即使处理效应存在，$Q$值可能不够大，从而效能较低。

为了能充分利用样本数据，同时又保证不同区组的水平“对齐”。HL的策略是“减掉区组效应”。具体而言，我们让每个区组中的数据都减去区组的平均值或中位数，从而拉平不同区组的水平。此时，我们就可以对所有数据进行混合编秩了。

当我们这么做了后，原本的双因素方差分析问题因为去除了区组效应，似乎已经转化成了单因素方差分析问题，可以直接采取Kruskal-Wallis检验了。但事实并没有这么简单：**每个区组的效应都是通过样本计算得到的。这是估计值，并不是真实准确的区组效应**。HL考虑到了这部分并没有完全去除的区组效应，构造了一个特别的$Q$统计量。


## 二、统计量
\[
Q' = \frac{(k - 1)\left[\sum_{j=1}^k R_{.j}^2 - \frac{kb^2(kb + 1)^2}{4}\right]}{\sum_{i,j} R_{ij}^2 - \frac{1}{k}\sum_{i=1}^b R_{i.}^2}
\]
这个统计量的构造比较复杂，不太好准确解读。从大体上看，其结构是：
\[Q' = \frac{处理间平方和}{总平方和 - 区组平方和}\]
尽管其结构复杂，但它的分子和大部分方差分析的结构一样，都是“组间变异”——$SSA$。因此统计量的拒绝方向也是和它们一致，太大说明不同处理差异明显，从而拒绝原假设。

## 三、分布
当零假设成立时:
\[
Q \stackrel{.}{\sim} \chi^2(k - 1)    
\]这个近似分布和Kruskal-Wallis, Friedman检验一样，不多赘述。

## 四、代码实现
```r
HL.test = function(mat, center = c("mean", "median")){
  # X轴为处理，Y轴为区组
  center <- match.arg(center)
  centerFunc <- if (center == "mean") mean else median

  # 减掉区组效应
  outCenterMat <- mat - apply(mat, 1, centerFunc)

  # 混合编秩
  rankMat <- matrix(rank(outCenterMat), ncol = ncol(mat))

  # 计算统计量
  b <- nrow(mat)
  k <- ncol(mat)
  N <- k * b
  riSumSq <- sum(apply(rankMat, 1, sum)^2)
  rjSumSq <- sum(apply(rankMat, 2, sum)^2)
  rijSumSq <- sum(rankMat^2)

  Q <- (k - 1) * (rjSumSq - (k * b^2 * (k * b + 1)^2) / 4) /
       (rijSumSq - riSumSq / k)

  p.value <- pchisq(Q, k - 1, lower.tail = FALSE)

  result <- list(
    statistic = c(Q = Q),
    parameter = c(df = k - 1),
    p.value = p.value,
    method = "Hodges-Lehmann Rank Sum Test",
    data.name = deparse(substitute(mat)),
    ranks = rankMat,
    squared.sums = c(Ri2 = riSumSq, Rj2 = rjSumSq, Rij2 = rijSumSq),
    dimensions = c(treatments = k, blocks = b, observations = N)
  )

  class(result) <- "htest"
  return(result)
}




if (sys.nframe() == 0) {
  A <- c(20.3, 21.2, 18.2, 18.6, 18.5)
  B <- c(25.6, 24.7, 19.3, 19.3, 20.7)
  C <- c(24.0, 23.1, 20.6, 19.8, 21.4)
  mat <- cbind(A, B, C)
  rownames(mat) <- c('I', 'II', 'III', 'IV', 'V')
  result = HL.test(mat, "mean")
  result

  # 测试去除区组效应后的Kruskal-Wallis检验效果
  # outCenterMat <- mat - apply(mat, 1, mean)
  # x = c(outCenterMat)
  # groups <- rep(c("A", "B", "C"), c(5, 5, 5))
  # kruskal.test(x ~ groups)
}

```
本人特别测试过在去除区组效应后，直接使用Kruskal-Wallis检验的效果，发现其计算得到的$p$值更为激进（即更容易落入拒绝域）。这表明未完全消除的区组效应，使我们高估了真实的处理效应。
