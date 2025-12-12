# Durbin检验
## 一、动机
在现实世界中，将不同人的评价作为区组是很常见的情况。考虑以下问题：

一个冰淇淋厂商想测试7种不同口味的受欢迎程度。如果我们请7名测试者，让他们分别品尝7种口味的冰淇淋然后作出评价，是否可行呢？

事实是并不太可行，原因如下：
1. 一个人同时对7个东西作评价，很容易产生偏差。因为人对事物的评价是相对的，而且往往和最近的评价密切相关。(例子补充)
2. 成本较高。受试者需要对7个冰淇淋作出评价，这对受试者要求很高，其必然要求有更高的报酬；

因此，此时仍然采用完全区组设计是非常困难的。更一般的，当我们面临处理数较多，且一个区组难以完全覆盖所有处理的情况时，我们应当采用“均衡的不完全区组设计”——**BIBD**。

## 二、均衡的不完全区组设计
均衡地不完全区组设计的核心，在于“**均衡**”二字。需要满足：
1. 任意两个处理得到相同次数的比较
2. 每个处理出现在相同多个不同的区组中（处理-区组对调亦然）

可以认为，前者是保证了比较的**完全性**；后者则保证了比较的**公平性**（不会偏向于某一个区组）。下面就是一个均衡的不完全区组设计：

```
        冰淇淋1 冰淇淋2 冰淇淋3 冰淇淋4 冰淇淋5 冰淇淋6 冰淇淋7
测试者1       2       3      NA       1      NA      NA      NA
测试者2      NA       3       1      NA       2      NA      NA
测试者3      NA      NA       2       1      NA       3      NA
测试者4      NA      NA      NA       1       2      NA       3
测试者5       3      NA      NA      NA       1       2      NA
测试者6      NA       3      NA      NA      NA       1       2
测试者7       3      NA       1      NA      NA      NA       2
```
对于任意一对冰淇淋，它们都得到1次比较，保证了比较的充分性；而对于每一个测试者，它们都只品尝了3个冰淇淋，不会有任何一位测试者“多尝”，保证了一个冰淇淋的评分不会偏向于某一位测试者的评价。

均衡不完全区组设计有几个重要参数。**只要这几个参数能得到确切值而非变量**，那么它是均衡不完全区组设计就没有疑问了：
\[BIBD(k, b, r, t, \lambda)\]

$k = 7, b = 7$是显然的；$t$是英文treatment处理的缩写，表示每个区组都测试了$t$个处理；上述例子中，每个测试者都需要品尝3个不同的冰淇淋，因此$t = 3$；

再来看$r$，它和$t$刚好反过来，表示每个处理都被$r$个区组测试；上述例子中，每个冰淇淋都需要被三名不同的测试者品尝，因此$r = 3$；

最后是$\lambda$，它表示每对处理均进行了$\lambda$次比较。上述例子中，7个冰淇淋共计$\binom{7}{2} = 21$对，每一对冰淇淋都经过了1次比较，因此$\lambda = 1$；

## 三、检验统计量
### 1. Durbin统计量
\[
D_{\alpha} = \frac{12(k - 1)}{rk(t^2 - 1)} \sum_{j=1}^k R_{.j}^2 - \frac{3r(k - 1)(t + 1)}{t - 1}
\]

让我们对比一下Friedman的检验统计量：
\[
Q = \frac{12}{bk(k + 1)} \sum_{j=1}^k R_{.j}^2 - 3b(k + 1)
\]

不难看出，对于$D$统计量，当$t = k, r = b$时，完全就变成了$Q$统计量。因此可以认为，$D$统计量是$Q$统计量的一般化。其推导过程和Friedman如出一辙，均化简自$\frac{SSt}{MST}$，同时考虑平均秩和自由度的变化即可。(详见本人写的Friedman检验)

### 2. 结的修正
\[
D_{\alpha} = \frac{k - 1}{A - C} \sum_{j=1}^k \left[R_{.j} - \frac{r(t + 1)}{2}\right]^2 \sim \chi^2(k - 1)        
\]
其中，$A = \sum \sum R_{ij}^2, C = \frac{bt(t + 1)^2}{4}$

在无结的情况下，结的修正公式和

### 3. 方差分析法
该方法还适用于不打结的情况，一些研究甚至认为其比$D_{\alpha}$更准确。不过本人尚未找到其更多的文献支持，仅是老师提及了该方法：
\[
D_F = \frac{D_{\alpha} / d_1}{(b(t - 1) - D_{\alpha}) / d_2} \sim F(d_1, d_2)
\]
其中，$d_1 = k - 1, d_2 = bt - b - k + 1$

## 四、事后多重比较
\[
\frac{|R_i - R_j|}{\sqrt{SE}} \sim t(d_2)        
\]
其中：
\[
SE = \frac{(A - C) \cdot 2r}{d_2}(1 - \frac{D_{\alpha}}{b(t - 1)})
\]

因此：当$|R_i - R_j| > t_{1 - \alpha / 2f}(d_2) \cdot \sqrt{SE}$时，落入拒绝域。

## 五、R代码实现
```r
Durbin.test = function(data, use_F = FALSE) {
  # X轴为处理，Y轴为区组
  k <- ncol(data)
  b <- nrow(data)

  bool_mat <- is.na(data)
  check_t <- apply(!bool_mat, 1, sum)
  check_r <- apply(!bool_mat, 2, sum)

  t <- as.integer(check_t[1])
  r <- as.integer(check_r[1])
  if (any(check_t != t)) {stop("处理数不均衡！")}
  if (any(check_r != r)) {stop("区组数不均衡！")}

  rank_mat <-  t(apply(data, 1, rank))
  rank_mat[bool_mat] <- 0

  A <- sum(rank_mat^2)
  C <- b * t * (t + 1)^2 / 4

  tmp <- apply(rank_mat, 2, sum) - r * (t + 1) / 2
  D <- (k - 1) / (A - C) * sum(tmp^2)

  if (use_F) {
    d1 <- k - 1
    d2 <- b * t - b - k + 1
    df <- c(d1, d2)
    D_F <- (D / d1) / ((b * (t - 1) - D) / d2)
    p.value <- pf(D_F, d1, d2, lower.tail = FALSE)
    statistic <- D_F
    stat_name <- "F"
  }
  else {
    df <- k - 1
    p.value <- pchisq(D, df, lower.tail = FALSE)
    statistic <- D
    stat_name <- "Durbin chi-squared"
  }

  result <- list(
    statistic = setNames(statistic, stat_name),
    parameter = c(df = df),
    p.value = p.value,
    method = paste0(c(
        "Durbin's rank sum test for a two-way",
        "balanced incomplete block design"), sep = "\n"
    ),
    data.name = deparse(substitute(data)),
    dimensions = c(k = k, b = b, r = r, t = t)
  )

  class(result) <- "htest"
  return(result)
}


if (sys.nframe() == 0) {
  data_matrix <- matrix(c(
    2, 3, NA, 1, NA, NA, NA,
    NA, 3, 1, NA, 2, NA, NA,
    NA, NA, 2, 1, NA, 3, NA,
    NA, NA, NA, 1, 2, NA, 3,
    3, NA, NA, NA, 1, 2, NA,
    NA, 3, NA, NA, NA, 1, 2,
    3, NA, 1, NA, NA, NA, 2
  ), nrow = 7, byrow = TRUE)

  rownames(data_matrix) <- paste0("测试者", 1:7)
  colnames(data_matrix) <- paste0("冰淇淋种类", 1:7)

  Durbin.test(data_matrix, use_F = TRUE)
}
```
需要注意，传递的矩阵一定要保证每行是一个区组，每列是一个处理。上述代码严格基于第三小节的统计量部分进行实现。
- 直接使用Durbin检验的结果如下
```
Durbin's rank sum test for a two-way
	balanced incomplete block design

data:  data_matrix
Durbin chi-squared = 12, df = 6, p-value =
0.06197
```

- 使用方差分析法(use_F = TRUE)的结果如下：
```
Durbin's rank sum test for a two-way
	balanced incomplete block design

data:  data_matrix
F = 8, df1 = 6, df2 = 8, p-value = 0.004904
```

如果不放心，还是去下载第三方库使用：
```r
library(PMCMRplus)

durbinTest(data_matrix)
```

## 六、补充
关于第三小节提到的**方差分析法**，其仅仅是小样本下的一个修正。因为小样本下Durbin统计量不太能近似到卡方分布；在更一般的情况下，很多Durbin统计量不拒绝原假设的数据，使用方差分析法却会落入拒绝域，而且$p$值非常小，有理由怀疑其计算过于“激进”。

另外，本人并没有找到更多资料支持这个方差分析法，虽然在代码中顺手实现了，但务必谨慎使用。
