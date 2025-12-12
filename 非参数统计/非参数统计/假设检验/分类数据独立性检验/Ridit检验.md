# 1. 引例
$H_0: A、B、C和D四部电影的评分之间无显著性差异$
$H_1: A、B、C和D四部电影的评分之间有显著性差异$
| 电影 | 一星 | 二星 | 三星 | 四星 | 五星 | 合计 |
|------|------|------|------|------|------|------|
| A    | 15   | 45   | 153  | 231  | 56   | 500  |
| B    | 39   | 89   | 198  | 126  | 48   | 500  |
| C    | 89   | 177  | 134  | 88   | 12   | 500  |
| D    | 13   | 23   | 86   | 257  | 121  | 500  |
| 合计 | 156  | 334  | 571  | 702  | 237  | 2000 |

对于这个问题，我们首先问：能不能使用非参数统计中的**Kruskal-Wallis单因素方差分析**？

首先，Krukal-Wallis方法需要给五级评分进行混合编秩，这利用了五级评分的**有序关系**：一星$<$二星$<$三星$<$四星$<$五星。但是秩次与秩次之间是等距的，即将一星和二星，二星和三星之间的差异视为等距。但实际上，不同等级的评分之间的差距很有可能是不同的。这是一种**距离信息**，蕴含在样本数据之中却没有被利用;

我们再问，能不能用**卡方检验**呢？

显然卡方检验更为极端，它甚至没有利用电影评分等级的有序关系，完全平视了所有评分。此外，卡方检验只能判断四部电影之间的评分是否有显著性差异，却不能知道到底是哪部电影更好，哪部更差。


# 2. 核心思想
## 2.1 有序数据变换
问题的关键在于如何从样本中获取有序数据的距离信息呢？Ridit的第一个核心思想用**连续的**累积频率代替原本**离散的**有序数据。累积频率不仅蕴含了原本数据的有序性，还蕴含了有序数据之间的**距离信息**。

那么如何将有序评分$O$转化成累积频率呢？首先，让我们想象一下每个观众内心对电影的打分是一个**连续的分数**$C$。假设$C \in [0, 5)$，我们将$C$的值域按顺序划分成5个等距离的区间，并规定：
- 当$C \in [0, 1)$时，$O$就是“一星”；
- 当$C \in [1, 2)$时，$O$就是“二星”；

依次类推。那么在每个子区间内，哪一个连续分数$C$最具有代表性，可以用它的累积频率来代替真实评分呢？很自然的想法就是区间中点。

在上述例子中，所有电影中“一星”评分，即$C \in [0, 1)$的人数为156人，我们选择$C = 0.5$作为代表，并假定在这一区间内人数的分布是对称的，那么$[0, 0.5)$的人数有$78$个。我们就用累积频率$\frac{78}{2000} \approx 0.039$来代替原来的“一星”。

## 2.2 选择标准组
一个新的问题来了。计算哪一个组的累积频率作为原始有序得分的映射呢？显然不可能让每个组都用自己累积频率作为有序得分映射，这会让不同电影之间没有了可比性。我们需要一个共同的标准。

Ridit的第二个核心思想就是选定一个标准组，用标准组的累积频率作为所有电影共同的有序得分映射。那么，怎么选择标准组呢？Ridit的策略是：
1. 当存在某个组的样本量比其他组要大得多时，使用这个组作为标准组；
2. 如果每个组的样本量都差不多，使用全部组的数据和作为标准组；

在上述电影的例子中，所有电影评分样本量都是500，因此我们选择总和作为标准组，并将原始评分映射为总和的累积频率。

但现在我们要问，为什么不能总是用总样本作为标准组，数据量更大不是拟合更精确吗？从数学上看确实如此，更大的样本量通常能让累积频率更稳定。之所以如此是基于以下两点理由：
1. **差异被稀释**：当使用总和作为标准组时，每个组的数据都参与构建标准分布，这可能导致差异被稀释，降低检验灵敏度；

2. **可解释性降低**：混合总体的性质是模糊的，而个体的性质是清晰的。例如，假设上述例子中存在一部非常经典的电影$B$且有很多评价，我们用它作为标准组，得到的结论就可能是$A$电影相对于已知的经典电影$B$表现得更好。经典电影$B$的拍摄手法，剧情安排都是清晰可知的，和它进行对照后，我们能更明确说出我们的电影$A$好在那。否则，我们就只能说$A$电影相比总体混合的平均水平表现得更好，但具体好在哪呢？我们很难解释。

# 3. Ridit值
### 3.1 标准组的Ridit值
正如前面所言，Ridit检验的第一步是选择标准组，随后计算标准组中各个有序等级的Ridit值。

记$P_{j}$是第$j$个等级的累积频率，$p_{.j}$是第$j$等级自身的频率，共$k$个有序等级。那么第$j$等级的Ridit值定义就是：
\[\begin{aligned}
R_j &= P_{j-1} + \frac{1}{2}p_{.j} = \sum_{i=1}^{j-1} p_{.i} + \frac{1}{2} p_{.j}
\end{aligned}\]

当然它还有很多等价的变形，例如将$p_{.j}$写成$P_{j} - P_{j-1}$然后化简等等。我们不妨计算一下第3个等级的Ridit值，别忘了我们是用总和作为标准组：
\[
R_3 = \frac{156 + 334}{2000} + \frac{1}{2} \times \frac{571}{2000} = 0.38775
\]

### 3.2 标准组Ridit值的期望与方差
标准组的每一个等级的Ridit都是一个随机变量。作为随机变量，我们很自然需要研究它的期望和方差。让我们画出标准组Ridit值的分布列：
| 评分 | 一星 | 二星 | 三星 | 四星 | 五星 |
|------|------|------|------|------|------|
| 总人数 | 156  | 334  | 571  | 702  | 237  |
| Ridit值 | 0.039  | 0.1615  | 0.38775  | 0.706  | 0.94075  |
| 概率p | 0.078  | 0.167  | 0.2855  | 0.3510  | 0.1185  |

自然，按照期望的定义：
\[E(R) = \sum_{j=1}^k p_j R_j\]

特别指出，标准组的Ridit值期望恒等于0.5。证明的核心步骤是使用$(\sum_{j=1}^k p_j)^2 = 1^2$，此处不作展开。

有了期望恒等于$0.5$这条性质，方差自然就是：
\[Var(R) = \sum_{j=1}^k p_j (R_j - 0.5)^2\]记标准组的总样本量为$N$，第$j$级评分的人数为$n_i$，那么只需将$p_j$替换成$n_j / N$，展开$(R_j - 0.5)^2$可以得到：
\[
Var(R) = \frac{1}{N}\sum_{j=1}^k n_j R_j^2 - \frac{1}{4}    
\]

另外，还有一个**贝塞尔校正版本**，实际上就是将上式乘以$\frac{N}{N - 1}$：
\[Var(R) = \frac{1}{N - 1}\sum_{j=1}^k n_j R_j^2 - \frac{1}{4}\frac{N}{N - 1}\]

然而这一校正我个人认为这是非必要的。这一点我们可以往前追溯：为什么样本方差要用$(n-1)$作分母，这是因为样本方差的计算是基于样本均值的。样本均值会有偏向性的拟合样本数据点，使得方差系统性地偏小，因此才用更小的分母来使方差调大；但现在情况不同，虽然看似我们的样本均值是通过样本计算得到的，但它恒等于$0.5$，表明**总体均值和样本均值是一样的**，不存在“样本均值会有偏向性的拟合样本数据点”这一情况。文献和教材多采用贝塞尔校正版本，多是出于习惯、保守性，或与软件保持一致。

## 3.3 对照组平均Ridit值的期望与方差
记第$s$个对象的第$k$个等级的Ridit值为$R_{sj}$。在原假设成立的情况下，对照组的每个$R_{sj}$独立同分布的，期望为$0.5$，方差为$\sigma_R^2$。当对照组的样本量较大时，由中心极限定理可以推得：
\[
\bar{R_s} \stackrel{.}{\sim} N(0.5, \frac{\sigma_R^2}{N_s}) 
\]

# 4. 统计量
不难发现，$\bar{R_s}$的大小其实就能直接反映这个组的评分高低。如果累积频率的计算和分数的提升方向是一致的，那么$\bar{R_s}$越大说明这个组的评分就越高。

记所有组数为$m$，对照组数为$m'$，根据对照组数量的不同，我们又可以进一步区分出单样本($m' = 1$)，双样本($m' = 2$)和多样本检验($m' > 2$)。

## 4.1 单样本检验
利用:
\[
\bar{R_s} \stackrel{.}{\sim} N(0.5, \frac{\sigma_R^2}{N_s}) 
\]
可以得到：
\[
Z_s = \frac{\bar{R_s} - 0.5}{\sqrt{\sigma_R^2 / N_s}} \stackrel{.}{\sim} N(0, 1)    
\]

使用$Z$统计量就可以直接进行单样本检验了。

## 4.2 双样本检验
利用：
\[
\bar{R_s} \stackrel{.}{\sim} N(0.5, \frac{\sigma_R^2}{N_s})， \bar{R_t} \stackrel{.}{\sim} N(0.5, \frac{\sigma_R^2}{N_t})
\]
可以得到：
\[\bar{R_s} - \bar{R_t} \stackrel{.}{\sim} N(0, \left(\frac{1}{N_s} + \frac{1}{N_t}\right)\sigma_R^2)\]

因此：
\[
Z = \frac{\bar{R_s} - \bar{R_t}}{\sqrt{\left(\frac{1}{N_s} + \frac{1}{N_t}\right)\sigma_R^2})} \stackrel{.}{\sim} N(0, 1)    
\]

使用该$Z$统计量就可以直接进行双样本检验了。

## 4.3 多样本检验
利用：
\[
Z_s = \frac{\bar{R_s} - 0.5}{\sqrt{\sigma_R^2 / N_s}} \stackrel{.}{\sim} N(0, 1)
\]
多个不同组的$Z$值平方求和可以得到式子1：
\[
\sum_{s=1}^m Z_s^2 = \sum_{s=1}^m \frac{N_s\left(\bar{R_s} - 0.5\right)^2}{\sigma_R^2} \stackrel{.}{\sim} \chi^2(m - 1)    
\]

自由度为$(m−1)$是因为：
1. 如果总和作为标准组，那么等级的Ridit值就是通过共同组计算得到的。此时各组的Ridit平均值并不是相互独立，存在约束：各组的Ridit均值加权平均恒等于0.5，自由度要减1，故自由度为$(m - 1)$。
2. 如果是选取最大组作为标准组时，那么控制组数量就要从$m$个减去一个标准组变成$(m - 1)$个。此时各组的Ridit平均值是相互独立的，因此自由度为$(m - 1)$。

特别的，当**标准组**的样本量足够大且分布是**连续均匀分布**时，有$\sigma_R^2 \approx \frac{1}{12}$。连续均匀分布就是指连续得分$C$对应的评分人数分布是均匀的。当标准组中不同等级的人数接近时，我们可以**近似**认为标准组服从连续均匀分布，此时得到简化式子2：
\[
W = 12\sum_{s=1}^m N_s\left(\bar{R_s} - 0.5\right)^2 \stackrel{.}{\sim} \chi^2(m - 1)
\]

实际上这种情况是比较很少见的。因此我们更多考虑使用式子1，又或者使用了式子2后进行结的校正，结的修正公式请参照其他出处。

# 5. R代码实现
注意这一实现使用的标准组的Ridit方差为**贝塞尔校正版本**，而卡方统计量的公式则采用$4.3$的式子1。代码如下：
```r
ridit.test = function(
    data,
    std_group_ratio = 3,
    conf = 0.95
  ) {
  # 使用双侧检验，比较不同组请看图
  if (is.null(nrow(data))) {
    stop("检验数据中至少包含两个对象！")
  }
 
  sample_size <- sum(data)
  group_sample_sum <- rowSums(data)
  std_group_idx <- -1
  df <- nrow(data) - 1
  group_names <- if (!is.null(rownames(data))) {
    rownames(data)
  } else {
    paste("G", 1:nrow(data))
  }
 
  # 1. 确定标准组
  sort_sample_sum <- sort(group_sample_sum, decreasing = TRUE)
  if (sort_sample_sum[1] >= 3 * sort_sample_sum[2]) {
    std_group_idx <- which.max(group_sample_sum)
    group_sample_sum <- group_sample_sum[-std_group_idx]
    std_group <- data[std_group_idx, ]
    control_group <- data[-std_group_idx, ]
    std_group_name <- group_names[std_group_idx]
    control_names <- group_names[-std_group_idx]
  } else {
    std_group <- colSums(data)
    control_group <- data
    std_group_name <- "merged"
    control_names <- group_names
  }
 
  # 2. 计算每个评分映射到的Ridit值
  N_std <- sum(std_group)
  cum_freq <- c(0, cumsum(std_group)[-length(std_group)])
  std_ridit <- (cum_freq + std_group / 2) / N_std
 
  # 3. 计算对照组的平均Ridit值
  control_avg_ridit <- (control_group / group_sample_sum) %*% std_ridit
 
  # 4. 计算标准组的方差
  std_variance <- ((1 / N_std) * sum(std_group * std_ridit^2) - 1/4) *
                 (N_std / (N_std - 1))
 
  control_SE <- sqrt(std_variance / group_sample_sum)
 
  # 5. 根据对照组数量的不同安排不同检验
  if (is.null(nrow(control_group)) || nrow(control_group) == 2) {
    if (is.null(nrow(control_group))) {
      Z <- (control_avg_ridit - 0.5) / control_SE
    }
    else {
      avg_R_diff <- abs(control_avg_ridit[1] - control_avg_ridit[2])
      combine_SE <- sqrt(sum(1 / group_sample_sum) * std_variance)
      Z <- avg_R_diff / combine_SE
    }
    statistic <- c(Z = Z)
    p.value <- pnorm(Z, lower.tail = FALSE) * 2
  } else {
    Z_list <- (control_avg_ridit - 0.5) / control_SE
    W <- sum(Z_list^2)
    statistic <- c(W = W)
    p.value <- pchisq(W, df, lower.tail = FALSE)
  }
 
  # 6. 画图
  Z_line <- qnorm(1 - (1 - conf) / 2)
  control_ci_lower <- control_avg_ridit - Z_line * control_SE
  control_ci_upper <- control_avg_ridit + Z_line * control_SE
  control_groups_num <- max(nrow(control_group), 1)
 
  margin <- 0.2
  if (control_groups_num == 1) {
    x_positions <- 1.5
    xlim_left <- 0.5
    xlim_right <- 2.5
  } else {
    # 计算位置，让线条均匀分布但不在最边缘
    total_width <- control_groups_num - 1
    start <- 1 + margin
    end <- control_groups_num - margin
    x_positions <- seq(start, end, length.out = control_groups_num)
    xlim_left <- 0.5
    xlim_right <- control_groups_num + 0.5
  }
 
  plot(
    0, 0,
    ylim = c(0, 1),
    xlim = c(xlim_left, xlim_right),
    xlab = "group",
    ylab = "R score",
    main = "Ridit value confidence interval",
    col = "gray7",
    xaxt = "n",
    yaxt = "n"
  )
  abline(h = 0.5, col = "black", lty = 2, lwd = 1.5)
  for (i in 1:control_groups_num) {
    lines(
      c(i, i),
      c(control_ci_lower[i], control_ci_upper[i]),
      col = "black",
      lwd = 2
    )
  }
 
  # 添加X轴标签
  axis(1, at = 1:control_groups_num, labels = control_names)
  axis(2, at = 0.5, labels = std_group_name)
 
  result <- list(
    statistic = statistic,
    df = df,
    p.value = p.value,
    parameter = c(df = df),
    data.name = deparse(substitute(data)),
    std_variance = std_variance,
    std_redit = std_ridit,
    control_avg_ridit = control_avg_ridit,
    method = "ridit Test",
    control_group = control_group,
    std_group = std_group
  )
 
  class(result) <- "htest"
  return(result)
}
```

# 6. 总结
Ridit检验相较Kruskal-Wallis检验最大创新点在于**有序数据距离信息的利用**和“**标准组**”的选取。但如果说有序数据之间的距离是均匀的，即电影的五级评分人数都差不多，使用Kruskal-Wallis检验或者像秩和检验，实际上区别并不大，甚至更简单直接。