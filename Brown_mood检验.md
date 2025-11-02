
## 核心思想
将X和Y两组样本混合后，混合样本的中位数应该和混合前两组样本的中位数接近。

## 假设(双侧检验)
$H_0: M_x = M_y \leftrightarrow H_1: M_x \neq M_y$

## 统计量
记双样本分别为$X$和$Y$，混合后中位数为$M_{xy}$。
那么，有计数：
①#$(X < M_{xy})$
②#$(X > M_{xy})$
③#$(Y < M_{xy})$
④#$(Y > M_{xy})$
我们通常取②#$(X > M_{xy})$，这就是$Brown\_mood$检验的$A$统计量。

# 拒绝域
如果原假设成立, 那么$②$和$④$应该接近。不难注意到$② + ④ = n$ 。因此$②$太大或太小都会落入拒绝域。
- 左侧检验拒绝域是是$A$太小，所以p值就是左尾概率(lower_tail)；
- 右侧检验拒绝域是$A$太大，所以p值就是右尾概率(upper_tail);
- 双侧检验拒绝域是$A$不能太小或太大。我们先找到更极端的一侧，也就是：min(lower_tail, upper_tail)，这是一侧的p值；另外一侧由于分布的对称性，p值相同。故p值就是：2 * min(lower_tail, upper_tail)

## 精确分布
记$M$是$X$的样本数；$N - M$是$Y$的样本数。$N$就是混合样本的样本数。
原假设成立的情况下，$X$和$Y$的数据均匀分布在混合样本中。于是我们可以换一个角度，**认为X和Y就是从混合样本中抽取的**。
设想一下，这个混合样本是从总体中抽取的随机样本，但恰好满足$M$个样本是属于$X$，$N - M$个样本属于$Y$。那么大于中位数且属于$X$的样本数A服从什么分布呢？
这个问题就可以看做是经典的不放回抽样问题。注意混合样本中所有数据都是不确定！
- 样本量为$N$随机混合样本中，我们选取$n$个变量，让它们大于中位数；
- $n$个变量中，有$A$个是从$X$中抽取的，$n - A$个是从$Y$抽取的。

于是：
$A \sim h(n, N, M)$


## 近似分布
PS: 大部分非参数检验的统计量，当n足够大时都会服从正态分布。
利用超几何分布的期望和方差的结论。
记 $p = M / N$
- 期望：$E(A) = np$
- 方差：$D(A) = np(1 - p) \cdot \frac{N - n}{N - 1}$

特别的，当$N$较大时，有时我们会把$\frac{N - n}{N - 1}$近似成$\frac{N - n}{N}$。这完全看个人习惯，此处我们不采用近似方差。近似分布：
$A \rightarrow N(np, np(1-p) \cdot \frac{N - n}{N - 1})$

## 补充细节
一般的中位数检验在处理**数值恰好等于中位数**(记作$X = M_e$)的情况时，选择全部剔除。但我本人并不认可这种做法，因为它们没考虑这会导致数据的中位数发生变化，并可能进一步让大于中位数的数字个数发生变化。另外，这可能对样本量造成明显影响。这都会较为明显地影响p值的计算。例如：
- 原数据：$[1, 2, 2, 2, 2, 3, 3]$， 
$M_e = 2$，$\#(x > M_e) = 2$, $\#(x < M_e) = 1$
- 剔除后：$[1, 3, 3]$, 
$M_e = 3$,  $\#(x > M_e) = 1$, $\#(x < M_e) = 1$

假设我们继续剔除下去，样本直接就只剩一个了！

所以，我想到一种更简单直接的方法是"单一遗弃法"。不难注意到：
- 如果样本量是偶数，一定不存在$X = M_e$；
- 仅当样本量是奇数，才可能出现$X = M_e$

所以，我们只需剔除1个恰好等于中位数的样本，原本样本量为奇数就会变成偶数，重新计算中位数后，一定不会存在$X = M_e$

## R代码实现(精确分布)
```
# 备注: Brown_mood检验本质上就是符号检验的双样本扩展

X <- c(698,688,675,656,655,648,640,639,620)
Y <- c(780,754,740,712,693,680,621)


# 方法一：直接调用函数
response <- c(X, Y)
fact <- c(rep('A',length(X)),rep('B',length(Y))) |> as.factor()
RVAideMemoire::mood.medtest(response~fact)


# 方法二：个人实现
BM.test = function(
    x,
    y,
    alternative = c("two.sided", "less", "greater")
){
  alternative <- match.arg(alternative)
  combine = c(x, y)

  # 计算原始混合中位数
  Mxy_original = median(combine)

  # 处理等于中位数的情况
  equal_to_median = combine == Mxy_original
  n_equal = sum(equal_to_median)

  if (n_equal > 0) {
    first_equal_idx = which(equal_to_median)[1]
    if (first_equal_idx <= length(x)) {
      x_clean = x[-first_equal_idx]
      y_clean = y
    } else {
      x_clean = x
      y_clean = y[-(first_equal_idx - length(x))]
    }
    combine_clean = c(x_clean, y_clean)
    Mxy = median(combine_clean)
  } else {
    x_clean = x
    y_clean = y
    combine_clean = combine
    Mxy = Mxy_original
  }

  # 统计量 A ~ h(n, N, M)
  A = sum(x_clean > Mxy)
  n = sum(combine_clean > Mxy)
  N = length(combine_clean)
  M = length(x_clean)

  lower_tail_p = phyper(A, M, N - M, n)
  upper_tail_p = phyper(A - 1, M, N - M, n, lower.tail = FALSE)
  if (alternative == "less") {p_value = lower_tail_p}
  if (alternative == "greater") {p_value = upper_tail_p}
  if (alternative == "two.sided") {
    p_value = 2 * min(lower_tail_p, upper_tail_p)
  }
  return(p_value)
}


BM.test(X, Y, alternative = "two.sided")


```