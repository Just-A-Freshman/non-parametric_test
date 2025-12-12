# Mantel-Haenszel检验
## 一、动机
**辛普森悖论**使得当我们将一个$2 \times 2$的列联表数据通过某一个因素进一步拆分时，可能得到截然相反的结论。

## 二、解决策略
分层做卡方检验，最后将结果进行综合。

需要注意，Mantel-Haenszel检验仅适用于$h$层的$2 \times 2$列联表。对于更一般的情况——$h$层非$2×2$列联表（例如，$h$层$r×c$表，其中$r>2$和$c>2$）——存在广义的Cochran-Mantel-Haenszel方法。

## 三、检验统计量
记分层结构$h = 1, 2, ..., k, n_{hij}$表示$h$层$2 \times 2$列联表的观测频数，$h$表示多层$2 \times 2$列联表的第$h$层，第$h$层的观测总频数为$n_h$，则$\sum_{h=1}^k n_h = n$

在原假设成立的情况下，我们可以把数据看成是通过超几何分布得来的(Fisher检验的思想)。具体而言，我们有：
\[n_{11} \sim h(n_{.1}, n_{..}, n_{1.})\]
套用超几何分布的性质。期望$=np$，方差$=np(1-p) \cdot \frac{N - n}{N - 1}$，其中$p = \frac{n_{1.}}{n_{..}}$可以得到：
- 期望
\[E(n_{h11}) = \frac{n_{h1.} \cdot n_{h \cdot 1}}{n_h}\]
- 方差
\[\mathrm{var}(n_{h11}) = \frac{n_{h1.} \cdot n_{h2.} \cdot n_{h \cdot 1} \cdot n_{h \cdot 2}}{n_h^2 (n_h - 1)}\]

最终的统计量：
\[
Q_{MH} = \frac{\left(\sum_{h=1}^{k} n_{h11} - \sum_{h=1}^{k} E n_{h11}\right)^2}{\sum_{h=1}^{k} \mathrm{var}(n_{h11})} \sim \chi^2(1)
\]

## 四、R语言实现
```
# 创建一个3层的2x2表数据
data <- array(c(
  12, 25,   # 层1: 暴露组和非暴露组的病例数
  8,  20,   # 层1: 暴露组和非暴露组的对照数
  15, 18,   # 层2: 暴露组和非暴露组的病例数
  10, 22,   # 层2: 暴露组和非暴露组的对照数
  20, 15,   # 层3: 暴露组和非暴露组的病例数
  12, 25    # 层3: 暴露组和非暴露组的对照数
), dim = c(2, 2, 3))

dimnames(data) <- list(
  暴露 = c("是", "否"),
  疾病 = c("病例", "对照"),
  层 = c("层1", "层2", "层3")
)


mantelhaen.test(data)
```
检验结果：
```
Mantel-Haenszel chi-squared test with continuity correction

data:  data
Mantel-Haenszel X-squared = 3.9776, df = 1, p-value = 0.04611
alternative hypothesis: true common odds ratio is not equal to 1
95 percent confidence interval:
 1.052717 3.367217
sample estimates:
common odds ratio 
         1.882744 
```
p值略小于0.05，表明暴露与否与疾病无关。


噗，有点懒得写这么多了，大概就这样。