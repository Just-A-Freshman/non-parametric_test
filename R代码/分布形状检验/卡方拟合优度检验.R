# 验证两种标准化公式的渐近性质
set.seed(123)  # 设置随机种子保证结果可重复

# 参数设置
k <- 5         # 类别数
p_true <- c(0.1, 0.2, 0.3, 0.25, 0.15)  # 真实的类别概率
n_sim <- 10000 # 模拟次数
sample_sizes <- c(30, 100, 500, 1000)  # 不同的样本量

# 创建图形布局
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

# 对每个样本量进行模拟
for (n in sample_sizes) {
  cat("正在处理样本量 n =", n, "\n")

  # 存储两种标准化的结果
  z1_values <- numeric(n_sim)  # 方法1: (O-E)/sqrt(E)
  z2_values <- numeric(n_sim)  # 方法2: (O-E)/sqrt(E(1-p)))

  for (i in 1:n_sim) {
    # 生成多项分布数据
    counts <- rmultinom(1, size = n, prob = p_true)
    observed <- counts[, 1]
    expected <- n * p_true

    # 选择一个特定的类别进行验证（这里选择第3个类别）
    j <- 3
    O_j <- observed[j]
    E_j <- expected[j]
    p_j <- p_true[j]

    # 方法1: Pearson标准化 (O-E)/sqrt(E)
    z1 <- (O_j - E_j) / sqrt(E_j)

    # 方法2: 使用真实方差标准化 (O-E)/sqrt(E(1-p))
    z2 <- (O_j - E_j) / sqrt(E_j * (1 - p_j))

    z1_values[i] <- z1
    z2_values[i] <- z2
  }

  # 绘制QQ图比较与正态分布
  plot_title <- paste("n =", n)

  # 方法1的QQ图
  qqnorm(z1_values, main = paste(plot_title, "\n方法1: (O-E)/√E"),
         cex.main = 0.9, xlab = "理论分位数", ylab = "样本分位数")
  qqline(z1_values, col = "red", lwd = 2)

  # 方法2的QQ图
  qqnorm(z2_values, main = paste(plot_title, "\n方法2: (O-E)/√[E(1-p)]"),
         cex.main = 0.9, xlab = "理论分位数", ylab = "样本分位数")
  qqline(z2_values, col = "blue", lwd = 2)

  # 计算并显示统计量
  cat("样本量", n, ":\n")
  cat("  方法1 - 均值:", round(mean(z1_values), 4),
      "方差:", round(var(z1_values), 4), "\n")
  cat("  方法2 - 均值:", round(mean(z2_values), 4),
      "方差:", round(var(z2_values), 4), "\n\n")
}

# 重置图形参数
par(mfrow = c(1, 1))

# 额外：绘制密度曲线比较
library(ggplot2)
library(reshape2)

# 使用最大样本量进行详细的密度比较
n_large <- 1000
z1_large <- numeric(n_sim)
z2_large <- numeric(n_sim)

for (i in 1:n_sim) {
  counts <- rmultinom(1, size = n_large, prob = p_true)
  observed <- counts[, 1]
  expected <- n_large * p_true

  j <- 3
  O_j <- observed[j]
  E_j <- expected[j]
  p_j <- p_true[j]

  z1_large[i] <- (O_j - E_j) / sqrt(E_j)
  z2_large[i] <- (O_j - E_j) / sqrt(E_j * (1 - p_j))
}

# 创建数据框
df <- data.frame(
  Method1 = z1_large,
  Method2 = z2_large
)

# 转换为长格式
df_long <- melt(df, variable.name = "Method", value.name = "Z")

# 绘制密度图
ggplot(df_long, aes(x = Z, color = Method, fill = Method)) +
  geom_density(alpha = 0.3) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1),
                color = "black", linetype = "dashed", size = 1) +
  labs(title = "两种标准化方法的密度比较 (n=1000)",
       x = "标准化值", y = "密度") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("(O-E)/√E", "(O-E)/√[E(1-p)]")) +
  scale_fill_manual(values = c("red", "blue"),
                    labels = c("(O-E)/√E", "(O-E)/√[E(1-p)]")) +
  xlim(-4, 4)

# 卡方统计量的验证
cat("\n=== 卡方统计量验证 ===\n")

# 计算完整的卡方统计量
n <- 1000
chi1_values <- numeric(n_sim)  # 使用Pearson标准化
chi2_values <- numeric(n_sim)  # 使用方差标准化

for (i in 1:n_sim) {
  counts <- rmultinom(1, size = n, prob = p_true)
  observed <- counts[, 1]
  expected <- n * p_true

  # 方法1: Pearson卡方统计量
  chi1 <- sum((observed - expected)^2 / expected)

  # 方法2: 使用真实方差的卡方统计量
  chi2 <- sum((observed - expected)^2 / (expected * (1 - p_true)))

  chi1_values[i] <- chi1
  chi2_values[i] <- chi2
}

# 绘制卡方统计量的QQ图
par(mfrow = c(1, 2))

# 理论卡方分布的分位数
theory_quantiles <- qchisq(ppoints(n_sim), df = k - 1)

# 方法1的QQ图
qqplot(theory_quantiles, chi1_values,
       main = "Pearson卡方统计量\n与理论卡方分布比较",
       xlab = "理论卡方分位数", ylab = "样本分位数")
abline(0, 1, col = "red", lwd = 2)

# 方法2的QQ图
qqplot(theory_quantiles, chi2_values,
       main = "方差标准化卡方统计量\n与理论卡方分布比较",
       xlab = "理论卡方分位数", ylab = "样本分位数")
abline(0, 1, col = "red", lwd = 2)

# 显示卡方统计量的均值和方差
cat("Pearson卡方统计量 - 均值:", round(mean(chi1_values), 4),
    "理论期望:", k-1, "\n")
cat("方差标准化卡方统计量 - 均值:", round(mean(chi2_values), 4),
    "理论期望:", k-1, "\n")
