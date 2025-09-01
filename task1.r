# 加载必要的包
library(dplyr)
library(ggplot2)
library(readr)
order_data = CourseProject_Order

# 删除NoReview行
order_data_clean <- order_data %>%
  filter(review != 'NoReview')
# 检查缺失值
colSums(is.na(order_data_clean))
# 处理缺失值（例如，删除包含缺失值的行）
order_data_clean <- order_data_clean %>%
  na.omit()


order_data_clean$review <- as.factor(order_data_clean$review)



# 假设支付金额超过99.5百分位数或低于0.5百分位数为异常值，进行删除
quantiles <- quantile(order_data_clean$pay_amount, probs = c(0.005, 0.995))
order_data_clean <- order_data_clean %>%
  filter(pay_amount >= quantiles[1] & pay_amount <= quantiles[2])

# 假设数量超过99.5百分位数或低于0.5百分位数为异常值，进行删除
quantiles_qty <- quantile(order_data_clean$item_qty, probs = c(0.005, 0.995))
order_data_clean <- order_data_clean %>%
  filter(item_qty >= quantiles_qty[1] & item_qty <= quantiles_qty[2])

# 绘制菜鸟组箱线图，使用对数变换
ggplot(order_data_clean %>% filter(if_cainiao == 1), aes(x = as.factor(review), y = log1p(pay_amount))) +
  geom_boxplot(outlier.shape = NA) +  # 隐藏异常值点
  stat_boxplot(geom ='errorbar', width = 0.2) +  # 添加误差线
  ggtitle("Log(Payment Amount) vs Logistic Service Rating (Cainiao)") +
  xlab("Logistic Service Rating") +
  ylab("Log(Payment Amount)") +
  theme_minimal()



# 绘制菜鸟组箱线图，使用对数变换
ggplot(order_data_clean %>% filter(if_cainiao == 0), aes(x = as.factor(review), y = log1p(pay_amount))) +
  geom_boxplot(outlier.shape = NA) +  # 隐藏异常值点
  stat_boxplot(geom ='errorbar', width = 0.2) +  # 添加误差线
  ggtitle("Log(Payment Amount) vs Logistic Service Rating (Cainiao)") +
  xlab("Logistic Service Rating") +
  ylab("Log(Payment Amount)") +
  theme_minimal()


# 绘制菜鸟组不同承诺时间下的物流评分分布图（条形图）
ggplot(order_data_clean %>% filter(if_cainiao == 1), aes(x = as.factor(promise), fill = as.factor(review))) +
  geom_bar(position = "fill") +
  ggtitle("Promise Time vs. Logistic Service Rating (Cainiao)") +
  xlab("Promise Time") +
  ylab("Proportion") +
  scale_fill_manual(values = c("red", "orange", "yellow", "green", "purple", "blue"),
                    name = "Logistic Service Rating") +
  theme_minimal()


# 绘制非菜鸟组不同承诺时间下的物流评分分布图（条形图）
ggplot(order_data_clean %>% filter(if_cainiao == 0), aes(x = as.factor(promise), fill = as.factor(review))) +
  geom_bar(position = "fill") +
  ggtitle("Promise Time vs. Logistic Service Rating (Non-Cainiao)") +
  xlab("Promise Time") +
  ylab("Proportion") +
  scale_fill_manual(values = c("red", "orange", "yellow", "green", "purple", "blue"),
                    name = "Logistic Service Rating") +
  theme_minimal()


# 绘制菜鸟组中 item_qty 与评分关系的条形图
ggplot(order_data_clean %>% filter(if_cainiao == 1), aes(x = as.factor(item_qty), fill = as.factor(review))) +
  geom_bar(position = "fill") +
  ggtitle("Item Quantity vs. Logistic Service Rating (Cainiao)") +
  xlab("Item Quantity") +
  ylab("Proportion") +
  scale_fill_manual(values = c("red", "orange", "yellow", "green", "purple", "blue"),
                    name = "Logistic Service Rating") +
  theme_minimal()



# 绘制非菜鸟组中 item_qty 与评分关系的条形图
ggplot(order_data_clean %>% filter(if_cainiao == 0), aes(x = as.factor(item_qty), fill = as.factor(review))) +
  geom_bar(position = "fill") +
  ggtitle("Item Quantity vs. Logistic Service Rating (Non-Cainiao)") +
  xlab("Item Quantity") +
  ylab("Proportion") +
  scale_fill_manual(values = c("red", "orange", "yellow", "green", "purple", "blue"),
                    name = "Logistic Service Rating") +
  theme_minimal()


# 确保review列为数值型，移除无法转换为数值的行
order_data_clean <- order_data_clean %>%
  mutate(review = as.numeric(review)) %>%
  filter(!is.na(review))

library(lubridate)
# 删除NoReview行并确保review列为数值型
order_data_clean <- order_data %>%
  filter(review != 'NoReview') %>%
  mutate(review = as.numeric(review)) %>%
  filter(!is.na(review) & review >= 0 & review <= 5)  # 移除无法转换为数值的行，并确保review在合理范围内
# 按日期计算每日平均review
daily_avg_review <- order_data_clean %>%
  group_by(day) %>%
  summarize(avg_review = mean(review))

# 以7天为周期计算每周平均review
weekly_avg_review <- daily_avg_review %>%
  mutate(week = as.numeric(format(day, "%U"))) %>%
  group_by(week) %>%
  summarize(avg_review = mean(avg_review))

# 绘制折线图
ggplot(weekly_avg_review, aes(x = week, y = avg_review)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 1) +
  ggtitle("Average Logistic Service Rating by Week") +
  xlab("Week") +
  ylab("Average Logistic Service Rating") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid.major = element_line(color = "gray", size = 0.5),
    panel.grid.minor = element_line(color = "gray", size = 0.25)
  )




library(ggplot2)
df = CourseProject_Merchant


# 去除异常值
df_clean <- df %>% filter(avgLogisticScore > 0)

# 分箱
df_clean$pcuv_bin <- cut(df_clean$pcuv, breaks=10)
ggplot(df_clean, aes(x=pcuv_bin, y=avgLogisticScore)) +
  geom_boxplot() +
  labs(title="Logistic Score vs PC Unique Visitors (Binned)", x="PC Unique Visitors (Binned)", y="Average Logistic Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 直方图
ggplot(order_data_clean, aes(x=review)) +
  geom_histogram(binwidth=0.1, fill="blue", color="black", alpha=0.7) +
  labs(title="Distribution of Average Logistic Scores", x="Average Logistic Score", y="Frequency")

# 密度图
ggplot(df_clean, aes(x=avgLogisticScore)) +
  geom_density(fill="blue", alpha=0.7) +
  labs(title="Density of Average Logistic Scores", x="Average Logistic Score", y="Density")
