## 菜鸟物流服务评分研究 | Cainiao Logistics Service Rating Research



### 项目简介

本研究基于电商订单数据，分析菜鸟物流与非菜鸟物流在服务评分上的表现差异，探究支付金额、承诺配送时间、商品数量等因素对物流服务评分的影响。

**研究问题**：在不同订单条件下，菜鸟物流是否能提供更稳定的高质量服务？

###  数据来源

项目使用两个主要数据文件：
- `CourseProject_Order.csv` - 订单级别数据 (主要数据集)
- `CourseProject_Merchant.csv` - 商家级别数据 (辅助数据集)

###  研究变量

#### 因变量
- **物流服务评分** (review): 客户对物流服务的评分（1-5分）

#### 自变量
- **支付金额** (pay_amount): 订单支付金额
- **承诺时间** (promise): 承诺配送天数
- **商品数量** (item_qty): 订单中包含的商品数量
- **物流类型** (if_cainiao): 是否使用菜鸟物流（1=是，0=否）

###  程序架构

├── data/
│   ├── CourseProject_Order.csv       # 主要订单数据
│   └── CourseProject_Merchant.csv    # 商家数据
├── scripts/
│   └── analysis.R                    # 完整分析代码
├── outputs/
│   ├── figures/                      # 生成的所有图表
│   └── results/                      # 分析结果
├── README.md                         # 项目说明
└── Research_Report.pdf               # 完整研究报告


###  分析方法

#### 数据预处理
```r
# 清除未评分订单和缺失值
order_data_clean <- order_data %>%
  filter(review != 'NoReview') %>%
  na.omit()

# 异常值处理（99.5%百分位数）
quantiles <- quantile(order_data_clean$pay_amount, probs = c(0.005, 0.995))

