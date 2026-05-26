library(ggplot2)
library(dplyr)
library(patchwork)

setwd("~/jupyter/ecworld/pipeline_outputs/")

# A. 哨兵国家滞后曲线对比 (证明滞后时间的一致性)
df_lags <- read.csv("SI_MultiSource_Lags.csv")
sentinels <- c("KAZ", "MNG", "ETH") # 选取核心牧区国家

fig_lags_robust <- df_lags %>%
  filter(ISO_A3 %in% sentinels) %>%
  ggplot(aes(x = Lag, y = Beta, color = Dataset, linetype = Dataset)) +
  geom_line(size = 0.8) +
  facet_wrap(~ISO_A3, scales = "free_y") +
  scale_color_brewer(palette = "Set1") +
  labs(title = "SI | Robustness of Lag-Response across Datasets",
       x = "Lag Year", y = expression(beta)) +
  theme_bw() + theme(legend.position = "bottom")

# B. 物理指标相关性 (验证 BEST vs CPC 的热浪频次趋势)
df_trends <- read.csv("SI_MultiSource_Trends.csv") %>%
  tidyr::pivot_wider(names_from = Dataset, values_from = Trend_Decadal, id_cols = ISO_A3)

fig_trend_corr <- ggplot(df_trends, aes(x = BEST_HWN, y = CPC_HWN)) +
  geom_point(alpha = 0.5, color = "#4575b4") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "SI | Trend Consistency (BEST vs CPC)",
       x = "BEST HWN Trend (Times/Dec)", y = "CPC HWN Trend (Times/Dec)") +
  theme_classic()
print(fig_lags_robust / fig_trend_corr)
# 导出附录图
ggsave("SI_Dataset_Indicator_Comparison.pdf", (fig_lags_robust / fig_trend_corr), 
       width = 180, height = 220, units = "mm", device = cairo_pdf)