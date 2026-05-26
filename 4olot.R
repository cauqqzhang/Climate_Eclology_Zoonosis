# ==============================================================================
# 完整 R 脚本：生成 SI 附录图表 (S6 & S7)
# 功能：读取滞后数据与国家收入分组，完成数据合并与全景绘图
# ==============================================================================

library(ggplot2)
library(dplyr)

# 1. 设置工作目录与基础配置
setwd("~/jupyter/ecworld/pipeline_outputs/")
income_pal <- c("HI"="#4575b4", "UMI"="#abd9e9", "LMI"="#f46d43", "LI"="#d73027")



# 1. 加载
df_lags_all <- read.csv("R_input_lags.csv")
retained_data <- read.csv("retained_countries_list.csv")

# 确保 retained_data 的列名统一，取 ISO_A3 和 Income_Group
# 假设你的 CSV 读进来是两列，我们重命名确保准确
colnames(retained_data) <- c("ISO_A3", "Income_Group")

# 2. 核心修复：使用 match 建立映射，而不依赖复杂的 join
# 这样即便 join 逻辑有微小偏差，也能强制把 Income_Group 塞进去
df_plot <- df_lags_all %>%
  filter(ISO_A3 %in% retained_data$ISO_A3) %>%
  mutate(Income_Group = retained_data$Income_Group[match(ISO_A3, retained_data$ISO_A3)])

# 3. 检查数据是否真的合并进去了
if(any(is.na(df_plot$Income_Group))) {
  warning("警告：部分国家找不到 Income_Group，请检查匹配！")
}
# 4. 强制转换 Factor
df_plot$ISO_A3 <- factor(df_plot$ISO_A3)
df_plot$Income_Group <- factor(df_plot$Income_Group, levels = c("HI", "UMI", "LMI", "LI"))

df_plot_sorted <- df_plot %>%
  arrange(Income_Group, ISO_A3) %>%
  mutate(facet_label = factor(paste0(Income_Group, ": ", ISO_A3), 
                              levels = unique(paste0(Income_Group, ": ", ISO_A3))))
# 建议：ncol = 6 (每行6个)，对于 37 个国家，总共会生成 7 行
fig_si_6_grouped <- ggplot(df_plot_sorted, aes(x = Lag, y = Beta, color = Income_Group)) +
  # 添加基准线
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60", size = 0.3) +
  # 绘制滞后曲线
  geom_line(size = 0.7) +
  geom_point(size = 0.5, alpha = 0.6) +
  # 分面设置：按排序后的标签分面，ncol=6 保证比例协调
  facet_wrap(~facet_label, ncol = 6, scales = "free_y") +
  # 使用你定义的标准配色
  scale_color_manual(values = c("HI"="#4575b4", "UMI"="#abd9e9", "LMI"="#f46d43", "LI"="#d73027")) +
  labs(
    title = "Supplementary Figure 6 | Global Country-Specific Lag-Response Profiles (Sorted by Income)",
    subtitle = "Faceted by World Bank Income Group (HI to LI) | Total Countries N=37",
    x = "Lag Year (0-10)", 
    y = expression(beta ~ " (Effect Coefficient)")
  ) +
  theme_bw() +
  theme(
    # 缩小分面标签字号，防止重叠
    strip.text = element_text(size = 6.5, face = "bold"), 
    strip.background = element_rect(fill = "grey95"),
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 10),
    legend.position = "none", # 全景图中不需要图例，因为分面标签已标明
    panel.spacing = unit(0.3, "lines"), # 紧凑布局
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

ggsave("SI_Figure_6_Grouped_Panoramic.pdf", fig_si_6_grouped, 
       width = 210, height = 297, units = "mm", device = cairo_pdf)
print(fig_si_6_grouped)
library(tidyr)
library(cluster)
library(dendrogram)

# 1. 准备聚类矩阵：将长格式数据转换为宽格式 (ISO_A3 x Lag)
# 每一行是一个国家，每一列是该国家在不同 Lag 年份的 Beta 值
df_wide <- df_plot %>%
  select(ISO_A3, Lag, Beta) %>%
  pivot_wider(names_from = Lag, values_from = Beta) %>%
  as.data.frame()

# 设置行名为 ISO_A3，并移除该列以便进行数值聚类
rownames(df_wide) <- df_wide$ISO_A3
df_matrix <- df_wide %>% select(-ISO_A3)

# 2. 计算距离矩阵 (使用欧几里得距离)
dist_matrix <- dist(scale(df_matrix)) # scale标准化以消除量纲差异

# 3. 进行层次聚类 (Ward 最小方差法)
hc <- hclust(dist_matrix, method = "ward.D2")

# 4. 可视化聚类树状图 (Dendrogram)
pdf("SI_Figure_7_Clustering_Analysis.pdf", width = 8, height = 10)
plot(hc, main = "SI 7 | Clustering Analysis of Lag-Response Profiles",
     sub = "Classification of countries based on similarity in response patterns",
     xlab = "Countries", ylab = "Distance", cex = 0.7)
rect.hclust(hc, k = 4, border = "red") # 自动画出 4 个聚类分支
dev.off()

