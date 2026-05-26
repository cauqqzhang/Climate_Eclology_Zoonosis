# ==============================================================================
# Script 3: Taylor Diagrams by Income Group (Robustness Check)
# Output: SI_Taylor_By_Income_R.pdf
# ==============================================================================

# 1. 加载库
library(plotrix)
library(tidyr)
library(dplyr)

# 设置工作目录
setwd("~/jupyter/ecworld/pipeline_outputs/")

# 2. 加载并清洗数据
data <- read.csv("SI_Robustness_Trends_with_Income.csv")

df_wide <- data %>%
  filter(!is.na(income_id)) %>%
  # 转换为宽表，以便提取各 Dataset 的列
  pivot_wider(names_from = Dataset, values_from = Trend_Decadal) %>%
  # 剔除含有缺失值的行（泰勒图对比需要相同的数据对）
  drop_na(CHIRTS, BEST, CPC)

# 定义映射
income_ids <- c(1, 2, 3, 4)
income_labels <- c("High Income", "Upper Middle Income", "Lower Middle Income", "Low Income")

# 3. 绘图预览 (直接在 RStudio Plots 窗口输出)
# 设置 2x2 布局和边距
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))

for (i in 1:4) {
  # 筛选组别数据
  group_data <- df_wide %>% filter(income_id == income_ids[i])
  
  # 健壮性检查：如果该组数据点太少则跳过
  if (nrow(group_data) < 2) {
    plot.new()
    text(0.5, 0.5, paste(income_labels[i], "\nInsufficient Data"), cex = 1.2)
    next
  }
  
  ref <- group_data$CHIRTS
  sim1 <- group_data$BEST
  sim2 <- group_data$CPC
  
  # 绘制泰勒图 (参考点)
  taylor.diagram(ref, ref, 
                 col = "#d73027", 
                 pch = 19, 
                 main = paste0(income_labels[i], " (n=", nrow(group_data), ")"),
                 show.gamma = TRUE, 
                 col.gamma = "#31a354", 
                 sd.arcs = TRUE)
  
  # 叠加对比数据集
  taylor.diagram(ref, sim1, add = TRUE, col = "#4575b4", pch = 15)
  taylor.diagram(ref, sim2, add = TRUE, col = "#74add1", pch = 17)
  
  # 在左上角第一张图添加图例
  if (i == 1) {
    legend("topright", 
           legend = c("CHIRTS (Ref)", "BEST", "CPC"), 
           col = c("#d73027", "#4575b4", "#74add1"), 
           pch = c(19, 15, 17), 
           cex = 0.7, 
           bty = "n")
  }
}

# 添加总标题
mtext("Model Robustness Across Income Groups", outer = TRUE, side = 3, cex = 1.2, font = 2)

# 4. 保存为 PDF (替代 ggsave)
# dev.copy 会将当前 Plots 窗口的内容“复印”到指定的 PDF 设备
dev.copy(pdf, "SI_Taylor_By_Income_R.pdf", width = 10, height = 10)
dev.off() # 关闭复印设备

# 提示：运行结束后，请在 Plots 窗口确认效果。
# 如果图像显示不全，请拉大 RStudio 右下角的绘图面板并重新运行代码。
