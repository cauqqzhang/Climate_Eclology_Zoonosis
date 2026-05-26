setwd("~/jupyter/ecworld/pipeline_outputs/")
# ==============================================================================
# Script 1: Main Figures for Echinococcosis Cascading Burden Study
# Output: Figure_Main_Consolidated.pdf (A4 Panel)
# ==============================================================================
library(ggplot2)
library(dplyr)
library(patchwork)
library(sf)
library(rnaturalearth)

# 1. 环境与投影配置

MOLL_CRS <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
income_pal <- c("HI"="#4575b4", "UMI"="#abd9e9", "LMI"="#f46d43", "LI"="#d73027")

# 加载底图并转换投影
world_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(continent != "Antarctica") %>%
  st_transform(crs = MOLL_CRS)

# 加载数据
df_attr <- read.csv("R_input_attribution.csv")
df_lags <- read.csv("R_input_lags.csv")
df_coef <- read.csv("R_input_coefficients.csv")

# ------------------------------------------------------------------------------
# Panel A: 全球空间异质性地图 (对数增强对比度)
# ------------------------------------------------------------------------------
fig_a <- world_sf %>%
  left_join(df_attr, by = c("iso_a3" = "ISO_A3")) %>%
  mutate(log_beta = log10(abs(Beta_NDVI_Sum) + 1e-6)) %>%
  ggplot() +
  geom_sf(aes(fill = log_beta), color = "white", size = 0.05) +
  scale_fill_gradientn(
    colors = c("#fee090", "#fdae61", "#f46d43", "#d73027", "#a50026"), 
    na.value = "grey95", name = expression(log[10](abs(beta)))
  ) +
  labs(title = "Global Heterogeneity of Ecological Sensitivity") +
  theme_void() + theme(legend.position = "bottom", plot.title = element_text(face="bold"))

# ------------------------------------------------------------------------------
# Panel B: 滞后响应曲线 (按收入组聚合)
# ------------------------------------------------------------------------------
df_lags_sum <- df_lags %>%
  filter(Income_Group %in% c("HI", "UMI", "LMI", "LI")) %>%
  mutate(Income_Group = factor(Income_Group, levels = c("HI", "UMI", "LMI", "LI"))) %>%
  group_by(Income_Group, Lag) %>%
  summarise(M = mean(Beta), S = sd(Beta)/sqrt(n()), .groups = "drop")

fig_b <- ggplot(df_lags_sum, aes(x = Lag, y = M, color = Income_Group, fill = Income_Group)) +
  geom_line(size = 0.8) + geom_ribbon(aes(ymin = M-S, ymax = M+S), alpha = 0.1, color = NA) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.3) +
  scale_color_manual(values = income_pal) + scale_fill_manual(values = income_pal) +
  scale_x_continuous(breaks = 0:10) +
  labs(title = "Temporal Lag-Response", x = "Lag Year", y = expression(beta)) +
  theme_classic() + theme(legend.position = "none")

# ------------------------------------------------------------------------------
# Panel C: 归因森林图 & Panel D: 收入组箱线图
# ------------------------------------------------------------------------------
df_coef$Type <- ifelse(df_coef$Coef > 0, "Amplifier", "Buffer")
fig_c <- ggplot(df_coef, aes(x = Coef, y = reorder(Factor, Coef), color = Type)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = Coef-1.96*SE, xmax = Coef+1.96*SE), height = 0.2) +
  geom_point(size = 2.5) + scale_color_manual(values = c("Amplifier"="#d73027", "Buffer"="#4575b4")) +
  labs(title = "Drivers", x = "Effect Size", y = "") + theme_classic() + theme(legend.position = "none")

fig_d <- ggplot(df_attr, aes(x = factor(Income_Group, levels=c("HI","UMI","LMI","LI")), 
                             y = log10(abs(Beta_NDVI_Sum)+1e-6), fill = Income_Group)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.3) + scale_fill_manual(values = income_pal) +
  labs(title = "Inequality", x = "", y = expression(log[10](abs(beta)))) + theme_minimal() + theme(legend.position = "none")

# 组合导出
final_main <- fig_a / (fig_b | fig_c | fig_d) + plot_layout(heights = c(1.5, 1)) + plot_annotation(tag_levels = 'A')
print(final_main)
ggsave("Figure_Main_Consolidated.pdf", final_main, width = 180, height = 220, units = "mm", device = cairo_pdf)

