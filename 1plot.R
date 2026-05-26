library(ggplot2); library(dplyr); library(patchwork); library(sf); library(rnaturalearth); library(ggrepel)

setwd("~/jupyter/ecworld/pipeline_outputs/")
# ==============================================================================
# Script 2: Supplementary Information (SI) for Echinococcosis Study
# Output: SI_Figures_Collection.pdf
# ==============================================================================

MOLL_CRS <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
income_pal <- c("HI"="#4575b4", "UMI"="#abd9e9", "LMI"="#f46d43", "LI"="#d73027")

# 加载数据
df_lags <- read.csv("R_input_lags.csv")
df_trends <- read.csv("results_si_trends.csv")
df_attr <- read.csv("R_input_attribution.csv")
world_sf <- ne_countries(scale = "medium", returnclass = "sf") %>% filter(continent != "Antarctica") %>% st_transform(crs = MOLL_CRS)

# ------------------------------------------------------------------------------
# SI 1: 哨兵国家 DLNM 曲线 (对应您上传的 PDF)
# ------------------------------------------------------------------------------
sentinels <- c("KAZ", "MNG", "AFG", "ETH", "CHN", "PER", "KGZ", "TJK", "NPL")
fig_si_lags <- df_lags %>%
  filter(ISO_A3 %in% sentinels) %>%
  mutate(ISO_A3 = factor(ISO_A3, levels = sentinels)) %>%
  ggplot(aes(x = Lag, y = Beta, color = ISO_A3)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_line(size = 1) + geom_point(size = 1.2) +
  facet_wrap(~ISO_A3, scales = "free_y", ncol = 3) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "SI | Country-Specific DLNM Curves", x = "Lag (Years)", y = expression(beta)) +
  theme_bw() + theme(legend.position = "none", panel.grid = element_blank())

# ------------------------------------------------------------------------------
# SI 2: 趋势一致性地图 (投影与主图一致)
# ------------------------------------------------------------------------------
fig_hwt_trend <- world_sf %>% left_join(df_trends, by = c("iso_a3" = "ISO_A3")) %>%
  ggplot() + geom_sf(aes(fill = HWT_Trend), color = "white", size = 0.05) +
  scale_fill_gradient2(low = "#4575b4", mid = "#ffffbf", high = "#d73027", name = "Days/Dec") +
  theme_void() + labs(title = "A HW indicator HWT ")

fig_ndvi_trend <- world_sf %>% left_join(df_trends, by = c("iso_a3" = "ISO_A3")) %>%
  ggplot() + geom_sf(aes(fill = NDVI_Trend), color = "white", size = 0.05) +
  scale_fill_gradient2(low = "#8c510a", mid = "#f5f5f5", high = "#01665e", name = "Index/Dec") +
  theme_void() + labs(title = "B Grassland NDVI")

# ------------------------------------------------------------------------------
# SI 3: 模型拟合度 (R2) 地图
# ------------------------------------------------------------------------------
fig_r2 <- world_sf %>% left_join(df_attr, by = c("iso_a3" = "ISO_A3")) %>%
  ggplot() + geom_sf(aes(fill = R2), color = "white", size = 0.05) +
  scale_fill_viridis_c(option = "magma", name = expression(R^2)) +
  theme_void() + labs(title = "SI | Model R-Squared")
# 在 RStudio 中预览组合地图
figpr=(fig_hwt_trend / fig_ndvi_trend / fig_r2) + plot_annotation(tag_levels = 'A')

# 预览滞后曲线
print(figpr)
# 导出所有 SI PDF
ggsave("SI_Sentinel_Lags.pdf", fig_si_lags, width = 180, height = 180, units = "mm", device = cairo_pdf)
ggsave("SI_Trend_Consistency.pdf", (fig_hwt_trend / fig_ndvi_trend), width = 180, height = 200, units = "mm", device = cairo_pdf)
ggsave("SI_Model_Fitness.pdf", fig_r2, width = 180, height = 100, units = "mm", device = cairo_pdf)