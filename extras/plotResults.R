library(tidyverse)
library(gridExtra) # for tableGrob
library(patchwork) # for combining plots and tables

## -----------------------------------------------------------------
## 1.  Collect and read all result files
## -----------------------------------------------------------------
# folder that contains the experiment sub-directories
base_dir <- "results" # adapt if necessary

files <- list.files(
  base_dir,
  pattern = "nested_cv_results_full.csv",
  recursive = TRUE,
  full.names = TRUE
)

results <- map_dfr(files, function(f) {
  dat <- read_csv(f, show_col_types = FALSE) # has at least column `auc`
  exp <- basename(dirname(f)) # dementiaPhenotypes, etc.

  # Extract the meta information from the directory name
  task <- str_extract(exp, "^(dementia|lungCancer|readmission)")
  training <- if_else(str_detect(exp, "Cyclops"), "Global", "Federated")
  features <- if_else(str_detect(exp, "Phenotypes"), "Phenotypes", "Regular")

  mutate(dat, task = task, training = training, features = features)
})


plot_df <- results %>%
  transmute(
    auc,
    task,
    training,
    features,
    group = interaction(training, features, sep = " / ", lex.order = TRUE)
  )

median_df <- plot_df %>%
  group_by(task, training, features, group) %>%
  summarise(median_auc = median(auc, na.rm = TRUE), .groups = "drop")

## -----------------------------------------------------------------
## 2.  Plot: dots = outer–fold AUCs, thick line = median AUC
## -----------------------------------------------------------------
# Order of x groups (unchanged)
group_levels <- c(
  "Federated / Regular",
  "Federated / Phenotypes",
  "Global / Regular",
  "Global / Phenotypes"
)

# Short labels for x-axis
short_labels <- c(
  "Federated / Regular" = "Fed / Reg",
  "Federated / Phenotypes" = "Fed / Pheno",
  "Global / Regular" = "Glob / Reg",
  "Global / Phenotypes" = "Glob / Pheno"
)

# Poster palette
poster_cols <- c(
  "Federated" = "#0072B2",
  "Global" = "#D55E00"
)

feature_shapes <- c(
  "Regular" = 16,
  "Phenotypes" = 17
)

auc_plot_modern <- results %>%
  mutate(
    group = interaction(training, features, sep = " / ", lex.order = TRUE),
    group = factor(as.character(group), levels = group_levels)
  ) %>%
  ggplot(aes(x = group, y = auc, colour = training, shape = features)) +

  geom_jitter(width = .15, height = 0, size = 3.5, alpha = .8) +

  stat_summary(
    fun = median,
    geom = "crossbar",
    width = .4,
    colour = "black",
    linewidth = 1.2,
    fatten = 2
  ) +

  facet_wrap(~task, nrow = 1) +

  scale_x_discrete(labels = short_labels, drop = FALSE) +
  scale_colour_manual(name = "Model type", values = poster_cols) +
  scale_shape_manual(name = "Feature set", values = feature_shapes) +

  labs(
    x = "Model type / feature set",
    y = "AUC (outer folds)",
    title = "Nested-CV AUCs"
  ) +

  theme_bw(base_size = 18) +
  coord_cartesian(clip = "off") +
  theme(
    plot.margin = unit(c(5.5, 10, 5.5, 5.5), "pt"),

    axis.text.x = element_text(size = 18, angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 24, margin = margin(t = 6)),
    axis.title.y = element_text(size = 24, margin = margin(r = 6)),

    strip.background = element_blank(),
    strip.text = element_text(size = 20, face = "bold"),

    plot.title = element_text(size = 38, face = "bold", hjust = 0.5),

    # Legend to the right (vertical stack)
    legend.position = "right",
    legend.box = "vertical",
    legend.key.width = unit(0.8, "cm"),
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 18),
    legend.spacing.x = unit(6, "pt"),
    legend.spacing.y = unit(2, "pt")
  ) +
  guides(
    colour = guide_legend(override.aes = list(size = 4, alpha = 1)),
    shape = guide_legend(override.aes = list(size = 4, alpha = 1))
  )

auc_plot_modern

ggplot2::ggsave(
  filename = "results/figures/discrimination_auc_plot.svg",
  plot = auc_plot_modern,
  width = 340,
  height = 220,
  units = "mm",
  dpi = 300
)

runtime_df <- tribble(
  ~task,
  ~training,
  ~features,
  ~runtime_chr,
  "dementia",
  "Federated",
  "Regular",
  "36 h",
  "dementia",
  "Federated",
  "Phenotypes",
  "4.4 h",
  "dementia",
  "Global",
  "Regular",
  "26.5 min",
  "dementia",
  "Global",
  "Phenotypes",
  "22.7 min",
  "lungCancer",
  "Federated",
  "Regular",
  "15.9 h",
  "lungCancer",
  "Federated",
  "Phenotypes",
  "5.3 h",
  "lungCancer",
  "Global",
  "Regular",
  "26.5 min",
  "lungCancer",
  "Global",
  "Phenotypes",
  "21.5 min",
  "readmission",
  "Federated",
  "Regular",
  "2.6 h",
  "readmission",
  "Federated",
  "Phenotypes",
  "18.8 min",
  "readmission",
  "Global",
  "Regular",
  "4.0 min",
  "readmission",
  "Global",
  "Phenotypes",
  "2.9 min"
) %>%
  mutate(
    group = interaction(training, features, sep = " / ", lex.order = TRUE),
    minutes = if_else(
      str_detect(runtime_chr, "h"),
      as.numeric(str_remove(runtime_chr, "\\s*h")) * 60,
      as.numeric(str_remove(runtime_chr, "\\s*min"))
    )
  )

runtime_tab <- runtime_df %>%
  select(task, group, runtime_chr) %>%
  pivot_wider(names_from = group, values_from = runtime_chr)

tbl <- tableGrob(
  runtime_tab,
  rows = NULL,
  theme = ttheme_minimal(base_size = 9)
)

p_auc <- auc_plot +
  theme(plot.title = element_text(hjust = 0, size = 12)) +
  ggtitle("a) Nested-CV AUCs per task and model type")

tbl_title <- textGrob(
  "b) Runtime per task and model type",
  gp = gpar(fontsize = 12),
  just = "right"
)
pad <- unit(0.4, "lines")

tbl <- gtable::gtable_add_rows(
  tbl,
  heights = grobHeight(tbl_title) + pad,
  pos = 0
)
tbl <- gtable::gtable_add_grob(tbl, tbl_title, t = 1, l = 1, r = ncol(tbl))

combined <- grid.arrange(
  p_auc,
  tbl,
  ncol = 1,
  heights = c(3, 1)
) # adjust the height ratio

ggsave("Figure2.svg", width = 8)
library(tidyverse)
library(gridExtra) # for tableGrob



# Poster palette and shapes (same mapping as your AUC plot)
poster_cols    <- c("Federated" = "#0072B2", "Global" = "#D55E00")
feature_shapes <- c("Regular" = 16, "Phenotypes" = 17)

# Pretty labels for minutes on a log scale
minutes_lab <- function(x) {
  h <- floor(x / 60)
  m <- round(x %% 60)
  ifelse(x < 60,
         paste0(round(x), "m"),
         ifelse(m == 0, paste0(h, "h"), paste0(h, "h ", m, "m")))
}

# Pairs (Global vs Federated) for segments and ratios
runtime_pairs <- runtime_df %>%
  select(task, features, training, minutes) %>%
  pivot_wider(names_from = training, values_from = minutes) %>%
  mutate(
    ratio       = Federated / Global,
    ratio_label = paste0("×", round(ratio, 1))
  )

# Long data for points
runtime_long <- runtime_df %>%
  select(task, features, training, minutes)

# Choose breaks for the log-minute axis (tweak to taste)
breaks_min <- c(3, 10, 30, 60, 120, 240, 480, 1440, 2160)

runtime_plot_v <- ggplot() +
  # Dumbbell segment: Global -> Federated
  geom_segment(
    data = runtime_pairs,
    aes(x = Global, xend = Federated, y = features, yend = features),
    linewidth = 1.6, colour = "#9ca3af", alpha = 0.9
  ) +
  # Endpoints
  geom_point(
    data = runtime_long,
    aes(x = minutes, y = features, colour = training, shape = features),
    size = 4.2, alpha = 0.95
  ) +
  # Bigger ratio label near Federated point
  geom_text(
    data = runtime_pairs,
    aes(x = Federated, y = features, label = ratio_label),
    hjust = -0.15, vjust = 0.5,
    size = 6.2, fontface = "bold", colour = "#111827"
  ) +
  facet_wrap(~ task, ncol = 1) +  # stack vertically to use height
  scale_x_continuous(
    trans = "log10",
    breaks = breaks_min,
    labels = minutes_lab,
    expand = expansion(mult = c(0.02, 0.14))  # extra right space for larger labels
  ) +
  scale_colour_manual(name = "Model type", values = poster_cols) +
  scale_shape_manual(name = "Feature set", values = feature_shapes) +
  labs(
    x = "Runtime (minutes, log scale)",
    y = "Feature set",
    title = "Training runtime by model type and feature set"
  ) +
  theme_bw(base_size = 18) +
  coord_cartesian(clip = "off") +
  theme(
    plot.margin   = unit(c(5.5, 10, 5.5, 5.5), "pt"),

    # 45° ticks so they don't overlap; make ticks larger
    axis.text.x   = element_text(size = 20, angle = 45, hjust = 1, vjust = 1),
    axis.text.y   = element_text(size = 20),
    axis.title.x  = element_text(size = 24, margin = margin(t = 6)),
    axis.title.y  = element_text(size = 24, margin = margin(r = 6)),

    strip.background = element_blank(),
    strip.text       = element_text(size = 22, face = "bold"),
    panel.spacing.y  = unit(16, "pt"),

    plot.title   = element_text(size = 38, face = "bold", hjust = 0.5),

    legend.position   = "right",
    legend.box        = "vertical",
    legend.key.width  = unit(0.8, "cm"),
    legend.title      = element_text(size = 20, face = "bold"),
    legend.text       = element_text(size = 18),
    legend.spacing.x  = unit(6, "pt"),
    legend.spacing.y  = unit(2, "pt")
  ) +
  guides(
    colour = guide_legend(override.aes = list(size = 5, alpha = 1)),
    shape  = guide_legend(override.aes = list(size = 5, alpha = 1))
  )

runtime_plot_v

ggplot2::ggsave(
  filename = "results/figures/runtime_plot.svg",
  plot = runtime_plot_v,
  width = 340,
  height = 220,
  units = "mm",
  dpi = 300
)
