library(tidyverse)
library(gridExtra) # for tableGrob
library(patchwork) # for combining plots and tables

## -----------------------------------------------------------------
## 1.  Collect and read all result files
## -----------------------------------------------------------------
# folder that contains the experiment sub-directories
base_dir <- "results" # adapt if necessary

files <- list.files(base_dir,
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

  mutate(dat,
    task     = task,
    training = training,
    features = features
  )
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
auc_plot <- results %>%
  mutate(group = interaction(training, features,
    sep = " / ",
    lex.order = TRUE
  )) %>% # 4 groups
  ggplot(aes(group, auc, colour = training, shape = features)) +
  geom_jitter(width = .15, height = 0, size = 2, alpha = .7) + # dots
  stat_summary(
    fun = median, geom = "crossbar", # short line
    width = .4, colour = "black", fatten = 2
  ) +
  facet_wrap(~task, nrow = 1) + # one facet per task
  scale_x_discrete(labels = c(
    "Federated / Regular",
    "Federated / Phenotypes",
    "Global / Regular",
    "Global / Phenotypes"
  )) +
  labs(
    x = "Model type / feature set",
    y = "AUC (outer folds)",
    colour = "Model type",
    shape = "Feature set",
    title = "Nested-CV AUCs"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

runtime_df <- tribble(
  ~task, ~training, ~features, ~runtime_chr,
  "dementia", "Federated", "Regular", "36 h",
  "dementia", "Federated", "Phenotypes", "4.4 h",
  "dementia", "Global", "Regular", "26.5 min",
  "dementia", "Global", "Phenotypes", "22.7 min",
  "lungCancer", "Federated", "Regular", "15.9 h",
  "lungCancer", "Federated", "Phenotypes", "5.3 h",
  "lungCancer", "Global", "Regular", "26.5 min",
  "lungCancer", "Global", "Phenotypes", "21.5 min",
  "readmission", "Federated", "Regular", "2.6 h",
  "readmission", "Federated", "Phenotypes", "18.8 min",
  "readmission", "Global", "Regular", "4.0 min",
  "readmission", "Global", "Phenotypes", "2.9 min"
) %>%
  mutate(
    group = interaction(training, features,
      sep = " / ",
      lex.order = TRUE
    ),
    minutes = if_else(str_detect(runtime_chr, "h"),
      as.numeric(str_remove(runtime_chr, "\\s*h")) * 60,
      as.numeric(str_remove(runtime_chr, "\\s*min"))
    )
  )

runtime_tab <- runtime_df %>%
  select(task, group, runtime_chr) %>%
  pivot_wider(names_from = group, values_from = runtime_chr)

tbl <- tableGrob(runtime_tab,
  rows  = NULL,
  theme = ttheme_minimal(base_size = 9)
)

p_auc <- auc_plot +
  theme(plot.title = element_text(hjust = 0, size = 12)) +
  ggtitle("a) Nested-CV AUCs per task and model type")

tbl_title <- textGrob("b) Runtime per task and model type",
  gp = gpar(fontsize = 12),
  just = "right"
)
pad <- unit(0.4, "lines")

tbl <- gtable::gtable_add_rows(tbl, heights = grobHeight(tbl_title) + pad, pos = 0)
tbl <- gtable::gtable_add_grob(tbl, tbl_title, t = 1, l = 1, r = ncol(tbl))

combined <- grid.arrange(
  p_auc,
  tbl,
  ncol = 1,
  heights = c(3, 1)
) # adjust the height ratio

ggsave("Figure2.svg", width = 8)
