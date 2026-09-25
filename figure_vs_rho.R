##------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(reshape2)
library(latex2exp)

source("00_functions.R")

# ggsave() will not create this itself.
dir.create("figures", showWarnings = FALSE)
load("data/final_results_random.Rdata")
index_random <- readRDS("data/index_random_quantile_90.rds")
original_scale <- final_results[, .N/100, by = fips]
#------------------------------------------------------------------------------

# Value of each statistic at the finest scale: the last row of each county
# block, where every zone is a single tract.  Keyed on FIPS.
ID_COLS <- c("fips", "county", "state")
cols <- setdiff(colnames(final_results), ID_COLS)

tau <- final_results[, .SD[.N], by = fips]

tau_melt <- melt(tau, id.vars = ID_COLS, measure.vars = cols,
                 variable.name = "colname", value.name = "tau")
tau_melt$colname <- as.character(tau_melt$colname)

index_random_with_tau <- left_join(tau_melt, index_random, by = c("fips", "colname"))

head(index_random_with_tau)

a <- subset(
  index_random_with_tau, 
  !(
    startsWith(colname, "var") |
      startsWith(colname, "mean") |
      startsWith(colname, "beta")
  )
)

a[startsWith(a$colname, "cor"), ]$colname <- "cor"

label_map <- c(
  "cor" = "Correlation",
  "gini" = "Gini*' '*index",
  "r2" = "R^2"
)

a <- a %>%
  mutate(colname = recode(colname, !!!label_map))

p_vs_rho <- ggplot(a, aes(x = tau, y = i1)) +
  geom_point(size = 0.4, alpha = 0.45, colour = PAL[["traj"]], stroke = 0) +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x,
              linewidth = 0.8, colour = PAL[["band"]]) +
  facet_wrap(~colname, labeller = label_parsed, scales = "free") +
  ylab(TeX("$I_m$")) +
  xlab("Statistic value at the finest scale") +
  theme_paper()
save_fig(p_vs_rho, "fig06_index_vs_statistic", width = FIG_FULL, ratio = 0.36, subdir = "paper")

##------------------------------------------------------------------------------
