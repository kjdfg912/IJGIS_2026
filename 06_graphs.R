#-------------------------------------------------------------------------------
source("01_loading.R")

# ggsave() will not create this itself.
dir.create("figures", showWarnings = FALSE)

da <- readRDS("data/index_random_quantile_90.rds")
da_adv <- readRDS("data/index_adv.rds")

load("data/final_results_random.Rdata")

original_scale <- final_results[, .N/100, by = fips]
#-------------------------------------------------------------------------------

da <- left_join(da, original_scale)
da_adv <- left_join(da_adv, original_scale)
# Refresh the random-scenario indices (I_m, I_s) on da_adv, selecting the
# columns to drop by name so the join is insensitive to column order.
da_adv <- da_adv[, setdiff(names(da_adv), c("i1", "i2"))]
da_adv <- left_join(da_adv, da[, c("fips", "colname", "i1", "i2")],
                    by = c("fips", "colname"))

da$colname <- factor(
  da$colname,
  levels = unique(da$colname)
)

library(ggplot2)

# The 20 statistics fall into families (Gini, R^2, four betas, four means,
# four variances, six correlations).  Light rules at the boundaries let the
# reader find a family without parsing every subscript.
STAT_BREAKS <- c(2.5, 6.5, 10.5, 14.5)

ggplot(da) +
  geom_vline(xintercept = STAT_BREAKS, colour = "grey75", linewidth = 0.4) +
  geom_boxplot(aes(x = colname, y = i1), linewidth = 0.38,
               outlier.size = 0.5, outlier.stroke = 0) +
  facet_wrap(~state, nrow = 5) +
  theme_paper() +
  scale_x_discrete(labels = c(
    "beta.1" = expression(beta[" 0"]),
    "beta.2" = expression(beta[" BP"]),
    "beta.3" = expression(beta[" T"]),
    "beta.4" = expression(beta[" RO"]),
    "cor.earning.race" = expression(rho[" E,BP"]),
    "cor.earning.rooms" = expression(rho[" E,RO"]),
    "cor.earning.time_travel" = expression(rho[" E,T"]),
    "cor.race.rooms" = expression(rho[" BP,RO"]),
    "cor.race.time_travel" = expression(rho[" BP,T"]),
    "cor.time_travel.rooms" = expression(rho[" T,RO"]),
    "gini" = "GI",
    "mean.earning" = expression(mu[" E"]),
    "mean.race" = expression(mu[" BP"]),
    "mean.rooms" = expression(mu[" RO"]),
    "mean.time_travel" = expression(mu[" T"]),
    "r2" = expression(R^2),
    "var.earning" = expression(S["E"]),
    "var.race" = expression(S["BP"]),
    "var.rooms" = expression(S["RO"]),
    "var.time_travel" = expression(S["T"])
  )) +
  xlab("Statistic") +
  ylab(expression(I["m"])) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = rel(0.85)))

save_fig(last_plot(), "fig05_index_by_state", width = FIG_FULL, ratio = 1.15, subdir = "paper")


ggplot(da) +
  geom_boxplot(aes(x = colname, y = i2), linewidth = 0.38,
               outlier.size = 0.5, outlier.stroke = 0) +
  facet_wrap(~state, nrow = 5) +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = rel(0.85))) +
  scale_x_discrete(labels = c(
    "beta.1" = expression(beta[0]),
    "beta.2" = expression(beta[1]),
    "beta.3" = expression(beta[2]),
    "beta.4" = expression(beta[3]),
    "cor.earning.race" = expression(rho["E,BP"]),
    "cor.earning.rooms" = expression(rho["E,RO"]),
    "cor.earning.time_travel" = expression(rho["E,T"]),
    "cor.race.rooms" = expression(rho["BP,RO"]),
    "cor.race.time_travel" = expression(rho["BP,T"]),
    "cor.time_travel.rooms" = expression(rho["T,RO"]),
    "gini" = "GI",
    "mean.earning" = expression(mu["E"]),
    "mean.race" = expression(mu["BP"]),
    "mean.rooms" = expression(mu["RO"]),
    "mean.time_travel" = expression(mu["T"]),
    "r2" = expression(R^2),
    "var.earning" = expression(S["E"]),
    "var.race" = expression(S["BP"]),
    "var.rooms" = expression(S["RO"]),
    "var.time_travel" = expression(S["T"])
  )) +
  xlab("Statistic") +
  ylab(expression(I[2]))

label_map <- c(
  "beta.1" = "beta*' '*phantom()[0]",
  "beta.2" = "beta*' '*phantom()[BP]",
  "beta.3" = "beta*' '*phantom()[T]",
  "beta.4" = "beta*' '*phantom()[RO]",
  "cor.earning.race" = "rho*' '*phantom()['E, BP']",
  "cor.earning.rooms" = "rho*' '*phantom()['E, RO']",
  "cor.earning.time_travel" = "rho*' '*phantom()['E, T']",
  "cor.race.rooms" = "rho*' '*phantom()['BP, RO']",
  "cor.race.time_travel" = "rho*' '*phantom()['BP, T']",
  "cor.time_travel.rooms" = "rho*' '*phantom()['T, RO']",
  "gini" = "GI",
  "mean.earning" = "mu*' '*phantom()[E]",  # Add space after mu
  "mean.race" = "mu*' '*phantom()[BP]",   # Add space after mu
  "mean.rooms" = "mu*' '*phantom()[RO]",  # Add space after mu
  "mean.time_travel" = "mu*' '*phantom()[T]",  # Add space after mu
  "r2" = "R^2",
  "var.earning" = "S[E]",
  "var.race" = "S[BP]",
  "var.rooms" = "S[RO]",
  "var.time_travel" = "S[T]"
)

da <- da %>%
  mutate(colname = recode(colname, !!!label_map))


ggplot(da) +
  geom_point(aes(x = i1, y = i2), size = 0.5, alpha = 0.55,
             colour = PAL[["ref"]], stroke = 0) +
  facet_wrap(~colname, labeller = label_parsed) + # Use label_parsed to interpret expressions
  theme_paper() +
  xlab(expression(I["m"])) +
  ylab(expression(I["s"]))

save_fig(last_plot(), "im_vs_is_scatter", width = FIG_FULL, ratio = 0.78, subdir = "other")

cor(da$i1, da$i2)

#-------------------------------------------------------------------------------
# Figure 9: I_z and I_m side by side, one facet per pair of variables.
#-------------------------------------------------------------------------------
da_adv_melt <- reshape2::melt(da_adv, measure.vars = c("I1_ADV", "i1"))
da_adv_melt$colname <- factor(da_adv_melt$colname)

label_map <- c(
  "cor.earning.race" = "rho*' '*phantom()['E, BP']",
  "cor.earning.rooms" = "rho*' '*phantom()['E, RO']",
  "cor.earning.time_travel" = "rho*' '*phantom()['E, T']",
  "cor.race.rooms" = "rho*' '*phantom()['BP, RO']",
  "cor.race.time_travel" = "rho*' '*phantom()['BP, T']",
  "cor.time_travel.rooms" = "rho*' '*phantom()['T, RO']"
)

da_adv_melt <- da_adv_melt %>%
  mutate(colname = recode(colname, !!!label_map))

ggplot(da_adv_melt) +
  geom_boxplot(aes(x = variable, y = value), linewidth = 0.38,
               outlier.size = 0.5, outlier.stroke = 0) +
  facet_wrap(~colname, labeller = label_parsed) + # Use label_parsed to interpret expressions
  theme_paper() +
  scale_x_discrete(labels = c(
    "i1" = expression(I["m"]),
    "I1_ADV" = expression(I["z"])
  )) +
  xlab("Metric") +
  ylab("Index")
save_fig(last_plot(), "fig09_iz_vs_im", width = FIG_FULL, ratio = 0.45, subdir = "paper")


da_adv <- da_adv %>%
  mutate(colname = recode(colname, !!!label_map))

ggplot(da_adv) +
  geom_point(aes(x = I1_ADV, y = i1), size = 0.5, alpha = 0.55,
             colour = PAL[["ref"]], stroke = 0) +
  facet_wrap(~colname, labeller = label_parsed) +
  theme_paper() +
  xlab(expression(I["z"])) +
  ylab(expression(I["m"]))
save_fig(last_plot(), "iz_vs_im_scatter", width = FIG_FULL, ratio = 0.5, subdir = "other")

# Overall agreement between the two indices, quoted in Section 6.
message(sprintf("cor(I_z, I_m) = %.3f over %d scenarios",
                cor(da_adv$I1_ADV, da_adv$i1), nrow(da_adv)))

# Scenarios in which the adversarial index falls below the moderate one.  These
# are cases where the greedy search fails to find a wide envelope, not a
# property of the indices themselves.
idx <- da_adv$I1_ADV < da_adv$i1
message(sprintf("I_z < I_m in %d of %d scenarios (%.1f%%)",
                sum(idx), length(idx), 100 * mean(idx)))

# Diagnostic (not used in the paper): I_m against the number of areas.
ggplot(da) +
  geom_point(aes(x = log(V1), y = i1), size = 0.5, alpha = 0.55,
             colour = PAL[["ref"]], stroke = 0) +
  facet_wrap(~ colname)



split_data_corr <- split(data_corr, data_corr$fips)
area_per_county <- sapply(split_data_corr, get_area)
area_per_county <- do.call(rbind, area_per_county)
head(area_per_county)

db <- left_join(area_per_county, da)
db$pop_density <- db$pop / db$area_km

ggplot(db) +
  geom_point(aes(x = log(area_km), y = i1)) +
  facet_wrap(~colname)

label_map <- c(
  "beta.1" = "beta*' '*phantom()[0]",
  "beta.2" = "beta*' '*phantom()[BP]",
  "beta.3" = "beta*' '*phantom()[T]",
  "beta.4" = "beta*' '*phantom()[RO]",
  "cor.earning.race" = "rho*' '*phantom()['E, BP']",
  "cor.earning.rooms" = "rho*' '*phantom()['E, RO']",
  "cor.earning.time_travel" = "rho*' '*phantom()['E, T']",
  "cor.race.rooms" = "rho*' '*phantom()['BP, RO']",
  "cor.race.time_travel" = "rho*' '*phantom()['BP, T']",
  "cor.time_travel.rooms" = "rho*' '*phantom()['T, RO']",
  "gini" = "GI",
  "mean.earning" = "mu*' '*phantom()[E]",  # Add space after mu
  "mean.race" = "mu*' '*phantom()[BP]",   # Add space after mu
  "mean.rooms" = "mu*' '*phantom()[RO]",  # Add space after mu
  "mean.time_travel" = "mu*' '*phantom()[T]",  # Add space after mu
  "r2" = "R^2",
  "var.earning" = "S[E]",
  "var.race" = "S[BP]",
  "var.rooms" = "S[RO]",
  "var.time_travel" = "S[T]"
)

db <- db %>%
  mutate(colname = recode(colname, !!!label_map))

ggplot(db) +
  geom_point(aes(x = log(pop_density), y = i1), size = 0.5, alpha = 0.55,
             colour = PAL[["ref"]], stroke = 0) +
  facet_wrap(~colname, labeller = label_parsed) + # Use label_parsed to interpret expressions
  theme_paper() +
  xlab(expression("log population density (people per km"^2*")")) +
  ylab(expression(I["m"])) +
  theme(strip.text.x = element_text(size = rel(0.85)))
save_fig(last_plot(), "fig07_index_vs_density", width = FIG_FULL, ratio = 0.72, subdir = "paper")

ggplot(db) +
  geom_point(aes(x = log(pop_density), y = i2), size = 0.5, alpha = 0.55,
             colour = PAL[["ref"]], stroke = 0) +
  facet_wrap(~colname, labeller = label_parsed) + # Use label_parsed to interpret expressions
  theme_paper() +
  xlab(expression("log population density (people per km"^2*")")) +
  ylab(expression(I[2])) +
  theme(strip.text.x = element_text(size = rel(0.85)))
save_fig(last_plot(), "is_vs_density", width = FIG_FULL, ratio = 0.72, subdir = "other")

ggplot(db) +
  geom_point(aes(x = log(V1), y = i1), size = 0.5, alpha = 0.55,
             colour = PAL[["ref"]], stroke = 0) +
  facet_wrap(~colname, labeller = label_parsed) + # Use label_parsed to interpret expressions
  theme_paper() +
  xlab("log number of census tracts") +
  ylab(expression(I["m"])) +
  theme(strip.text.x = element_text(size = rel(0.85)))
save_fig(last_plot(), "index_vs_tract_count", width = FIG_FULL, ratio = 0.72, subdir = "other")

ggplot(db) +
  geom_point(aes(x = log(pop), y = i1)) +
  facet_wrap(~ colname)

load("data/final_results_random.Rdata")
N <- final_results[, .N, by = fips]$N

original_scale_value <- melt(final_results[cumsum(N), ])
colnames(original_scale_value)[3] <- "colname"

db2 <- left_join(original_scale_value, db)

ggplot(db2) +
  geom_point(aes(x = pop, y = value)) +
  facet_wrap(~ colname, scales = "free")

ggplot(db2) +
  geom_point(aes(x = pop, y = V1))
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
db <- left_join(area_per_county, da_adv)
db$pop_density <- db$pop / db$area_km

ggplot(db) +
  geom_point(aes(x = log(area_km), y = i1)) +
  facet_wrap(~ colname)

ggplot(db) +
  geom_point(aes(x = log(pop_density), y = i1)) +
  facet_wrap(~ colname)

ggplot(db) +
  geom_point(aes(x = log(pop), y = i1)) +
  facet_wrap(~ colname)
#-------------------------------------------------------------------------------