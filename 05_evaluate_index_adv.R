#-------------------------------------------------------------------------------
source("01_loading.R")
load("data/final_results_adv.Rdata")
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
da_adv <- do.call(rbind, lapply(final_adv_results, function(x) {
  # M(s) and m(s) are produced by two independent greedy searches on two
  # different MSTs, so neither dominates the other at every scale: where the
  # greedy maximiser does poorly it can return a value below the one the greedy
  # minimiser found.  Both are attainable values of the statistic at that
  # scale, so the tightest available envelope is their pointwise maximum and
  # minimum.
  hi <- pmax(x$M, x$m)
  lo <- pmin(x$M, x$m)

  # Equation (2): I_z = mean_s (M(s) - m(s)) / h.  The correlation is bounded,
  # so h = 2; a two-zone partition attains +-1, which makes this exactly
  # max_s M(s) - min_s m(s).  w(s) = 1 throughout.
  i1 <- mean(hi - lo) / 2
  df <- data.frame(
    fips = x$fips[1],
    county = x$County[1],
    state = x$State[1],
    colname = rename_var(paste0(x$Var1[1], ".", x$Var2[1])),
    I1_ADV = i1
  )
}))

head(da_adv)

da <- readRDS("data/index_random_quantile_90.rds")

# Join only the index columns: `da` also carries county/state, which would
# otherwise arrive as county.x / county.y.
da_adv <- left_join(da_adv, da[, c("fips", "colname", "i1", "i2")],
                    by = c("fips", "colname"))

# Report how often the two greedy searches cross.  This is a quality measure
# for the heuristic, not an error: a high rate means the greedy bounds are
# loose for those scenarios.
cross <- vapply(final_adv_results, function(x) mean(x$M < x$m), numeric(1))
message(sprintf("greedy bounds cross in %d of %d scenarios (%.1f%%)",
                sum(cross > 0), length(cross), 100 * mean(cross > 0)))
if (any(cross > 0)) {
  message(sprintf("  affected scales within those scenarios: median %.2f%%, max %.2f%%",
                  100 * median(cross[cross > 0]), 100 * max(cross)))
}

saveRDS(da_adv, "data/index_adv.rds")
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Same pointwise envelope as above, so the curves plotted in the figures match
# the curves the index is computed from.
da_adv_with_m_M <- do.call(rbind, lapply(final_adv_results, function(x) {
  df <- data.frame(
    fips = x$fips[1],
    county = x$County[1],
    state = x$State[1],
    colname = rename_var(paste0(x$Var1[1], ".", x$Var2[1])),
    m = pmin(x$M, x$m),
    M = pmax(x$M, x$m)
  )
}))

da <- readRDS("data/index_random_quantile_90.rds")

da_adv_with_m_M <- left_join(da_adv_with_m_M, da[, c("fips", "colname", "i1", "i2")],
                             by = c("fips", "colname"))

# Carry I_z across as well.  Without it this table held only the random-scenario
# indices, and figure_typical.R annotated `i1` (which is I_m) under the label
# I_z -- the values printed on Figure 8 were I_m, not I_z.
da_adv_with_m_M <- left_join(da_adv_with_m_M,
                             da_adv[, c("fips", "colname", "I1_ADV")],
                             by = c("fips", "colname"))

saveRDS(da_adv_with_m_M, "data/index_adv_with_m_M.rds")
#-------------------------------------------------------------------------------