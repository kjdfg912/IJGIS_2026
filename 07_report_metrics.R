# ---------------------------------------------------------------------------
# Every quantity reported in Section 6 of the paper, recomputed from the files
# the pipeline produces.  Run after `make all`.  The output is printed and also
# written to section6_metrics.txt.
#
# Each block is headed by the sentence or figure in the paper that it supports,
# so the numbers can be checked one by one against the manuscript.
# ---------------------------------------------------------------------------
source("01_loading.R")
library(data.table)

sink_file <- "section6_metrics.txt"
con <- file(sink_file, open = "wt")
sink(con, split = TRUE)
on.exit({ sink(); close(con) }, add = TRUE)

hdr <- function(...) cat("\n", strrep("-", 74), "\n", sprintf(...), "\n", sep = "")

load("data/final_results_random.Rdata")
index_random <- as.data.table(readRDS("data/index_random_quantile_90.rds"))
index_adv    <- as.data.table(readRDS("data/index_adv.rds"))
bounds       <- as.data.table(readRDS("data/index_adv_with_m_M.rds"))

# Number of scale levels actually fitted for each county.
scales_per_county <- final_results[, .(n_scales = .N / 100L), by = fips]

# ---------------------------------------------------------------------------
hdr("Section 6, data description: states, counties and tracts")

analysed <- unique(as.character(index_random$fips))
tracts <- as.data.table(table(data_corr$fips))
setnames(tracts, c("fips", "n_tracts"))
tracts <- tracts[fips %in% analysed]

cat(sprintf("states                       : %d\n", uniqueN(data_corr$State)))
cat(sprintf("counties with >= 20 tracts   : %d\n",
            sum(table(data_corr$fips) >= 20)))
cat(sprintf("counties in the final sample : %d\n", length(analysed)))
cat(sprintf("tracts per county            : min %d, max %d (%s)\n",
            min(tracts$n_tracts), max(tracts$n_tracts),
            county_lookup$County[county_lookup$fips ==
                                   tracts$fips[which.max(tracts$n_tracts)]]))
cat(sprintf("tracts per county, quartiles : Q1 %.1f, median %.1f, Q3 %.1f\n",
            quantile(tracts$n_tracts, .25), median(tracts$n_tracts),
            quantile(tracts$n_tracts, .75)))
cat("\ncounties per state:\n")
st <- unique(index_random[, .(fips, state)])
print(sort(table(st$state)))

# ---------------------------------------------------------------------------
hdr("Figure 5 and Section 6: I_m by statistic")

cat("median I_m over the counties, one row per statistic:\n")
print(index_random[, .(median_Im = round(median(i1), 3),
                       Q1 = round(quantile(i1, .25), 3),
                       Q3 = round(quantile(i1, .75), 3)),
                   by = colname][order(median_Im)])

hdr("Figure 5: the Gini index against the other statistics")
g  <- index_random[colname == "gini", median(i1)]
o  <- index_random[colname != "gini", median(i1)]
cat(sprintf("median I_m, Gini index        : %.3f\n", g))
cat(sprintf("median I_m, all other statistics: %.3f\n", o))

hdr("Figure 5: variation across states for one statistic")
cat("median I_m for the correlation between the Black population and rooms:\n")
print(index_random[colname == "cor.race.rooms",
                   .(median_Im = round(median(i1), 3)), by = state][order(median_Im)])

# ---------------------------------------------------------------------------
hdr("Figure 6: I_m against the value of the statistic at the finest scale")

finest <- final_results[, lapply(.SD, function(v) v[length(v)]),
                        by = fips, .SDcols = setdiff(names(final_results),
                                                     c("fips", "county", "state"))]
finest <- melt(finest, id.vars = "fips", variable.name = "colname",
               value.name = "finest")
f6 <- merge(index_random, finest, by = c("fips", "colname"))

for (s in c("gini", "r2")) {
  x <- f6[colname == s]
  cat(sprintf("%-4s : cor(I_m, statistic at finest scale) = %+.3f  (n = %d)\n",
              s, cor(x$i1, x$finest, use = "complete.obs"), nrow(x)))
}
x <- f6[startsWith(as.character(colname), "cor")]
cat(sprintf("cor  : cor(I_m, |correlation at finest scale|) = %+.3f  (n = %d)\n",
            cor(x$i1, abs(x$finest), use = "complete.obs"), nrow(x)))

# ---------------------------------------------------------------------------
hdr("Figure 7: I_m against population density")

area_per_county <- do.call(rbind, lapply(split(data_corr, data_corr$fips), get_area))
db <- merge(index_random, as.data.table(area_per_county), by = "fips")
db[, pop_density := pop / area_km]
db <- merge(db, scales_per_county, by = "fips")

cat("Pearson correlation of I_m with each size measure, on the log scale:\n")
print(db[, .(log_density = round(cor(i1, log(pop_density)), 3),
             log_population = round(cor(i1, log(pop)), 3),
             log_n_tracts = round(cor(i1, log(n_scales)), 3),
             log_area = round(cor(i1, log(area_km)), 3)),
         by = colname])
cat(sprintf("\npooled over all statistics: log density %+.3f, log population %+.3f,\n",
            cor(db$i1, log(db$pop_density)), cor(db$i1, log(db$pop))))
cat(sprintf("                           log tracts  %+.3f, log area       %+.3f\n",
            cor(db$i1, log(db$n_scales)), cor(db$i1, log(db$area_km))))

# ---------------------------------------------------------------------------
hdr("Figures 8 and 9, and Section 6: the adversarial index I_z")

cat(sprintf("scenarios (counties x variable pairs): %d\n", nrow(index_adv)))
cat(sprintf("counties                             : %d\n", uniqueN(index_adv$fips)))
cat(sprintf("variable pairs                       : %d\n", uniqueN(index_adv$colname)))
cat(sprintf("median I_z                           : %.3f\n", median(index_adv$I1_ADV)))
cat(sprintf("median I_m over the same scenarios   : %.3f\n", median(index_adv$i1)))
cat(sprintf("I_z > I_m in                         : %d of %d (%.1f%%)\n",
            sum(index_adv$I1_ADV > index_adv$i1), nrow(index_adv),
            100 * mean(index_adv$I1_ADV > index_adv$i1)))
cat(sprintf("cor(I_z, I_m)                        : %.3f\n",
            cor(index_adv$I1_ADV, index_adv$i1)))

cat("\nby variable pair:\n")
print(index_adv[, .(median_Iz = round(median(I1_ADV), 3),
                    median_Im = round(median(i1), 3),
                    cor = round(cor(I1_ADV, i1), 3)), by = colname])

# ---------------------------------------------------------------------------
hdr("Values annotated inside the figure panels")

# Look up one scenario.  The logical vector is built outside the `[` so that
# the argument names are not shadowed by the columns of the data.table.
panel <- function(.fips, .colname, label, index) {
  keep <- index$fips == .fips & index$colname == .colname
  if (!any(keep)) { cat(sprintf("  %-46s not fitted\n", label)); return(NULL) }
  as.data.frame(index)[which(keep)[1], ]
}
cat("Figure 4, the I_m printed in each panel:\n")
for (p in list(c("17043", "cor.race.time_travel", "upper left  DuPage IL, rho(BP,T)"),
               c("06017", "gini",                 "upper right El Dorado CA, Gini"),
               c("48453", "var.earning",          "lower left  Travis TX, variance"),
               c("06037", "r2",                   "lower right Los Angeles CA, R^2"))) {
  v <- panel(p[1], p[2], p[3], index_random)
  if (!is.null(v)) cat(sprintf("  %-46s I_m = %.2f\n", p[3], v$i1))
}

cat("\nFigure 8, the I_z printed in each panel:\n")
for (p in list(c("48201", "cor.earning.race", "upper left  Harris TX, rho(E,BP)"),
               c("39049", "cor.race.rooms",   "upper right Franklin OH, rho(BP,RO)"),
               c("06077", "cor.earning.race", "lower left  San Joaquin CA, rho(E,BP)"),
               c("26161", "cor.earning.time_travel", "lower right Washtenaw MI, rho(E,T)"))) {
  v <- panel(p[1], p[2], p[3], index_adv)
  if (!is.null(v)) cat(sprintf("  %-46s I_z = %.2f\n", p[3], v$I1_ADV))
}

# ---------------------------------------------------------------------------
hdr("Figure 2 and Section 5: the zoning range M(s) - m(s) at selected scales")

for (p in list(c("26125", "cor.earning.time_travel", "Oakland MI, income x commute time"),
               c("06065", "cor.earning.rooms",       "Riverside CA, income x rooms"))) {
  b <- bounds[fips == p[1] & colname == p[2]]
  if (!nrow(b)) next
  hi <- pmax(b$M, b$m); lo <- pmin(b$M, b$m)
  cat(sprintf("\n%s  (%d scale levels)\n", p[3], nrow(b)))
  for (s in c(25L, 50L, 100L, 200L)) {
    if (s <= nrow(b))
      cat(sprintf("  s = %-4d m(s) = %+.3f  M(s) = %+.3f  range = %.3f\n",
                  s, lo[s], hi[s], hi[s] - lo[s]))
  }
  cat(sprintf("  I_z = %.3f\n", mean(hi - lo) / 2))
}

hdr("Denominator h of equations (2) and (3)")
cat("h is the range of T where the statistic has one (2 for a correlation,\n")
cat("1 for the Gini index and R^2) and the observed range otherwise.\n")
cat("Attained range max_s M(s) - min_s m(s), estimated from the simulated\n")
cat("trajectories, for comparison:\n")
for (v in c("gini", "r2", "cor.earning.time_travel")) {
  r <- final_results[, {
    n <- .N / 100L
    d <- data.table(x = rep(1:n, 100L), V1 = get(v))[!is.na(V1)]
    .(rng = max(d[, max(V1), by = x]$V1) - min(d[, min(V1), by = x]$V1))
  }, by = fips]
  cat(sprintf("  %-24s median %.3f   [%.3f, %.3f]\n",
              v, median(r$rng), min(r$rng), max(r$rng)))
}

cat("\n")
