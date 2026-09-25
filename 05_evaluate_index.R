#-------------------------------------------------------------------------------
source("01_loading.R")

load("data/final_results_random.Rdata")

original_scale <- final_results[, .N/100, by = fips]
#-------------------------------------------------------------------------------


n_traj <- 100L

# Smallest scale level retained by the weight function w(s).
S_MIN <- 12L

# Denominator h of equations (2) and (3): the height of the rectangle against
# which the enclosed area is normalised, i.e. the maximum value the range can
# possibly take.  Where T has fixed theoretical bounds this is that range --
# 1 for the Gini index and R^2, 2 for a correlation -- and for a correlation it
# is exactly max_s M(s) - min_s m(s), since a two-zone partition gives +-1.
# Statistics without theoretical bounds fall back to the empirical range.

I2 <- function(
  total_areas,
  county_trajectories,
  n_traj = 100L,
  cut = NULL
  ) {
  x <- rep(1:total_areas, n_traj)
  data <- data.table(x = x, V1 = county_trajectories)
  data <- drop_na(data)
  data_M <- data[, .(m = min(V1), M = max(V1)), by = x]

  if (!is.null(cut)) {
    data_M <- data_M[!(x %in% c(1:cut))]
    data <- data[!(x %in% c(1:cut))]
  }

  data$x <- data$x + 1
  data_with_M <- left_join(data, data_M)
  differences <- data_with_M[
    ,
    .(diff_m = abs(V1 - m),
    diff_M = abs(V1 - M))
  ]
  max_differences <- apply(drop_na(differences), 1, max)

  return(
    mean(max_differences)
  )
}

I1 <- function(
  total_areas,
  county_trajectories,
  statistic,
  n_traj = 100L,
  cut = NULL
  ) {
  
  x <- rep(1:total_areas, n_traj)
  data <- data.table(x = x, V1 = county_trajectories)
  data <- drop_na(data)
  # U(s) and L(s): the 95th and 5th percentiles of the simulated trajectories
  # at each scale, i.e. the 90% band of equation (3).
  M <- data[, quantile(V1, 1 - 0.05), by = x]$V1
  m <- data[, quantile(V1, 0.05), by = x]$V1

  if (!is.null(cut)) {
    M <- M[-c(1:cut)]
    m <- m[-c(1:cut)]
  }

  if (statistic == "r2" | statistic == "gini") {
    denominator <- 1
  }

  if (startsWith(statistic, "cor")) {
    denominator <- 2
  }

  if (startsWith(statistic, "beta") | 
  startsWith(statistic, "var") | 
  startsWith(statistic, "mean")) {
    denominator <- max(M) - min(m)
  }

  mean((M - m) / denominator)
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
is <- vector("list", length(unique(final_results$fips)))

# Identify statistic columns by name, not by position: final_results carries
# three id columns and positional arithmetic breaks whenever they change.
ID_COLS <- c("fips", "county", "state")
stat_cols <- setdiff(colnames(final_results), ID_COLS)
p <- length(stat_cols)
stat_names <- sub("\\..*$", "", stat_cols)
idx <- 1
# Grouped by FIPS.  County names are not unique across states -- Walton County
# exists in both Georgia and Florida -- so grouping by name would pool two
# different counties into one set of trajectories.
for (.fips in unique(final_results$fips)) {
  for (k in 1:p) {
    statistic <- stat_names[k]
    colname <- stat_cols[k]
    # w(s) = 0 below S_MIN, applied only to statistics without fixed
    # theoretical bounds.  Trajectory row x corresponds to s = x + 1, so
    # dropping rows 1..(S_MIN - 2) implements the weight.  With S_MIN = 12
    # this discards the first ten attainable scales, s = 2, ..., 11.
    if (statistic == "beta" | statistic == "var" | statistic == "mean") {
      cut <- S_MIN - 2L
    } else {
      cut <- NULL
    }

    state <- unique(final_results[fips == .fips, state])
    .county <- unique(final_results[fips == .fips, county])
    total_areas <- original_scale[fips == .fips, V1]
    county_trajectories <- unlist(
      final_results[fips == .fips, ..colname]
    )
    
    i1 <- I1(
      county_trajectories = county_trajectories,
      total_areas = total_areas,
      statistic = statistic,
      cut = cut
    )
    i2 <- I2(
      total_areas = total_areas,
      county_trajectories = county_trajectories,
      cut = cut
    )

    is[[idx]] <- data.frame(
      i1 = i1,
      i2 = i2,
      fips = .fips,
      county = .county,
      state = state,
      colname = colname
    )
    idx <- idx + 1
  }
}

da <- do.call(rbind, is)

saveRDS(da, "data/index_random_quantile_90.rds")
