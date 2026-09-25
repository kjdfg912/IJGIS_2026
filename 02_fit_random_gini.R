source("01_loading.R")

parser <- ArgumentParser()
parser$add_argument("--n_threads", type = "integer", default = 1L)
parser$add_argument("--n_sample", type = "integer", default = 100L)
parser$add_argument("--n_counties", type = "integer", default = 1040L)
parser$add_argument("--verbose", type = "logical", default = FALSE)

args <- parser$parse_args()

n_sample <- args$n_sample
n_counties <- args$n_counties
n_threads <- args$n_threads
verbose <- FALSE

set.seed(2024)
# Counties are keyed by 5-digit FIPS, not by name (see 01_loading.R).
#
# Only counties with at least 20 tracts enter the analysis, so they are
# pre-filtered here and ordered smallest-first.  The ordering matters only when
# --n_counties is set below the number of eligible counties, where it makes the
# subset cheap to fit; the default covers every eligible county.
tract_counts <- table(data_corr$fips)
counties <- names(sort(tract_counts[tract_counts >= 20]))
un <- head(counties, n_counties)
message(sprintf("%d eligible counties; fitting %d.", length(counties), length(un)))
sim_results <- list()

cluster <- makeCluster(n_threads)
registerDoSNOW(cluster)

message(sprintf("Starting evaluating simulation for GINI with RANDOM model. Using %d threads, generating %d samples for each county, running for %d counties.\n",
n_threads, n_sample, n_counties))

idx <- 1
for (k in 1:n_counties) {
	message(paste(k, "(k) out of", n_counties))
	dc <- data_corr[data_corr$fips == un[k], ]
	dc <- drop_na(dc)
	if (nrow(dc) < 20) {
		next
	}
	dc <- dc[!st_is_empty(dc), ]
	if (nrow(dc) < 20) {
		next
	}
	dc_nb <- poly2nb(dc, snap = ADJ_SNAP)
	search <- n.comp.nb(dc_nb)
	idmax <- as.numeric(names(which.max(table(search$comp.id))))
	dc <- dc[search$comp.id == idmax, ]
	if (nrow(dc) < 20) {
		next
	}
  data <- dc[, c("earning", "estimate")]
	data <- st_drop_geometry(data)
	geometry <- st_geometry(dc)
	sim_results[[idx]] <- foreach(
		i = 1:n_sample,
		.packages = c("sf", "spdep"), .verbose = verbose
	) %dopar% {
		# Seed from the county FIPS and the draw index so the trajectory is
		# reproducible regardless of which worker runs it.  Cluster-level
		# seeding is not enough: doSNOW dispatches tasks load-balanced.
		set.seed(as.integer(un[k]) * 1000L + i)
		trajectory <- eval_maup_skater(
			model = "random",
			data = data,
			geometry = geometry,
			stat_function = eval_gini,
			# Population is an extensive quantity, so it is summed over
			# the areas forming each zone rather than averaged.
			sum_vars = "estimate",
			income_col = "earning",
			population_col = "estimate"
		)
  gini <- unlist(trajectory$stat)

	results <- data.frame(gini = gini)

	ret <- list(
		fips = un[k],
		county = unique(dc$County),
		state = unique(dc$State),
		results = results
	)

		return(ret)
	}
	idx <- idx + 1
}

save(sim_results, file = "data/results_random_gini.Rdata")