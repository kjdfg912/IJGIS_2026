source("01_loading.R")

parser <- ArgumentParser()
parser$add_argument("--n_threads", type = "integer", default = 1L)
parser$add_argument("--n_counties", type = "integer", default = 1040L)
args <- parser$parse_args()

n_threads <- args$n_threads
n_counties <- args$n_counties

cluster <- makeCluster(n_threads)
registerDoSNOW(cluster)

message(sprintf("Starting evaluating simulation with ADVERSARIAL model. Using %d threads.\n", n_threads))

vars <- t(combn(c("time_travel", "earning", "race", "rooms"), 2))
vars <- as.data.frame(vars)
# Same eligible, size-ordered selection as 02_fit_random.R, so that a given
# --n_counties yields the same subset of counties in both fits.
tract_counts <- table(data_corr$fips)
counties <- head(names(sort(tract_counts[tract_counts >= 20])), n_counties)
message(sprintf("Adversarial fit over %d counties x %d variable pairs.",
                length(counties), nrow(vars)))

result <- expand.grid(fips = counties, idx = 1:nrow(vars))
final_result <- merge(
    result, vars,
    by.x = "idx", by.y = "row.names"
)
final_result <- final_result[, -1]

colnames(final_result)[2:3] <- c("Var1", "Var2")

table_counties <- as.data.frame(table(data_corr$fips))
colnames(table_counties) <- c("fips", "Freq")

sim_setup <- left_join(final_result, table_counties)
sim_setup <- sim_setup[order(sim_setup$Freq, decreasing = TRUE), ]

saveRDS(sim_setup, "data/sim_setup.rds")

N <- nrow(sim_setup)
adv_results <- foreach(
	i = 1:N,
	.packages = c("sf", "spdep", "tidyr"),
	.verbose = TRUE
	) %dopar% {
		idx_county <- data_corr$fips == sim_setup$fips[i]
		dc <- data_corr[idx_county, ]
		dc <- drop_na(dc)
		if (nrow(dc) < 20) {
			return(NULL)
		}
		dc <- dc[!st_is_empty(dc), ]
		if (nrow(dc) < 20) {
			return(NULL)
		}
		dc_nb <- poly2nb(dc, snap = ADJ_SNAP)
		search <- n.comp.nb(dc_nb)
		idmax <- as.numeric(names(which.max(table(search$comp.id))))
		dc <- dc[search$comp.id == idmax, ]
		if (nrow(dc) < 20) {
			return(NULL)
		}
		data <- dc[, c(sim_setup$Var1[i], sim_setup$Var2[i])]
		data <- st_drop_geometry(data)
		data <- as.data.frame(scale(data))
		geometry <- st_geometry(dc)
		skater_adv <- eval_maup_skater(
			model = "adversarial",
			data = data,
			geometry = geometry
		)
		
		results_max <- skater_adv$skater_max$stat_correlation_adversarial
		results_min <- skater_adv$skater_min$stat_correlation_adversarial

		return(list(
			Results = data.frame(
				M = results_max,
				m = results_min
			),
			fips = sim_setup$fips[i],
			County = unique(dc$County),
			State = unique(dc$State)
		))
}

save(adv_results, file = "data/results_adv.Rdata")