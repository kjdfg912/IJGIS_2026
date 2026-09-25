source("01_loading.R")

parser <- ArgumentParser()
parser$add_argument("--n_threads", type = "integer", default = 50L)
parser$add_argument("--n_sample", type = "integer", default = 50L)
parser$add_argument("--verbose", type = "logical", default = TRUE)

args <- parser$parse_args()

n_sample <- args$n_sample
n_threads <- args$n_threads
verbose <- args$verbose

var1 <- "earning"
var2 <- "race"
# Keyed by FIPS, not by name: "Harris County" exists in both Texas and
# Georgia, and 48201 is the Texas one used in the paper.
counties <- c(
  "48201",  # Harris County, TX
  "36005",  # Bronx County, NY
  "06065",  # Riverside County, CA
  "48027",  # Bell County, TX
  "17197",  # Will County, IL
  "06019",  # Fresno County, CA
  "12103",  # Pinellas County, FL
  "39095"   # Lucas County, OH
)
n_counties <- length(counties)

cluster <- makeCluster(n_threads)
registerDoSNOW(cluster)

sim_results <- list()

message(sprintf("Starting evaluating simulation with ADV model with RANDOM trees. Using %d threads, generating %d samples for each county.\n",
n_threads, n_sample))

idx <- 1
for (k in 1:n_counties) {
	message(paste(k, "(k) out of", n_counties))
	dc <- data_corr[data_corr$fips == counties[k], ]
  dc <- drop_na(dc)
	dc <- dc[!st_is_empty(dc), ]
	dc_nb <- poly2nb(dc, snap = ADJ_SNAP)
	search <- n.comp.nb(dc_nb)
	idmax <- as.numeric(names(which.max(table(search$comp.id))))
	dc <- dc[search$comp.id == idmax, ]
  
	data <- dc[, c(var1, var2)]
	data <- st_drop_geometry(data)
	data <- as.data.frame(scale(data))
	geometry <- st_geometry(dc)
  
	sim_results[[idx]] <- foreach(
		i = 1:n_sample,
		.packages = c("sf", "spdep"), .verbose = verbose
	) %dopar% {
		set.seed(as.integer(counties[k]) * 1000L + i)
		skater_adv <- eval_maup_skater(
			model = "adversarial_random",
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
			fips = counties[k],
			County = unique(dc$County),
			State = unique(dc$State)
		))
	}
	idx <- idx + 1
}

saveRDS(sim_results, file = "data/results_adv_random.rds")