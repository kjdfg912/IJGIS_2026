#-------------------------------------------------------------------------------
source("01_loading.R")

load("data/results_random_gini.Rdata")
load("data/final_results_random.Rdata")
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
n_counties <- length(sim_results)
results_gini <- vector("list", n_counties)

for (i in 1:n_counties) {
  results_county_gini <- lapply(
    sim_results[[i]],
    function(x) x$results
  )
  results_county_gini <- data.frame(do.call(rbind, results_county_gini))
  results_county_gini$fips <- sim_results[[i]][[1]]$fips
  results_county_gini$county <- sim_results[[i]][[1]]$county
  results_county_gini$state <- sim_results[[i]][[1]]$state

  results_gini[[i]] <- results_county_gini

  if (i %% 10 == 0) print(i)
}

final_results_gini <- as.data.table(do.call(rbind, results_gini))

# The gini trajectories are cbind()ed onto final_results positionally, so the
# two tables must be row-aligned.
stopifnot(identical(final_results_gini$fips, final_results$fips))

final_results <- cbind(gini = final_results_gini$gini, final_results)

save(final_results, file = "data/final_results_random.Rdata")
#-------------------------------------------------------------------------------

