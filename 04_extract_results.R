#-------------------------------------------------------------------------------
source("01_loading.R")

load("data/results_random.Rdata")
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
n_counties <- length(sim_results)
results <- vector("list", n_counties)

for (i in 1:n_counties) {
  results_county <- lapply(
    sim_results[[i]],
    function(x) x$results
  )
  results_county <- data.frame(do.call(rbind, results_county))
  results_county$fips <- sim_results[[i]][[1]]$fips
  results_county$county <- sim_results[[i]][[1]]$county
  results_county$state <- sim_results[[i]][[1]]$state

  results[[i]] <- results_county

  if (i %% 10 == 0) print(i)
}

final_results <- as.data.table(do.call(rbind, results))
head(final_results)

save(final_results, file = "data/final_results_random.Rdata")
#-------------------------------------------------------------------------------