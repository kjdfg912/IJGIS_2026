#-------------------------------------------------------------------------------
source("01_loading.R")
load("data/results_adv.Rdata")
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
idx <- unlist(lapply(adv_results, function(x) !is.null(x)))
results <- readRDS("data/sim_setup.rds")
results <- results[idx, ]
adv_results <- adv_results[idx]

final_adv_results <- vector("list", nrow(results))
for (i in 1:nrow(results)) {
  final_adv_results[[i]] <- data.frame(
    fips = results[i, ]$fips,
    County = adv_results[[i]]$County,
    State = adv_results[[i]]$State,
    Var1 = results[i, ]$Var1,
    Var2 = results[i, ]$Var2,
    M = adv_results[[i]]$Results$M,
    m = adv_results[[i]]$Results$m,
    County_size = results[i, ]$Freq
  )
  if (i %% 100 == 0) print(i)
}

save(final_adv_results, file = "data/final_results_adv.Rdata")
#-------------------------------------------------------------------------------