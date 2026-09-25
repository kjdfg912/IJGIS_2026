N_THREADS ?= 1
N_SAMPLE ?= 100
# Number of counties to fit.  1040 is every county in the ten states; the
# >=20-tract filter then reduces this to the 355-county analysis sample.
# Override with a small value only for a quick trial run.
N_COUNTIES ?= 1040

all: data/index_random_quantile_90.rds data/index_adv.rds

# Step 0: download the ACS tables and build the tract-level extract.  Needs a
# Census API key and network access; see the README.  The repository already
# contains data/data_corr.Rdata, so this is only needed to rebuild it from the
# original source.
.PHONY: data
data:
	Rscript 00_build_data.R

# One command for a complete run: fits, indices, figures and the Section 6
# metrics report.
.PHONY: full
full: all figures metrics

# Recompute every number quoted in Section 6 and write section6_metrics.txt.
.PHONY: metrics
metrics: data/index_adv.rds
	Rscript 07_report_metrics.R

# Just the figures, from indices that already exist.
.PHONY: figures
figures: data/index_adv.rds
	@echo "Generating figures."
	Rscript 06_graphs.R
	Rscript figure_typical.R
	Rscript figure_vs_rho.R

# Fail fast if the environment is not ready.
.PHONY: check
check:
	Rscript 00_check_env.R

# Correctness check for the incremental edge-cost routine.
.PHONY: test
test:
	Rscript tools/test_prunecost.R

# Remove every intermediate and result file, so that `make all` recomputes the
# analysis from scratch.  data/data_corr.Rdata, the tract-level extract, is the
# only input and is kept.
.PHONY: clean-results
clean-results:
	rm -f data/results_random.Rdata data/results_random_gini.Rdata \
	      data/results_adv.Rdata data/final_results_random.Rdata \
	      data/final_results_adv.Rdata data/index_random_quantile_90.rds \
	      data/index_adv.rds data/index_adv_with_m_M.rds data/sim_setup.rds

.PHONY: random advs
random: data/index_random_quantile_90.rds
advs: data/index_adv.rds data/index_adv_with_m_M.rds

.PHONY: extract
extract:
	@echo "Extracting results from simulated data."
	Rscript 04_extract_results.R
	Rscript 04_extract_results_gini.R
	Rscript 04_extract_results_adv.R
	@echo "Evaluating indices."
	Rscript 05_evaluate_index.R
	Rscript 05_evaluate_index_adv.R
	@echo "Generating figures."
	Rscript 06_graphs.R
	Rscript figure_typical.R
	Rscript figure_vs_rho.R

.PHONY: fit_random fit_random_gini
fit_random: 02_fit_random.R
	@echo "Fitting the random model."
	Rscript 02_fit_random.R --n_threads $(N_THREADS) --n_sample $(N_SAMPLE) \
	--n_counties $(N_COUNTIES)

fit_random_gini: 02_fit_random_gini.R
	@echo "Fitting the random model for gini index."
	Rscript 02_fit_random_gini.R --n_threads $(N_THREADS) --n_sample $(N_SAMPLE) \
	--n_counties $(N_COUNTIES)

# Order-only prerequisite: an existing fit is not redone merely because the
# script's timestamp changed.  Use `make clean-results` to force a re-fit.
data/results_random.Rdata: | 02_fit_random.R
	@echo "Fitting the random model."
	Rscript 02_fit_random.R --n_threads $(N_THREADS) --n_sample $(N_SAMPLE) \
	--n_counties $(N_COUNTIES)

data/results_random_gini.Rdata: | 02_fit_random_gini.R
	@echo "Fitting the random model for gini index."
	Rscript 02_fit_random_gini.R --n_threads $(N_THREADS) --n_sample $(N_SAMPLE) \
	--n_counties $(N_COUNTIES)

.PHONY: fit_adv
fit_adv: 03_fit_adv.R
	@echo "Fitting adversarial model (this can take a long time)."
	Rscript 03_fit_adv.R --n_threads $(N_THREADS) --n_counties $(N_COUNTIES)

# No prerequisites, for the same reason as above: this fit is expensive and is
# not repeated unless data/results_adv.Rdata is missing.
data/results_adv.Rdata:
	@echo "Fitting adversarial model (this can take a long time)."
	Rscript 03_fit_adv.R --n_threads $(N_THREADS) --n_counties $(N_COUNTIES)

data/final_results_random.Rdata: data/results_random.Rdata data/results_random_gini.Rdata
	@echo "Extracting the results from the simulated data."
	Rscript 04_extract_results.R
	Rscript 04_extract_results_gini.R

data/final_results_adv.Rdata: data/results_adv.Rdata
	@echo "Extracting the results from the simulated data (ADV)."
	Rscript 04_extract_results_adv.R

data/index_random_quantile_90.rds: data/final_results_random.Rdata
	@echo "Evaluating index for random scenario"
	Rscript 05_evaluate_index.R

data/index_adv.rds: data/final_results_adv.Rdata data/index_random_quantile_90.rds
	@echo "Evaluating index for adversarial scenario"
	Rscript 05_evaluate_index_adv.R

# Produced by 05_evaluate_index_adv.R in the same run as data/index_adv.rds.
data/index_adv_with_m_M.rds: data/index_adv.rds
	@test -f $@ || (echo "Missing $@ (should be produced by 05_evaluate_index_adv.R)."; exit 1)
