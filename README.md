# A Stochastic Metric for the Modifiable Areal Unit Problem

This repository contains the data and code of the paper *A Stochastic Metric for the Modifiable Areal Unit Problem*.
It builds the tract-level extract from its original source
(the 2020 American Community Survey), fits both the random and the adversarial
models, computes the two MAUP indices `I_m` and `I_z`, and writes the manuscript
figures and a report of the quantities quoted in Section 6.

Everything runs from the top of this directory with relative paths, and no
script needs to be edited to reproduce the published results.

---

## 1. Software requirements

* GNU Make
* R (tested on 4.3 and 4.4)
* System libraries GDAL, GEOS and PROJ, required by `sf` and `spdep`. On
  Debian/Ubuntu: `apt install libgdal-dev libgeos-dev libproj-dev libudunits2-dev`
* R packages:

```sh
Rscript -e 'install.packages(c("sf","spdep","data.table","dplyr","tidyr",
  "ggplot2","foreach","doSNOW","argparse","reshape2","latex2exp","units"))'
```

`tidycensus` is needed only for step 2 below, the download from the Census API:

```sh
Rscript -e 'install.packages("tidycensus")'
```

Verify the installation before starting:

```sh
make check
```

This reports any missing package, confirms that the input data are present, and
prints the number of cores detected.

Every step of the analysis is run from the R scripts in this directory. There is
no interactive or graphical software in the workflow: all input parameters are
either set in the scripts themselves (documented at the point of use) or passed
on the `make` command line as `N_THREADS`, `N_SAMPLE` and `N_COUNTIES`.

---

## 2. Step 1: build the tract-level extract from the ACS

`data/data_corr.Rdata` is the only input the analysis needs, and a copy is
included in this repository. `00_build_data.R` regenerates it from the original
source, so the workflow can be followed end to end.

The script needs a free Census API key. Request one at
<https://api.census.gov/data/key_signup.html> and install it once:

```sh
Rscript -e 'tidycensus::census_api_key("YOUR_KEY", install = TRUE)'
```

or export it in the environment as `CENSUS_API_KEY`. Then:

```sh
make data
```

It downloads seven ACS 2020 five-year tables at census tract level for the ten
study states (CA, NY, TX, FL, PA, IL, OH, GA, NC, MI), collapses the bracketed
tables to one value per tract using the midpoints listed in the script, and
writes the extract. Because `data/data_corr.Rdata` is already present, the
script writes `data/data_corr_rebuilt.Rdata` instead of overwriting it and
prints a column-by-column comparison of the two. Delete
`data/data_corr.Rdata` first if you want the rebuild to become the input.

The download takes roughly 20 minutes and needs about 2 GB of scratch space for
the `tigris` geometry cache.

### ACS tables used

| table | variable in the extract | meaning |
|---|---|---|
| B01003 | `estimate` | total population |
| B02001_003 | `race` | population identifying as Black or African American alone |
| B08303 | `time_travel` | travel time to work, bracketed; collapsed with midpoints |
| B08303_001 | `workers` | workers with a reported travel time |
| B19001 | `earning` | household income, bracketed; collapsed with midpoints |
| B23020_001 | `hw` | mean usual hours worked |
| B25017 | `rooms` | rooms per unit, bracketed; collapsed with midpoints |
| B25001_001 | `hu` | housing units |

`01_loading.R` then forms the four analysis variables per tract:

| variable | definition |
|---|---|
| `earning` | `earning / hu`, mean household income in USD |
| `race` | `race / estimate`, share of the population identifying as Black alone |
| `time_travel` | `time_travel / estimate`, aggregate commute minutes per resident |
| `rooms` | `rooms / hu`, mean number of rooms per housing unit |

Note that `time_travel` is aggregate commute minutes per **resident**, not the
mean commute time of commuters: the denominator counts the whole population,
not workers. Its median is about 12.7 minutes against a true mean one-way
commute near 26 minutes. The paper reports it on this definition throughout.

---

## 3. Step 2: run the analysis

```sh
make all N_THREADS=<cores>
```

This runs, in order:

| script | reads | writes |
|---|---|---|
| `02_fit_random.R` | `data/data_corr.Rdata` | `data/results_random.Rdata` |
| `02_fit_random_gini.R` | `data/data_corr.Rdata` | `data/results_random_gini.Rdata` |
| `03_fit_adv.R` | `data/data_corr.Rdata` | `data/results_adv.Rdata`, `data/sim_setup.rds` |
| `04_extract_results.R` | `data/results_random.Rdata` | `data/final_results_random.Rdata` |
| `04_extract_results_gini.R` | the two files above | `data/final_results_random.Rdata` (with the Gini column) |
| `04_extract_results_adv.R` | `data/results_adv.Rdata`, `data/sim_setup.rds` | `data/final_results_adv.Rdata` |
| `05_evaluate_index.R` | `data/final_results_random.Rdata` | `data/index_random_quantile_90.rds` |
| `05_evaluate_index_adv.R` | `data/final_results_adv.Rdata` | `data/index_adv.rds`, `data/index_adv_with_m_M.rds` |

`02_fit_random.R` and `02_fit_random_gini.R` simulate 100 random aggregation
trajectories per county under the model of Algorithm 2, evaluating twenty
statistics at every scale. `03_fit_adv.R` runs the single-shot adversarial
estimator of Algorithm 4 for all six pairs of variables in every county,
producing `M(s)` and `m(s)`. `05_evaluate_index*.R` integrate these into `I_m`
(equation 3) and `I_z` (equation 2).

Counties with fewer than 20 census tracts are excluded, as are tracts with
missing values and tracts outside the largest connected component of the
county's adjacency graph. This leaves **355 counties**, and 355 x 6 = **2130
county-by-variable-pair scenarios** for the adversarial model.

### Reproducibility of the random model

Each trajectory is seeded from the county FIPS code and the draw index, inside
the parallel task, so the simulated trajectories do not depend on how many
threads are used or on the order in which tasks are dispatched. A run with
`N_THREADS=1` and a run with `N_THREADS=100` give identical results.

---

## 4. Step 3: figures and reported metrics

```sh
make figures     # all 20 figures
make metrics     # every number quoted in Section 6
```

or `make full N_THREADS=<cores>` to do steps 2 and 3 in one command.

Figures are written as vector PDF at final printed size, split into two
directories:

```
figures/paper/   the 14 panels that appear in the manuscript
figures/other/    6 exploratory figures that do not
```

Set `FIG_PNG=1` to also write 400 dpi PNGs: `FIG_PNG=1 make figures`.

---

## 5. Where each reported finding comes from

### Figures

| manuscript | file under `figures/paper/` | produced by |
|---|---|---|
| Figure 2, left | `fig02_oakland_income_commute.pdf` | `figure_typical.R` |
| Figure 2, right | `fig02_riverside_income_rooms.pdf` | `figure_typical.R` |
| Figure 4, upper left | `fig04a_dupage_correlation.pdf` | `figure_typical.R` |
| Figure 4, upper right | `fig04b_eldorado_gini.pdf` | `figure_typical.R` |
| Figure 4, lower left | `fig04c_travis_variance.pdf` | `figure_typical.R` |
| Figure 4, lower right | `fig04d_losangeles_r2.pdf` | `figure_typical.R` |
| Figure 5 | `fig05_index_by_state.pdf` | `06_graphs.R` |
| Figure 6 | `fig06_index_vs_statistic.pdf` | `figure_vs_rho.R` |
| Figure 7 | `fig07_index_vs_density.pdf` | `06_graphs.R` |
| Figure 8, four panels | `fig08a_harris.pdf`, `fig08b_franklin.pdf`, `fig08c_sanjoaquin.pdf`, `fig08d_washtenaw.pdf` | `figure_typical.R` |
| Figure 9 | `fig09_iz_vs_im.pdf` | `06_graphs.R` |

Figures 1 and 3 are schematic illustrations of the MAUP and of the SKATER
algorithm and are not generated from the data.

### Section 6 metrics

`make metrics` runs `07_report_metrics.R`, which recomputes every quantity
quoted in Section 6 from the files produced in step 2 and writes
`section6_metrics.txt`. Its sections correspond to the manuscript as follows:

| manuscript statement | heading in `section6_metrics.txt` |
|---|---|
| ten states, counties retained, tracts per county and their quartiles | *data description: states, counties and tracts* |
| the Gini index shows a smaller MAUP effect than the other statistics | *the Gini index against the other statistics* |
| variation of `I_m` across states for a given statistic | *variation across states for one statistic* |
| `I_m` is larger when the finest-scale correlation is near zero | *Figure 6: I_m against the value of the statistic* |
| higher population density goes with a lower MAUP effect; the same pattern holds for total population, tract count and area | *Figure 7: I_m against population density* |
| 2130 scenarios; `I_z` exceeds `I_m`; the two indices correlate at 0.86 | *Figures 8 and 9: the adversarial index I_z* |
| the `I_m` and `I_z` values printed inside the Figure 4 and Figure 8 panels | *Values annotated inside the figure panels* |
| the zoning range at `s = 100` and `s = 200` in Figure 2 | *Figure 2: the zoning range M(s) - m(s)* |
| the denominator `h` of equations (2) and (3) | *Denominator h of equations (2) and (3)* |

The report takes about a minute once the analysis files exist.

---

## 6. Runtime and hardware

`02_fit_random.R` iterates over counties sequentially and parallelises the 100
trajectories *within* each county, so useful parallelism is capped at 100 and
each county costs `ceil(100 / cores)` rounds. The speedup is therefore stepped
rather than smooth:

| cores | rounds per county | random fit | adversarial fit | total |
|---|---|---|---|---|
| 16 | 7 | 3.5 h | 0.16 h | ~3.6 h |
| 32 | 4 | 2.0 h | 0.08 h | ~2.1 h |
| 50 | 2 | 1.0 h | 0.05 h | ~1.0 h |
| 64 | 2 | 1.0 h | 0.04 h | ~1.0 h |
| 100 | 1 | 0.5 h | 0.03 h | ~0.5 h |
| 112 | 1 | 0.5 h | 0.02 h | ~0.5 h |

100 cores is the last value that buys anything: 64 is worth the same as 50, and
112 the same as 100.

Each parallel worker is a separate R process holding `sf` and `spdep`, about
156 MB resident. Budget `cores x 156 MB` plus roughly 2 GB for the master
process as results accumulate:

| cores | worker memory | request |
|---|---|---|
| 32 | ~5 GB | 8 GB |
| 50 | ~8 GB | 12 GB |
| 100 | ~15 GB | 32 GB |

`data/final_results_random.Rdata` is about 570 MB; allow 2 GB of disk for the
intermediate files.

### Trial run

To confirm the pipeline works before committing a full run, fit a small number
of counties:

```sh
make clean-results
make all N_COUNTIES=10 N_THREADS=4     # about 10 minutes
```

`06_graphs.R` and `figure_vs_rho.R` work on any subset. `figure_typical.R`
draws named counties from the paper and will report that they were not fitted;
this is expected on a reduced run. Run `make clean-results` again before the
full run.

---

## 7. Implementation notes

**Counties are keyed by FIPS.** Counties are identified by the five-digit
state+county FIPS code (`substr(GEOID, 1, 5)`), not by name. Within these ten
states there are 1040 counties but only 783 distinct county names, and 131
names occur in more than one state, so a name is not a unique key.

**The Gini index is population-weighted.** `gini_index()` computes the Gini of
the income distribution across people, with each zone contributing its mean
income weighted by its total population. Zone population is summed over the
constituent tracts rather than averaged; see the `sum_vars` argument of
`eval_stat_function()`. With equal weights the function reduces exactly to the
unweighted Gini index.

**`M(s)` and `m(s)` come from two independent searches.** The tree used to push
the correlation up weights edges by their distance to the line `y = x`; the
tree used to push it down uses the distance to `y = -x`. Neither depends on the
sign of the correlation at the finest scale. Because the two greedy searches
are independent, neither dominates the other at every scale, so
`05_evaluate_index_adv.R` takes their pointwise maximum and minimum as the
envelope and reports how often they cross.

**The denominator `h`.** In equations (2) and (3), `h` is the range of the
statistic where the statistic has one (2 for a correlation, 1 for the Gini
index and `R^2`), and the observed range of the band otherwise. For a
correlation this coincides exactly with `max_s M(s) - min_s m(s)`, since a
two-zone partition attains ±1. For the Gini index and `R^2` it does not: the
attained range is about 0.16 and 0.83 respectively, so the theoretical bound is
the wider and more conservative reference. `make metrics` prints both, under
*Denominator h of equations (2) and (3)*.

**The weight `w(s)`.** For the statistics with no fixed theoretical bounds
(the means, variances and regression coefficients) `w(s) = 0` below
`S_MIN = 12`, which discards the ten coarsest scales `s = 2, ..., 11`. Bounded statistics use
`w(s) = 1` throughout. `S_MIN` is set at the top of `05_evaluate_index.R`.

---

## 8. File map

```
00_functions.R            model, statistics, SKATER variants, figure style
00_check_env.R            environment check (make check)
00_build_data.R           ACS download and extract construction (make data)
01_loading.R              packages, input data, derived variables
02_fit_random.R           random model, nineteen statistics
02_fit_random_gini.R      random model, Gini index
03_fit_adv.R              adversarial model, single-shot M(s) and m(s)
03_fit_adv_random.R       Algorithm S3, the adversarial model over repeated
                          random trees; provided for completeness, not part of
                          the reproduction path (see below)
04_extract_results*.R     reshape simulation output into trajectory tables
05_evaluate_index.R       I_m and I_s from the random trajectories
05_evaluate_index_adv.R   I_z from the adversarial bounds
06_graphs.R               Figures 5, 7 and 9, plus exploratory figures
figure_typical.R          Figures 2, 4 and 8
figure_vs_rho.R           Figure 6
07_report_metrics.R       Section 6 metrics report (make metrics)
Makefile                  the workflow above
tools/test_prunecost.R    correctness check for the incremental edge cost
```

`03_fit_adv_random.R` implements Algorithm S3, the adversarial estimator that
prunes greedily over repeated random spanning trees. The paper describes it in
Appendix C but reports results from the single-shot estimator of Algorithm S4,
so this script is not called by the Makefile and no reported finding depends on
it. It is included so that the appendix has code behind it.

`make test` runs `tools/test_prunecost.R`, which compares the incremental
edge-cost routine used by the adversarial pruner against a direct
implementation over 116 cases drawn from the real data. It takes a few seconds
and reports the largest discrepancy.

---

## 9. AI usage statement
The method and experimental setup were first implemented without the use of any
AI tools by an experienced researcher. Later, we used Claude Code to improve
efficiency, reproducibility, and this documentation.