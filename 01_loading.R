# Packages used by the analysis pipeline.  The ACS extraction that produces
# data/data_corr.Rdata needs tidycensus as well and lives in 00_build_data.R,
# which loads it separately.
library(data.table)
library(dplyr)
library(tidyr)
library(sf)
library(spdep)
library(ggplot2)
library(foreach)
library(doSNOW)
library(argparse)

source("00_functions.R")

load("data/data_corr.Rdata")

# ---------------------------------------------------------------------------
# County key.
#
# Counties are identified by their 5-digit state+county FIPS code rather than
# by name.  Within these ten states there are 1040 counties but only 783
# distinct county names, and 131 names (Adams, Butler, Clark, Cook, ...) occur
# in more than one state, so a name is not a unique key.
# ---------------------------------------------------------------------------
data_corr$fips <- substr(data_corr$GEOID, 1, 5)

# Human-readable labels, carried alongside the key for plotting and reporting.
county_lookup <- unique(
  st_drop_geometry(data_corr)[, c("fips", "County", "State")]
)
stopifnot(!anyDuplicated(county_lookup$fips))

# Derived per-tract variables.
#
#   earning / hu        -> mean household income (USD); `hu` is housing units
#   race    / estimate  -> share of the population identifying as Black alone
#   time_travel / estimate -> AGGREGATE COMMUTE MINUTES PER RESIDENT.  This is
#       not the mean commute time of commuters: the correct denominator is the
#       number of workers, which is not available in this extract.  Median is
#       ~12.7 min against a true mean one-way commute of ~26 min, because the
#       denominator includes non-workers.  Reported as such in the paper.
#   rooms   / hu        -> mean number of rooms per housing unit
data_corr$earning <- data_corr$earning / data_corr$hu
data_corr$race <- data_corr$race / data_corr$estimate
data_corr$time_travel <- data_corr$time_travel / data_corr$estimate
data_corr$rooms <- data_corr$rooms / data_corr$hu
