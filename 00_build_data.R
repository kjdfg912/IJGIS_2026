# ---------------------------------------------------------------------------
# Builds data/data_corr.Rdata from the 2020 ACS (5-year), tract level.
#
# This is the first step of the workflow: it downloads the seven ACS tables
# used in the paper, collapses the bracketed tables to one value per tract, and
# writes the tract-level extract that every later script reads.
#
# Requires a Census API key, which should not be hard-coded here.  Register a
# key at https://api.census.gov/data/key_signup.html and install it once with
#   census_api_key("<key>", install = TRUE)
# or export CENSUS_API_KEY in the environment before running this script.
# ---------------------------------------------------------------------------

library(tidycensus)
library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)

options(tigris_use_cache = TRUE)
if (!nzchar(Sys.getenv("CENSUS_API_KEY"))) {
  stop("Set CENSUS_API_KEY (see census_api_key()) before running this script.")
}

STATES <- c("CA", "NY", "TX", "FL", "PA", "IL", "OH", "GA", "NC", "MI")
YEAR   <- 2020

TABLES <- c(Total_population = "B01003",  # total population
            Race             = "B02001",  # _003 = Black or Afr. American alone
            Travel_time      = "B08303",  # travel time to work, bracketed
            Earnings         = "B19001",  # household income, bracketed
            Hours_worked     = "B23020",  # _001 = mean usual hours worked
            Rooms            = "B25017",  # rooms, bracketed
            Housing_units    = "B25001")  # housing units

# Bracket midpoints (censusreporter.org).  Open-ended top categories are
# assigned the values used in the original extraction.
INCOME_MID <- c(B19001_002 = 5000,   B19001_003 = 12500,  B19001_004 = 17500,
                B19001_005 = 22500,  B19001_006 = 27500,  B19001_007 = 32500,
                B19001_008 = 37500,  B19001_009 = 42500,  B19001_010 = 47500,
                B19001_011 = 55000,  B19001_012 = 67500,  B19001_013 = 87500,
                B19001_014 = 112500, B19001_015 = 137500, B19001_016 = 175000,
                B19001_017 = 250000)                      # "$200,000 or more"

TRAVEL_MID <- c(B08303_002 = 2.5, B08303_003 = 7,    B08303_004 = 12,
                B08303_005 = 17,  B08303_006 = 22,   B08303_007 = 27,
                B08303_008 = 32,  B08303_009 = 37,   B08303_010 = 42,
                B08303_011 = 52,  B08303_012 = 74.5, B08303_013 = 110)
                                                          # "90 or more minutes"

ROOMS_MID  <- c(B25017_002 = 1, B25017_003 = 2, B25017_004 = 3, B25017_005 = 4,
                B25017_006 = 5, B25017_007 = 6, B25017_008 = 7, B25017_009 = 8,
                B25017_010 = 11)                          # "9 or more rooms"

fetch <- function(tbl) {
  do.call(rbind, lapply(STATES, function(s)
    get_acs(geography = "tract", table = tbl, state = s, year = YEAR,
            cache_table = TRUE, geometry = TRUE)))
}
data_us <- lapply(TABLES, fetch)

# Aggregate a bracketed table to one value per tract: sum(count * midpoint).
collapse <- function(df, mids, out) {
  x <- as.data.table(st_drop_geometry(df))
  x[, mid := unname(mids[variable])]
  x <- x[!is.na(mid), .(v = sum(estimate * mid, na.rm = TRUE)), by = GEOID]
  setnames(x, "v", out)[]
}
# Pull a single variable from a table.
single <- function(df, v, out) {
  x <- as.data.table(st_drop_geometry(df))[variable == v, .(GEOID, v = estimate)]
  setnames(x, "v", out)[]
}

earn    <- collapse(data_us$Earnings,    INCOME_MID, "earning")
tt      <- collapse(data_us$Travel_time, TRAVEL_MID, "time_travel")
rooms   <- collapse(data_us$Rooms,       ROOMS_MID,  "rooms")
race    <- single(data_us$Race,          "B02001_003", "race")
pop     <- single(data_us$Total_population, "B01003_001", "estimate")
hw      <- single(data_us$Hours_worked,  "B23020_001", "hw")
hu      <- single(data_us$Housing_units, "B25001_001", "hu")

# Workers with a reported travel time.  Carried through so that mean commute
# time per worker can be formed if wanted; the paper uses aggregate commute
# minutes per resident (see 01_loading.R).
workers <- single(data_us$Travel_time,   "B08303_001", "workers")

# Tract geometry and the State / County labels, taken once from B01003.
base <- data_us$Total_population %>%
  filter(variable == "B01003_001") %>%
  separate(NAME, into = c("Tract", "County", "State"), sep = ",\\s*") %>%
  select(GEOID, State, County)

# Joined on GEOID, which is stable across the seven separate downloads.
data_corr <- base %>%
  left_join(earn, "GEOID") %>% left_join(race, "GEOID") %>%
  left_join(pop, "GEOID")  %>% left_join(tt, "GEOID") %>%
  left_join(hw, "GEOID")   %>% left_join(rooms, "GEOID") %>%
  left_join(hu, "GEOID")   %>% left_join(workers, "GEOID") %>%
  select(GEOID, State, County, earning, race, estimate, time_travel,
         hw, rooms, hu, workers)

# If the archived extract is absent, this run becomes the pipeline input.
# If it is present, write alongside it and report the comparison instead, so
# that rebuilding never overwrites the copy the published results came from.
if (!file.exists("data/data_corr.Rdata")) {
  save(data_corr, file = "data/data_corr.Rdata")
  cat("wrote data/data_corr.Rdata (", nrow(data_corr), "tracts )\n")
} else {
  save(data_corr, file = "data/data_corr_rebuilt.Rdata")
  cat("wrote data/data_corr_rebuilt.Rdata; comparing with the archived extract\n")

  archived <- local({
    e <- new.env(); load("data/data_corr.Rdata", envir = e); e$data_corr
  })
  j <- merge(st_drop_geometry(data_corr), st_drop_geometry(archived),
             by = "GEOID", suffixes = c(".new", ".old"))
  cat(sprintf("matched tracts: %d of %d archived\n", nrow(j), nrow(archived)))
  for (v in c("earning", "race", "estimate", "time_travel", "hw", "rooms", "hu")) {
    x <- j[[paste0(v, ".new")]]; y <- j[[paste0(v, ".old")]]
    ok <- is.finite(x) & is.finite(y)
    cat(sprintf("  %-12s identical=%-5s max|diff|=%s\n", v,
                isTRUE(all(abs(x[ok] - y[ok]) < 1e-6)),
                format(if (any(ok)) max(abs(x[ok] - y[ok])) else NA)))
  }
}
