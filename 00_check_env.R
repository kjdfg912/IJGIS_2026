# Fail fast if the environment is not ready to run the pipeline.
pkgs <- c("sf", "spdep", "data.table", "dplyr", "tidyr", "ggplot2",
          "foreach", "doSNOW", "argparse", "reshape2", "latex2exp", "units")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("missing R packages: ", paste(missing, collapse = ", "),
       "\n  install with: Rscript -e 'install.packages(c(",
       paste(sprintf('"%s"', missing), collapse = ","), "))'", call. = FALSE)
}
if (!file.exists("data/data_corr.Rdata")) {
  stop("missing data/data_corr.Rdata (the only pipeline input)", call. = FALSE)
}
cat(sprintf("environment OK: %d packages present, input data found (%.1f MB)\n",
            length(pkgs), file.size("data/data_corr.Rdata") / 1048576))
cat(sprintf("cores detected: %d\n", parallel::detectCores()))
