##------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(reshape2)
library(spdep)
library(latex2exp)

source("00_functions.R")

# ggsave() will not create this itself.
dir.create("figures", showWarnings = FALSE)
load("data/final_results_random.Rdata")
index_random <- readRDS("data/index_random_quantile_90.rds")
original_scale <- final_results[, .N / 100, by = fips]
da_adv_with_m_M <- readRDS("data/index_adv_with_m_M.rds")

# Counties shown in the paper's figures, keyed by FIPS.
#
# County names are not unique keys here: Wilson County exists in both Texas
# (48493) and North Carolina (37195), and Franklin County occurs in eight of
# the ten states.
FIPS <- c(
  dupage      = "17043",  # DuPage County, Illinois
  wilson      = "37195",  # Wilson County, North Carolina
  los_angeles = "06037",  # Los Angeles County, California
  travis      = "48453",  # Travis County, Texas
  lenawee     = "26091",  # Lenawee County, Michigan
  lucas       = "39095",  # Lucas County, Ohio
  el_dorado   = "06017",  # El Dorado County, California
  oakland     = "26125",  # Oakland County, Michigan
  riverside   = "06065",  # Riverside County, California
  new_york    = "36061",  # New York County, New York
  harris      = "48201",  # Harris County, Texas
  franklin    = "39049",  # Franklin County, Ohio
  san_joaquin = "06077",  # San Joaquin County, California
  washtenaw   = "26161"   # Washtenaw County, Michigan
)

# The figures below are drawn for named counties.  On a reduced run
# (--n_counties below 100) those counties need not have been fitted, so the
# script exits quietly; on a full run their absence is an error.
N_FITTED <- length(unique(final_results$fips))
SUBSET_RUN <- N_FITTED < 100L

if (SUBSET_RUN && !all(FIPS %in% final_results$fips)) {
  message(sprintf(paste("figure_typical.R: only %d counties were fitted, so the",
                        "paper's figure counties are not all present. Skipping.",
                        "\n  These figures need a full run (make full N_COUNTIES=1040)."),
                  N_FITTED))
  quit(save = "no", status = 0)
}

need_fips <- function(f) {
  if (!f %in% final_results$fips) {
    stop(sprintf(paste("county FIPS %s is missing from",
                       "data/final_results_random.Rdata even though %d counties were",
                       "fitted. Expected it in a full run."), f, N_FITTED), call. = FALSE)
  }
  invisible(f)
}
##------------------------------------------------------------------------------

##------------------------------------------------------------------------------
                                        # Figure 12
plot_typical_random <- function(
  this_fips, var, ylab_name, print_i1 = TRUE, print_i2 = TRUE, plot_adv = FALSE, plot_grey = FALSE, y_min_grey = NULL, y_max_grey = NULL,x_max_grey = 10, plot_rect = FALSE, y_anot = NULL
  ) {
    need_fips(this_fips)
    da <- final_results[fips == this_fips, ..var]
    da$x <- rep(1:(nrow(da) / 100), 100)
    da$group <- factor(rep(1:100, each = nrow(da) / 100))
    data_m <- as.data.table(melt(da,
      id.vars = c("x", "group"),
      value.name = "value"
    ))
    maxmin_AUX <- data_m[, .(
      max = max(value),
      min = min(value),
      avg = mean(value)
    ),
    by = x
    ]
    # alpha = 0.10, matching the quantiles used for I_m in 05_evaluate_index.R,
    # so that the band drawn here is the same band the annotated I_m measures.
    maxmin <- data_m[, .(
      max = quantile(value, 0.95, na.rm = TRUE),
      min = quantile(value, 0.05, na.rm = TRUE),
      avg = mean(value)
    ),
    by = x
    ]
    tau <- as.numeric(da[nrow(da), ..var])
    idx <- index_random$fips == this_fips & index_random$colname == var

    anot <- TeX(paste("$I_m$ =",
                      format(round(index_random[idx, ]$i1, 2),
                             nsmall = 2)))
    anot2 <- TeX(paste("$I_s$ =",
                       format(round(index_random[idx, ]$i2, 2),
                              nsmall = 2)))
    x_anot <- original_scale[fips == this_fips, ]$V1 * 0.9
    y_anot_given <- !is.null(y_anot)
    if (!y_anot_given) {
      y_anot <- max(maxmin_AUX$max, na.rm = TRUE) * 0.9
    }
    y_anot_2 <- min(maxmin$min, na.rm = TRUE) * 0.8
    p <- ggplot(data_m)
    if (plot_grey) {
      p <- p + annotate("rect",
        ymin = -Inf, ymax = Inf, xmin = 0, xmax = x_max_grey,
        fill = "grey85", alpha = 0.55
      )
    } else if (!y_anot_given) {
      # only fall back to the rectangle top when no position was supplied
      y_anot <- y_max_grey * 0.9
    }
    if (plot_rect) {
      if (is.null(y_min_grey)) {
        y_min_grey <- as.numeric(maxmin_AUX[maxmin_AUX$x == x_max_grey, "min"])
        y_max_grey <- as.numeric(maxmin_AUX[maxmin_AUX$x == x_max_grey, "max"])
      }
      
      p <- p + geom_hline(yintercept = y_min_grey, linewidth = 0.3, linetype = "dotted", colour = "grey35") +
        geom_hline(yintercept = y_max_grey, linewidth = 0.3, linetype = "dotted", colour = "grey35")
    }
    if (print_i1) {
      p <- p + annotate("text",
        x = x_anot, y = y_anot,
        label = anot, size = 3.4
      )
    }
    # Trajectories are density, not identity: thin, translucent, recessive, so
    # the band and bounds read on top of them instead of a solid black mass.
    p <- p +
        geom_path(aes(x = x, y = value, group = group),
                  colour = PAL[["traj"]], linewidth = 0.15, alpha = 0.25) +
        geom_hline(aes(yintercept = tau, colour = "Finest scale",
                       linetype = "Finest scale"), linewidth = 0.6) +
        geom_line(data = maxmin,
                  mapping = aes(x = x, y = max, colour = "90% band",
                                linetype = "90% band"), linewidth = 0.6) +
        geom_line(data = maxmin,
                  mapping = aes(x = x, y = min, colour = "90% band",
                                linetype = "90% band"), linewidth = 0.6) +
        ylab(ylab_name) +
        xlab("Number of clusters") +
        theme_paper()
    if (print_i2) {
      p <- p + annotate("text",
        x = x_anot, y = y_anot_2,
        label = anot2, size = 3.4
      )
    }
    if (plot_adv) {
      aux <- da_adv_with_m_M[da_adv_with_m_M$fips == this_fips &
        da_adv_with_m_M$colname == var, ]
      aux$x <- 1:nrow(aux)
      aux <- left_join(data_m, aux, by = c("x" = "x"))
      p <- p +
        geom_line(data = aux, mapping = aes(x = x, y = m, colour = "m(s)",
                                            linetype = "m(s)"), linewidth = 0.6) +
        geom_line(data = aux, mapping = aes(x = x, y = M, colour = "M(s)",
                                            linetype = "M(s)"), linewidth = 0.6)
    }

    # One merged legend over colour and linetype, so that m(s) and M(s) are
    # told apart by line type as well as by colour.
    p <- p +
      scale_colour_manual(
        name = NULL,
        values = c("Finest scale" = unname(PAL["ref"]),
                   "90% band"     = unname(PAL["band"]),
                   "m(s)"         = unname(PAL["bound"]),
                   "M(s)"         = unname(PAL["bound"]))
      ) +
      scale_linetype_manual(
        name = NULL,
        values = c("Finest scale" = "solid", "90% band" = "solid",
                   "m(s)" = "solid", "M(s)" = "22")
      ) +
      guides(colour = guide_legend(order = 1, keywidth = unit(14, "pt")),
             linetype = guide_legend(order = 1, keywidth = unit(14, "pt")))

    return(p)
}



p1 <- plot_typical_random(
  FIPS[["dupage"]], "cor.race.time_travel", "Correlation", print_i2 = FALSE,
  plot_grey = FALSE, y_min_grey = -1, y_max_grey = 1, plot_rect = TRUE
) + ylim(-1, 1)

# Gini panel.  Two constraints shaped this choice.
#
# (1) The axis.  Across all 355 counties the Gini never exceeds 0.292, so a
#     [0, 1] axis would be mostly blank; the y axis is left free to fit the
#     data.
#
# (2) Aggregation almost always reduces the Gini, because merging two zones
#     replaces their means with a weighted average, a progressive transfer.
#     86% of merge steps decrease it, so the band normally sits entirely below
#     the finest-scale value.  El Dorado County CA is the county where the 95th
#     percentile rises furthest above that line (by 0.0096, 4.2% of the panel
#     height) while still having enough tracts to show trajectory shape.
p2 <- plot_typical_random(FIPS[["el_dorado"]], "gini", "Gini Index",
                          print_i2 = FALSE, y_anot = 0.02)

p3 <- plot_typical_random(FIPS[["los_angeles"]], "r2", "Coefficient of determination", print_i2 = FALSE, y_min_grey = 0, y_max_grey = 1, plot_rect = TRUE) + ylim(0, 1)

p4 <- plot_typical_random(FIPS[["travis"]], "var.earning", "Variance", print_i2 = FALSE, plot_grey = TRUE, x_max_grey = 10, plot_rect = TRUE)

save_fig(p1, "fig04a_dupage_correlation", width = FIG_HALF, ratio = 0.72, subdir = "paper")
save_fig(p2, "fig04b_eldorado_gini", width = FIG_HALF, ratio = 0.72, subdir = "paper")
save_fig(p3, "fig04d_losangeles_r2", width = FIG_HALF, ratio = 0.72, subdir = "paper")
save_fig(p4, "fig04c_travis_variance", width = FIG_HALF, ratio = 0.72, subdir = "paper")
## ------------------------------------------------------------------------------



p1 <- plot_typical_random(
  FIPS[["lenawee"]], "cor.race.time_travel", "Correlation",
  print_i2 = FALSE,
  print_i1 = FALSE,
  plot_grey = FALSE,
  y_min_grey = -1,
  y_max_grey = 1,
  plot_rect = TRUE,
  plot_adv = TRUE
) + ylim(-1, 1)

p_oak <- plot_typical_random(
  FIPS[["oakland"]], "cor.earning.time_travel", "Correlation", print_i1 = FALSE,
  print_i2 = FALSE, plot_adv = TRUE
)
save_fig(p_oak, "fig02_oakland_income_commute", width = FIG_HALF, ratio = 0.72, subdir = "paper")

p_river <- plot_typical_random(
  FIPS[["riverside"]], "cor.earning.rooms", "Correlation", print_i1 = FALSE,
  print_i2 = FALSE, plot_adv = TRUE
)
save_fig(p_river, "fig02_riverside_income_rooms", width = FIG_HALF, ratio = 0.72, subdir = "paper")

p_la <- plot_typical_random(
  FIPS[["los_angeles"]], "cor.race.rooms", "Correlation", print_i1 = FALSE,
  print_i2 = FALSE, plot_adv = TRUE
)
save_fig(p_la, "losangeles_correlation", width = FIG_HALF, ratio = 0.72, subdir = "other")


p_sd <- plot_typical_random(
  FIPS[["new_york"]], "cor.earning.time_travel", "Correlation", print_i1 = FALSE,
  print_i2 = FALSE, plot_adv = TRUE
)
p_sd


p_sd <- plot_typical_random(
  FIPS[["new_york"]], "cor.race.rooms", "Correlation", print_i1 = FALSE,
  print_i2 = FALSE, plot_adv = TRUE
)
p_sd


save_fig(p_sd, "newyork_correlation", width = FIG_HALF, ratio = 0.72, subdir = "other")


#-------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(reshape2)
library(spdep)
library(latex2exp)

source("00_functions.R")
index_adv <- readRDS("data/index_adv_with_m_M.rds")
  
plot_typical <- function(this_fips, colname) {
    idx <- which(index_adv$fips == this_fips & index_adv$colname == colname)
    if (!length(idx)) stop(sprintf("FIPS %s / %s not in index_adv_with_m_M.rds (needs a full run)", this_fips, colname), call. = FALSE)
    county_m <- index_adv[idx, ]$m
    county_M <- index_adv[idx, ]$M
    data_m <- melt(data.frame(x = 1:length(county_m),
                              m = county_m,
                              M = county_M),
                   id.vars = "x", variable.name = "zs", value.name = "value")

    tau <- county_m[length(county_m)]
    # I1_ADV holds I_z.  The `i1` column of this table is I_m, joined on from
    # the random-model index, and is not what these panels annotate.
    anot <- TeX(paste("$I_z$ =", format(round(index_adv[idx[1], "I1_ADV"], 2), nsmall = 2)))
    x_anot <- length(county_m) * 0.9
    y_anot = 0.8
    p <- ggplot(data_m) +
        geom_hline(aes(yintercept = tau, colour = "Finest scale",
                       linetype = "Finest scale"), linewidth = 0.6) +
        geom_line(aes(x = x, y = value, colour = zs, linetype = zs),
                  linewidth = 0.6) +
        scale_colour_manual(
          name = NULL,
          values = c("Finest scale" = unname(PAL["ref"]),
                     "m" = unname(PAL["bound"]), "M" = unname(PAL["bound"]))
        ) +
        scale_linetype_manual(
          name = NULL,
          values = c("Finest scale" = "solid", "m" = "solid", "M" = "22")
        ) +
        guides(colour = guide_legend(keywidth = unit(14, "pt")),
               linetype = guide_legend(keywidth = unit(14, "pt"))) +
        ylab("Correlation") +
        xlab("Number of clusters") +
        annotate("text", x = x_anot, y = y_anot,
                 label = anot, size = 3.4) +
        theme_paper()
    return(p)
}

p_texas <- plot_typical(FIPS[["harris"]], "cor.earning.race")
p_franklin <- plot_typical(FIPS[["franklin"]], "cor.race.rooms")
p_san <- plot_typical(FIPS[["san_joaquin"]], "cor.earning.race")
p_washtenaw <- plot_typical(FIPS[["washtenaw"]], "cor.earning.time_travel")


save_fig(p_texas, "fig08a_harris", width = FIG_HALF, ratio = 0.72, subdir = "paper")
save_fig(p_franklin, "fig08b_franklin", width = FIG_HALF, ratio = 0.72, subdir = "paper")
save_fig(p_san, "fig08c_sanjoaquin", width = FIG_HALF, ratio = 0.72, subdir = "paper")
save_fig(p_washtenaw, "fig08d_washtenaw", width = FIG_HALF, ratio = 0.72, subdir = "paper")

