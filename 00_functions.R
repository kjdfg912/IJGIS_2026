# Ensure scripts are runnable from any working directory when executed via Rscript.
.local_setwd_to_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) < 1) return(invisible(FALSE))
  script_path <- sub("^--file=", "", file_arg[1])
  script_dir <- dirname(normalizePath(script_path))
  if (dir.exists(script_dir)) {
    setwd(script_dir)
    return(invisible(TRUE))
  }
  invisible(FALSE)
}
.local_setwd_to_script_dir()
rm(.local_setwd_to_script_dir)

# ---------------------------------------------------------------------------
# Figure style
#
# Figures are produced at their final printed size, so type is specified once
# and is not rescaled by the typesetter.  IJGIS (Taylor & Francis) sets a text
# block about 6.9 in wide; a half-width figure is about 3.35 in.
#
# Output is vector PDF, so lines and text stay sharp at any magnification.
# Setting the environment variable FIG_PNG to any non-empty value also writes
# a 400 dpi PNG of each figure, for previewing or for Word-based workflows.
# ---------------------------------------------------------------------------

FIG_FULL <- 6.9   # inches, full text width
FIG_HALF <- 3.35  # inches, one column
FIG_BASE <- 11    # pt, base font size at final size

# Okabe-Ito palette, which stays legible under the common forms of colour
# vision deficiency: the blue/vermillion pair below separates by dE 21.9 in
# the worst case (deuteranopia).  Line type differs as well, so no curve is
# identified by colour alone.
PAL <- c(
  band  = "#0072B2",  # blue      - U(s)/L(s), the 90% band behind I_m
  bound = "#D55E00",  # vermillion- M(s)/m(s), the adversarial bounds behind I_z
  traj  = "grey55",   # simulated trajectories: background density
  ref   = "grey15",   # the statistic at the finest scale
  # Scatter marks in Figures 6 and 7, where the points are the data.  Points
  # drawn under a fitted line (figure_vs_rho.R) use the lighter `traj` grey.
  mark  = "grey15"
)

theme_paper <- function(base_size = FIG_BASE) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      # Grid stays recessive but dark enough to survive printing and
      # downscaling to journal column width.
      panel.grid.major = ggplot2::element_line(colour = "grey82", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border     = ggplot2::element_rect(colour = "grey35", linewidth = 0.45),
      axis.ticks       = ggplot2::element_line(colour = "grey35", linewidth = 0.4),
      axis.text        = ggplot2::element_text(colour = "grey15"),
      axis.title       = ggplot2::element_text(colour = "black"),
      strip.background = ggplot2::element_rect(fill = "grey90", colour = "grey35",
                                               linewidth = 0.45),
      strip.text       = ggplot2::element_text(colour = "black", face = "plain",
                                               margin = ggplot2::margin(3, 3, 3, 3)),
      legend.position  = "top",
      legend.text      = ggplot2::element_text(colour = "black"),
      legend.key       = ggplot2::element_blank(),
      legend.margin    = ggplot2::margin(0, 0, 0, 0),
      legend.box.spacing = ggplot2::unit(2, "pt"),
      plot.margin      = ggplot2::margin(3, 6, 3, 3)
    )
}

# Write one figure.
#
# PDF only by default, which is the format the manuscript uses.  Set FIG_PNG=1
# in the environment to also emit a 400 dpi PNG for previewing.
#
# `subdir` separates the figures that appear in the paper from the exploratory
# ones, so the manuscript set can be reviewed on its own.
save_fig <- function(plot, name, width = FIG_FULL, height = NULL,
                     ratio = 0.62, subdir = "other") {
  if (is.null(height)) height <- width * ratio
  outdir <- file.path("figures", subdir)
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

  pdf_path <- file.path(outdir, paste0(name, ".pdf"))
  ggplot2::ggsave(pdf_path, plot = plot, width = width, height = height,
                  units = "in", device = grDevices::cairo_pdf)

  if (nzchar(Sys.getenv("FIG_PNG"))) {
    ggplot2::ggsave(file.path(outdir, paste0(name, ".png")), plot = plot,
                    width = width, height = height, units = "in",
                    dpi = 400, bg = "white")
  }
  invisible(pdf_path)
}

rename_var <- function(input_string) {
  switch(input_string,
    "time_travel.earning" = "cor.earning.time_travel",
    "time_travel.race" = "cor.race.time_travel",
    "time_travel.rooms" = "cor.time_travel.rooms",
    "earning.race" = "cor.earning.race",
    "earning.rooms" = "cor.earning.rooms",
    "race.rooms" = "cor.race.rooms",
    stop("Input not recognized")
  )
}


cor_vector <- function(cor_matrix) {
  result <- c()
  
  var_names <- colnames(cor_matrix)
  
  for (i in 1:(ncol(cor_matrix) - 1)) {
    for (j in (i + 1):ncol(cor_matrix)) {
      name <- paste(var_names[i], var_names[j], sep = ",")
      
      result[name] <- cor_matrix[i, j]
    }
  }
  
  return(result)
}

# Population-weighted Gini index of `income`, where each zone contributes with
# weight `population`.  This is the Gini of the income distribution across
# people, obtained as one minus twice the area under the weighted Lorenz curve,
# evaluated by the trapezoid rule.
#
# The weighting matters here because zones vary in population: the Gini of the
# per-zone income values, unweighted, would let a zone of 50 people count as
# much as a zone of 5,000.  With equal weights this function reduces exactly to
# the unweighted Gini index.
gini_index <- function(income, population) {
  keep <- is.finite(income) & is.finite(population) & population > 0
  income <- income[keep]
  population <- population[keep]

  if (length(income) < 2L) return(NA_real_)
  if (sum(population) <= 0) return(NA_real_)

  ord <- order(income)
  income <- income[ord]
  population <- population[ord]

  total_income <- sum(population * income)
  if (!is.finite(total_income) || total_income <= 0) return(NA_real_)

  # cumulative population share and cumulative income share
  p <- cumsum(population) / sum(population)
  q <- cumsum(population * income) / total_income

  p_prev <- c(0, p[-length(p)])
  q_prev <- c(0, q[-length(q)])

  1 - sum((p - p_prev) * (q + q_prev))
}


cost_random <- function(data, id, id.neigh) {
  return(rnorm(length(id.neigh)))
}

# Snapping tolerance for building the adjacency graph.  data_corr is in NAD83
# (geographic), so 1e-5 degrees is roughly 1.1 m.  The same value is used when
# selecting the largest connected component, so that the component filter and
# the partitioning step operate on the same graph.
ADJ_SNAP <- 1e-5

create_mstree <- function(geometry, data, cost_function) {
  neighbourhood <- poly2nb(geometry, snap = ADJ_SNAP)
  costs <- nbcosts(neighbourhood, data, cost_function)
  neighbourhood_weights <- nb2listw(
    neighbourhood,
    costs,
    style = "B"
  )
  return(mstree(neighbourhood_weights))
}

eval_maup_skater <- function(
    model,
    data,
    geometry,
    cost_function_mstree = NULL,
    stat_function = NULL,
    n_cuts = NULL, ...) {
  if (!(model %in% c("adversarial", "random", "adversarial_random"))) {
    stop("Wrong model argument")
  }

  if (is.null(n_cuts)) {
    n_cuts <- nrow(data) - 1
  }

  if (model == "adversarial") {
    # M(s) and m(s) are estimated from two independent trees.  The tree that
    # pushes the correlation up weights edges by distance to the line y = x;
    # the tree that pushes it down uses distance to y = -x.  Neither depends
    # on the sign of the correlation at the finest scale.
    #
    # maup_skater_adv() always prunes the edge with the largest cost, and
    # stat_correlation_adversarial() returns +cor for type = "negative" and
    # -cor for type = "positive".  So type = "negative" maximises the
    # correlation (giving M) and type = "positive" minimises it (giving m,
    # once the sign flip is undone below).
    max_mstree <- create_mstree(
      geometry = geometry,
      data = data,
      cost_function = cost_correlation_adversarial_identity
    )
    min_mstree <- create_mstree(
      geometry = geometry,
      data = data,
      cost_function = cost_correlation_adversarial_minus_identity
    )
    skater_max <- maup_skater_adv(
      edges = max_mstree[, 1:2],
      data = data,
      n_cuts = n_cuts,
      type = "negative"
    )
    skater_min <- maup_skater_adv(
      edges = min_mstree[, 1:2],
      data = data,
      n_cuts = n_cuts,
      type = "positive"
    )
    skater_min$stat_correlation_adversarial <- -skater_min$stat_correlation_adversarial

    return(list(skater_max = skater_max, skater_min = skater_min))
  }

  if (model == "random") {
    if (is.null(stat_function)) {
      stop("stat_function must be provided if the model is random")
    }

    if (is.null(cost_function_mstree)) {
      cost_function_mstree <- cost_random
    }

    random_mstree <- create_mstree(
      geometry = geometry,
      data = data,
      cost_function = cost_function_mstree
    )
    edges <- random_mstree[, 1:2]
    ret <- maup_skater_random(
      edges = edges,
      data = data,
      stat_function = stat_function,
      n_cuts = n_cuts, ...
    )

    return(ret)
  }

  if (model == "adversarial_random") {
    if (is.null(cost_function_mstree)) {
      cost_function_mstree <- cost_random
    }

    random_mstree <- create_mstree(
      geometry = geometry,
      data = data,
      cost_function = cost_function_mstree
    )
    edges <- random_mstree[, 1:2]

    # Both bounds are searched on the same random tree; as above the labelling
    # does not depend on the sign of the correlation at the finest scale.
    skater_max <- maup_skater_adv(
      edges = edges,
      data = data,
      n_cuts = n_cuts,
      type = "negative"
    )
    skater_min <- maup_skater_adv(
      edges = edges,
      data = data,
      n_cuts = n_cuts,
      type = "positive"
    )
    skater_min$stat_correlation_adversarial <- -skater_min$stat_correlation_adversarial

    return(list(skater_max = skater_max, skater_min = skater_min))
  }
}

# `means_groups` carries one row per zone: the income column holds the zone
# mean income and the population column the zone total population (see the
# `sum_vars` argument of eval_stat_function()).
eval_gini <- function(means_groups, ...) {
  means_groups <- as.data.frame(do.call(rbind, means_groups))
  additional_args <- list(...)
  gini <- gini_index(
    income = means_groups[, additional_args$income_col],
    population = means_groups[, additional_args$population_col]
  )

  return(unlist(gini))
}

eval_stats <- function(means_groups, ...) {
  additional_args <- list(...)
  means_groups <- as.data.frame(do.call(rbind, means_groups))
  regression_data <- means_groups[, additional_args$regression_vars]

  mean_data <- means_groups[, additional_args$mean_vars]
  var_data <- means_groups[, additional_args$var_vars]
  cor_data <- means_groups[, additional_args$cor_vars]

  if (ncol(regression_data) > nrow(regression_data)) {
    r2 <- NA
    beta <- rep(NA, ncol(regression_data))
  } else {
    model <- lm(additional_args$formula, data = regression_data)
    summary_model <- summary(model)
    r2 <- summary_model$r.squared
    beta <- coef(model)
  }

  mean_value <- colMeans(mean_data)
  var_value <- apply(var_data, 2, var)
  cor_matrix <- cor(cor_data)
  cor_value <- cor_vector(cor_matrix)

  return(list(
    r2 = r2,
    beta = beta,
    mean = mean_value,
    var = var_value,
    cor = cor_value
  ))
}

univariate_stat_cor <- function(means_groups) {
  means_groups <- as.data.frame(do.call(rbind, means_groups))
  cor_matrix <- cor(means_groups)
  
  return(cor_matrix[1, 2])
}

univariate_stat_mean <- function(means_groups, ...) {
  return(mean(sapply(means_groups, mean)))
}

# Aggregate the fine-scale attributes up to the zone level.
#
# Columns are averaged over the areas making up each zone, except those named
# in `sum_vars`, which are summed.  Extensive quantities such as population
# counts belong in `sum_vars`: a zone's population is the number of people it
# contains, not the mean population of its tracts.
eval_stat_function <- function(data, id_groups, stat_function,
                               sum_vars = NULL, ...) {
  newdata_group <- lapply(id_groups, function(x) {
    if (length(x) == 0) {
      return()
    }
    data[x, , drop = FALSE]
  })
  newdata_group <- newdata_group[lengths(newdata_group) != 0]
  means_groups <- lapply(newdata_group, function(x) {
    x <- rbind(x)
    agg <- colMeans(x)
    if (length(sum_vars)) {
      agg[sum_vars] <- colSums(x)[sum_vars]
    }
    agg
  })
  return(do.call(
    stat_function,
    list(means_groups = means_groups, ...)
  ))
}

maup_skater_random <- function(edges, data, stat_function, n_cuts, ...) {
    n <- nrow(edges) + 1
    res <- list(groups = rep(1, n), edges.groups = list(list(node = 1:n,
                                                             edge = edges)),
                not.prune = NULL, candidates = 1)
    res$edges.groups[[1]]$edge = cbind(res$edges.groups[[1]]$edge,
                                       rnorm(nrow(res$edges.groups[[1]]$edge)))
    cuts <- length(res$edges.groups)
    res$candidates <- setdiff(1:length(res$edges.groups), res$not.prune)
    res$stat <- vector("list", n_cuts - 1)
    repeat {
        if (cuts > n_cuts)
            break
        if (length(res$candidates) == 0)
            break
        l.costs.ord <- lapply(res$edges.groups[res$candidates],
                              function(x) x$edge[, 3])
        t.id <- rep(res$candidates, sapply(l.costs.ord, length))
        t.cost <- unlist(l.costs.ord)
        t.idi <- unlist(lapply(l.costs.ord, function(x) {
            if (length(x) > 0)
                1:length(x)
            else NULL
        }))
        dc <- cbind(t.id, t.cost, t.idi)
        if (nrow(dc) > 1) dc <- dc[order(rnorm(nrow(dc))), ]
        toprun <- rbind(res$edges.groups[[dc[1, 1]]]$edge[dc[1, 3], 1:2],
                        res$edges.groups[[dc[1, 1]]]$edge[-dc[1, 3], 1:2])
        g.pruned <- prunemst(toprun, only.nodes = FALSE)
        groups <- lapply(res$edges.groups[-dc[1, 1]],
                         function(x) list(node = x$node,
                                          edge = x$edge))
        groups <- append(groups, g.pruned[1])
        groups <- append(groups, g.pruned[2])
        gc.pruned <- lapply(groups, function(e) {
            list(node = e$node,
                 edge = cbind(e$edge, rnorm(nrow(e$edge))))
        })
        res$edges.groups[[dc[1, 1]]] <- gc.pruned[[length(gc.pruned) - 1]]
        cuts <- cuts + 1
        res$edges.groups[[cuts]] <- gc.pruned[[length(gc.pruned)]]
        ids <- lapply(res$edges.groups, function(x) x$node)
        res$stat[[cuts - 1]] <- eval_stat_function(
            data = data,
            id_groups = ids,
            stat_function = stat_function,
            ...
            )
        res$candidates <- setdiff(1:length(res$edges.groups),
                                  res$not.prune)
    }
    for (i in 1:length(res$edges.groups)) res$groups[res$edges.groups[[i]]$node] <- i
    res$n_group <- unlist(lapply(ids, length))
    attr(res, "class") <- "skater"
    return(res)
}



cost_correlation_adversarial_identity <- function(data, id, id.neigh) {
  ret <- vector("list", length(id.neigh))
  k <- 1
  aux <- as.matrix(data)

  for (i in id.neigh) {        
      mean_xy_ij <- (aux[i, ] + aux[id, ]) / 2
      ret[[k]] <- abs(diff(mean_xy_ij))
      k <- k + 1
  }
  unlist(ret)
}

cost_correlation_adversarial_minus_identity <- function(data, id, id.neigh) {
  ret <- vector("list", length(id.neigh))
  k <- 1
  aux <- as.matrix(data)
  for (i in id.neigh) {
      mean_xy_ij <- (aux[i, ] + aux[id, ]) / 2
      ret[[k]] <- abs(sum(mean_xy_ij))
      k <- k + 1
  }
  unlist(ret)
}

stat_correlation_adversarial <- function (data, id_groups, type) {
    newdata_group <- lapply(id_groups, function(x) {
        if (length(x) == 0) return()
        data[x, ]
    })
    newdata_group <- newdata_group[lengths(newdata_group) != 0]
    means_group <- lapply(newdata_group, function(x) colMeans(rbind(x)))
    if (type == "positive") return(-cor(do.call(rbind, means_group))[1, 2])
    if (type == "negative") return(cor(do.call(rbind, means_group))[1, 2])
}

prunecost_corrrelation_first <- function(edges, data, type) {
  sapply(1:nrow(edges),
          function(i) {
              pruned.ids <- prunemst(
                rbind(edges[i, ], edges[-i, ]),
                only.nodes = TRUE
              )
              newdata_group <- lapply(pruned.ids, function(x) {
                data[x, ]
              })
              means_group <- lapply(
                newdata_group,
                function(x) colMeans(rbind(x))
              )
              da <- do.call(rbind, means_group)
              if (type == "positive") {
                return(-diff(c(da[1, 1], da[2, 1])) /
                      diff(c(da[1, 2], da[2, 2])))
              }
              if (type == "negative") {
                return(diff(c(da[1, 1], da[2, 1])) /
                      diff(c(da[1, 2], da[2, 2])))
              }
          })
}

# Root a zone's spanning tree at its first node and return, for every node,
# the sums of x and y and the node count of the subtree hanging below it.
# One O(k) pass replaces the per-edge prunemst() calls.
.subtree_sums <- function(loc1, loc2, m, xz, yz) {
    from <- c(loc1, loc2)
    to   <- c(loc2, loc1)
    ord  <- order(from)
    from <- from[ord]; to <- to[ord]
    start <- c(1L, cumsum(tabulate(from, m)) + 1L)

    parent <- integer(m)
    visit  <- integer(m)
    seen   <- logical(m)
    stack  <- integer(m)
    sp <- 1L; stack[1L] <- 1L; seen[1L] <- TRUE; nv <- 0L
    while (sp > 0L) {
        v <- stack[sp]; sp <- sp - 1L
        nv <- nv + 1L; visit[nv] <- v
        lo <- start[v]; hi <- start[v + 1L] - 1L
        if (lo <= hi) for (e in lo:hi) {
            w <- to[e]
            if (!seen[w]) {
                seen[w] <- TRUE; parent[w] <- v
                sp <- sp + 1L; stack[sp] <- w
            }
        }
    }

    sx <- xz; sy <- yz; cnt <- rep(1L, m)
    if (m >= 2L) for (i in m:2L) {
        v <- visit[i]; pa <- parent[v]
        sx[pa] <- sx[pa] + sx[v]
        sy[pa] <- sy[pa] + sy[v]
        cnt[pa] <- cnt[pa] + cnt[v]
    }
    list(parent = parent, sx = sx, sy = sy, cnt = cnt)
}

# Cost of pruning each candidate edge of a single zone.
#
# Pruning an edge inside a zone splits that zone in two and leaves every other
# zone untouched, so the Pearson correlation across zone means can be updated
# in O(1) per candidate edge from running moments over the unaffected zones.
# Rooting the zone's subtree once (see .subtree_sums) gives the x and y sums on
# either side of every edge, so the whole zone costs O(k + n) rather than the
# O(k * n) of rebuilding each candidate partition from the raw data.
prunecost_corrrelation <- function(edges, data, id_groups, type) {
    k <- nrow(edges)
    if (k == 0L) return(numeric(0))

    xv <- data[[1L]]
    yv <- data[[2L]]

    zone <- unique(c(edges[, 1L], edges[, 2L]))
    m <- length(zone)
    zone_head <- zone[1L]

    # Moments over the zones this cut does not touch.
    ng <- length(id_groups)
    ox <- numeric(ng); oy <- numeric(ng); nn <- 0L
    for (gi in seq_len(ng)) {
        g <- id_groups[[gi]]
        if (!length(g)) next
        if (any(g == zone_head)) next      # the zone being re-costed
        nn <- nn + 1L
        ox[nn] <- mean(xv[g])
        oy[nn] <- mean(yv[g])
    }
    if (nn > 0L) {
        ox <- ox[seq_len(nn)]; oy <- oy[seq_len(nn)]
        b_n <- nn
        b_sx <- sum(ox);      b_sy <- sum(oy)
        b_sxx <- sum(ox * ox); b_syy <- sum(oy * oy); b_sxy <- sum(ox * oy)
    } else {
        b_n <- 0L; b_sx <- 0; b_sy <- 0; b_sxx <- 0; b_syy <- 0; b_sxy <- 0
    }

    loc1 <- match(edges[, 1L], zone)
    loc2 <- match(edges[, 2L], zone)
    st <- .subtree_sums(loc1, loc2, m, xv[zone], yv[zone])

    # For each tree edge the child endpoint is the one whose parent is the other.
    child <- ifelse(st$parent[loc2] == loc1, loc2, loc1)

    tot_x <- st$sx[1L]; tot_y <- st$sy[1L]

    cA <- st$cnt[child]
    ax <- st$sx[child] / cA
    ay <- st$sy[child] / cA
    cB <- m - cA
    bx <- (tot_x - st$sx[child]) / cB
    by <- (tot_y - st$sy[child]) / cB

    G   <- b_n + 2L
    Sx  <- b_sx  + ax + bx
    Sy  <- b_sy  + ay + by
    Sxx <- b_sxx + ax * ax + bx * bx
    Syy <- b_syy + ay * ay + by * by
    Sxy <- b_sxy + ax * ay + bx * by

    cxx <- Sxx - Sx * Sx / G
    cyy <- Syy - Sy * Sy / G
    cxy <- Sxy - Sx * Sy / G
    den <- sqrt(cxx * cyy)

    r <- ifelse(is.finite(den) & den > 0, cxy / den, NA_real_)
    if (type == "positive") return(-r)
    r
}


maup_skater_adv <- function(edges, data, n_cuts, type) {
    n <- nrow(edges) + 1
    res <- list(groups = rep(1, n),
                edges.groups = list(list(node = 1:n, edge = edges[, 1:2, drop = FALSE])),
                candidates = 1)

    cost1 <- prunecost_corrrelation_first(
        res$edges.groups[[1]]$edge, data, type
    )
    res$edges.groups[[1]]$edge <- cbind(res$edges.groups[[1]]$edge, cost1)

    cuts <- length(res$edges.groups)
    res$candidates <- setdiff(seq_along(res$edges.groups), res$not.prune)
    ids <- lapply(res$edges.groups, function(x) x$node)

    repeat {
        if (cuts > n_cuts) break
        if (length(res$candidates) == 0) break

        # Locate the globally largest cost across all candidate zones.  A
        # linear scan suffices; only the single best edge is ever used.
        best_g <- NA_integer_
        best_j <- NA_integer_
        best_cost <- -Inf
        for (g in res$candidates) {
            cst <- res$edges.groups[[g]]$edge[, 3]
            if (!length(cst)) next
            j <- which.max(cst)
            if (cst[j] > best_cost) {
                best_cost <- cst[j]
                best_g <- g
                best_j <- j
            }
        }
        if (is.na(best_g)) break

        toprun <- rbind(res$edges.groups[[best_g]]$edge[best_j, 1:2],
                        res$edges.groups[[best_g]]$edge[-best_j, 1:2])
        g.pruned <- prunemst(toprun, only.nodes = FALSE)

        # Zone configuration as it stands after this split.
        id_groups <- lapply(res$edges.groups[-best_g], function(x) x$node)
        id_groups <- append(id_groups, list(g.pruned[[1]]$node))
        id_groups <- append(id_groups, list(g.pruned[[2]]$node))

        # Re-cost only the two subtrees created by this cut.  Zones left
        # untouched by the cut keep the edge costs they already carry, which
        # is what keeps the whole sweep O(n^2) rather than O(n^3).
        recost <- function(e) {
            if (nrow(e$edge) == 0) {
                return(list(node = e$node, edge = matrix(0, 0, 3)))
            }
            cst <- prunecost_corrrelation(
                edges = e$edge[, 1:2, drop = FALSE],
                data, id_groups = id_groups, type
            )
            list(node = e$node,
                 edge = cbind(e$edge[, 1:2, drop = FALSE], cst))
        }

        res$edges.groups[[best_g]] <- recost(g.pruned[[1]])
        cuts <- cuts + 1
        res$edges.groups[[cuts]] <- recost(g.pruned[[2]])

        ids <- lapply(res$edges.groups, function(x) x$node)
        res$stat_correlation_adversarial <- c(
            res$stat_correlation_adversarial,
            stat_correlation_adversarial(data, ids, type)
        )
        res$candidates <- setdiff(seq_along(res$edges.groups), res$not.prune)
    }

    for (i in seq_along(res$edges.groups)) res$groups[res$edges.groups[[i]]$node] <- i
    res$n_group <- unlist(lapply(ids, length))
    attr(res, "class") <- "skater"
    return(res)
}


# Total population and total land area of one county, taken over the tracts
# that survive the same filters the analysis applies: non-empty geometry, at
# least 20 tracts, and membership of the largest connected component of the
# adjacency graph.  Returns NULL for counties that do not qualify.
get_area <- function(dc) {
  if (nrow(dc) < 20) {
    return(NULL)
  }
  dc <- dc[!st_is_empty(dc), ]
  if (nrow(dc) < 20) {
    return(NULL)
  }
  dc_nb <- poly2nb(dc, snap = ADJ_SNAP)
  search <- n.comp.nb(dc_nb)
  idmax <- as.numeric(names(which.max(table(search$comp.id))))
  dc <- dc[search$comp.id == idmax, ]
  if (nrow(dc) < 20) {
    return(NULL)
  }
  # Total area of the retained tracts, which is the area of the county over
  # which the population below is spread.
  area <- sum(st_area(dc))

  pop <- sum(dc$estimate)
  area_km <- as.numeric(units::set_units(area, km^2))
  data.frame(pop = pop, area_km = area_km,
             fips = unique(dc$fips), county = unique(dc$County))
}
