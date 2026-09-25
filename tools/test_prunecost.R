# Correctness check for prunecost_corrrelation(), the incremental edge-cost
# routine used by the adversarial pruner.  The fast version updates the
# correlation across zone means in O(1) per candidate edge from cached moments;
# this compares it against a direct implementation that rebuilds every zone mean
# from the raw data for each edge.  Run from the top of the repository:
#
#   Rscript tools/test_prunecost.R
src <- readLines("00_functions.R")
i0 <- grep("^\\.subtree_sums <- function", src)
i1 <- grep("^maup_skater_adv <- function", src) - 1
eval(parse(text = paste(src[i0:i1], collapse = "\n")))

# components of a tree after deleting one edge (stand-in for prunemst)
comps <- function(nodes, e) {
  adj <- split(c(e[, 2], e[, 1]), c(e[, 1], e[, 2]))
  seen <- c(); out <- list()
  for (s in nodes) {
    if (s %in% seen) next
    stack <- s; cc <- c()
    while (length(stack)) {
      v <- stack[1]; stack <- stack[-1]
      if (v %in% cc) next
      cc <- c(cc, v); stack <- c(stack, adj[[as.character(v)]])
    }
    seen <- c(seen, cc); out[[length(out) + 1]] <- sort(cc)
  }
  out
}

ref_cost <- function(edges, data, id_groups, type) {
  zone <- unique(c(edges[, 1], edges[, 2]))
  sapply(seq_len(nrow(edges)), function(i) {
    cc <- comps(zone, edges[-i, , drop = FALSE])
    stopifnot(length(cc) == 2)
    aux <- lapply(id_groups, function(x) setdiff(setdiff(x, cc[[1]]), cc[[2]]))
    aux <- c(aux, cc[1], cc[2])
    aux <- aux[lengths(aux) > 0]
    mg <- do.call(rbind, lapply(aux, function(g) colMeans(data[g, , drop = FALSE])))
    r <- cor(mg)[1, 2]
    if (type == "positive") -r else r
  })
}

set.seed(7); worst <- 0; ncase <- 0
for (trial in 1:60) {
  n <- sample(12:60, 1)
  data <- as.data.frame(scale(matrix(rnorm(2 * n), n, 2)))
  names(data) <- c("x", "y")
  # random spanning tree on 1..n
  perm <- sample(n)
  edges <- cbind(perm[2:n], perm[sapply(2:n, function(i) sample(i - 1, 1))])
  # carve a zone out of the tree, rest of the map = other zones
  zone_nodes <- sort(unique(c(edges[, 1], edges[, 2])))
  keep <- sort(sample(zone_nodes, max(4, floor(n * runif(1, .4, .9)))))
  sub <- edges[edges[, 1] %in% keep & edges[, 2] %in% keep, , drop = FALSE]
  cc <- comps(keep, sub); zone <- cc[[which.max(lengths(cc))]]
  ze <- sub[sub[, 1] %in% zone & sub[, 2] %in% zone, , drop = FALSE]
  if (nrow(ze) < 2) next
  others <- setdiff(1:n, zone)
  id_groups <- if (length(others)) split(others, ceiling(seq_along(others) / 3)) else list()
  id_groups <- c(unname(id_groups), list(zone))
  for (ty in c("positive", "negative")) {
    a <- prunecost_corrrelation(ze, data, id_groups, ty)
    b <- ref_cost(ze, data, id_groups, ty)
    d <- max(abs(a - b), na.rm = TRUE)
    if (is.finite(d)) { worst <- max(worst, d); ncase <- ncase + 1 }
    stopifnot(identical(is.na(a), is.na(b)))
  }
}
cat(sprintf("cases compared : %d\nmax |fast - reference| : %.3e\n", ncase, worst))
cat(if (worst < 1e-10) "PASS\n" else "FAIL\n")
