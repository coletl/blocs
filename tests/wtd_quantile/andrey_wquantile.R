# Andrey weighted quantile functions
# source: https://aakinshin.net/posts/weighted-quantiles/

# generic CDF
wquantile.generic <- function(x, probs, cdf.gen, weights = NA) {
    n <- length(x)
    if (any(is.na(weights)))
        weights <- rep(1 / n, n)
    nw <- sum(weights)^2 / sum(weights^2) # Kish's effective sample size

    indexes <- order(x)
    x <- x[indexes]
    weights <- weights[indexes]

    weights <- weights / sum(weights)
    cdf.probs <- cumsum(c(0, weights))

    sapply(probs, function(p) {
        cdf <- cdf.gen(nw, p)
        q <- cdf(cdf.probs)
        w <- tail(q, -1) - head(q, -1)
        sum(w * x)
    })
}

# Weighted Harrell-Davis quantile estimator
whdquantile <- function(x, probs, weights = NA) {
    cdf.gen <- function(n, p) return(function(cdf.probs) {
        pbeta(cdf.probs, (n + 1) * p, (n + 1) * (1 - p))
    })
    n <- length(x)
    if (any(is.na(weights)))
        weights <- rep(1 / n, n)
    nw <- sum(weights)^2 / sum(weights^2) # Kish's effective sample size

    indexes <- order(x)
    x <- x[indexes]
    weights <- weights[indexes]

    weights <- weights / sum(weights)
    cdf.probs <- cumsum(c(0, weights))

    sapply(probs, function(p) {
        cdf <- cdf.gen(nw, p)
        q <- cdf(cdf.probs)
        w <- tail(q, -1) - head(q, -1)
        sum(w * x)
    })
}

# Weighted Type 7 quantile estimator
wquantile <- function(x, probs, weights = NA) {
    cdf.gen <- function(n, p) return(function(cdf.probs) {
        h <- p * (n - 1) + 1
        u <- pmax((h - 1) / n, pmin(h / n, cdf.probs))
        u * n - h + 1
    })
    wquantile.generic(x, probs, cdf.gen, weights)
}
