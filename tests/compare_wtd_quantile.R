# Andrey functions ----
# source: https://aakinshin.net/posts/weighted-quantiles/
# Weighted generic quantile estimator
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
    wquantile.generic(x, probs, cdf.gen, weights)
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

#### Testing ####
# Code adapted from Michael Chirico:
# https://stats.stackexchange.com/questions/373269/why-does-this-simple-weighted-quantile-differ-from-hmiscwtd-quantile-which-me

set.seed(3049)
p = seq(0, 1, length.out = 100)
# png('~/Desktop/wtd_quantile.png', width = 1920, height = 1920, res = 100)
par(mfrow = c(2, 2), mar = c(0, 0, 0, 0), oma = c(5.1, 4.1, 4.1, 2.1))


for (nn in 10^(2:5)) {
    x = rnorm(nn)
    w = rchisq(nn, df = ceiling(abs(x^3)))

    add_x = nn %in% c(1000, 10000)
    add_y = nn %in% c(10, 1000)
    matplot(p, cbind(sapply(p, blocs::wtd_quantile, x = x, weight = w),
                     sapply(p, Hmisc::wtd.quantile, x = x, w = w),
                     # sapply(p, wquantile.generic, x = x, w = w),
                     sapply(p, whdquantile, x = x, w = w),
                     sapply(p, wquantile, x = x, w = w),
                     sapply(p, quantile, x = x)),
            xaxt = if (!add_x) 'n', xlab = '',
            yaxt = if (!add_y) 'n', ylab = '',
            type = 'l', lty = 1L, lwd = 2L, las = 1L, main = '')
    title(line = -1, sprintf('n = %s', prettyNum(nn, big.mark = ',')))
    if (add_x) mtext(side = 1L, 'Quantile', line = 3)
    if (add_y) mtext(side = 2L, 'Inverse CDF', line = 3)
    legend('topleft', col = 1:3, lwd = 2L,
           legend = c('blocs Weighted', 'Hmisc::wtd.quantile',
                      "andrey HD", "andrey type7",
                      'Unweighted'))
}
title('Comparison of Weighted Quantile Methods\nVarious Sample Sizes',
      outer = TRUE)
dev.off()

