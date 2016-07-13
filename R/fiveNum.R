# Five Number Summary Function
# Uses stats::fivenum and then names the entries.
fiveNum <- function (x, na.rm = TRUE) {
  xna <- is.na(x)
  if (any(xna)) {
    if (na.rm)
      x <- x[!xna]
    else return(rep.int(NA, 5))
  }
  x <- sort(x)
  n <- length(x)
  if (n == 0)
    rep.int(NA, 5)
  else {
    n4 <- floor((n + 3)/2)/2
    d <- c(1, n4, (n + 1)/2, n + 1 - n4, n)
    summ <- 0.5 * (x[floor(d)] + x[ceiling(d)])
    names(summ) <- c("min", "Q1", "Med", "Q3", "max")
    summ
  }
}
