si_powers <- sort(c(
  "Y"      =  24L,
  "Z"      =  21L,
  "E"      =  18L,
  "P"      =  15L,
  "T"      =  12L,
  "G"      =   9L,
  "M"      =   6L,
  "k"      =   3L,
  "h"      =   2L,
  "da"     =   1L,
               0L,
  "d"      =  -1L,
  "c"      =  -2L,
  "m"      =  -3L,
  "\u00b5" =  -6L,
  "n"      =  -9L,
  "p"      = -12L,
  "f"      = -15L,
  "a"      = -18L,
  "z"      = -21L,
  "y"      = -24L
))

si_powers_of <- function(base) {
  si_powers[rowSums(1 - sign(outer(si_powers, log10(base), `%%`))) > 0]
}

#' SI scale factors and unit prefixes
#' @seealso [`number()`] for formatting numbers
#' @param x A numeric vector.
#' @param base A numeric vector of bases whose SI powers are included.
#' @return `si_scale()`: A numeric vector of SI scaling factors.
#' @export
#' @examples
#' # scaling factors
#' si_scale(10^(-6:6))
#' si_scale(10^(-6:6), base = 1000)
si_scale <- function(x, base = 10) {
  powers <- si_powers_of(base)

  i <- findInterval(abs(x), 10^powers)
  power <- unname(powers)[pmax(i, 1L)]
  power[is.infinite(x) | x == 0] <- 0L

  10^power
}

#' @rdname si_scale
#' @inheritParams si_scale
#' @return `si_prefix()`: A character vector of SI unit prefixes.
#' @export
#' @examples
#'
#' # unit prefixes
#' si_prefix(10^(-6:6))
#' si_prefix(10^(-6:6), base = 1000)
#' si_prefix(10^(-6:6), base = c(100, 1000))
si_prefix <- function(x, base = 10) {
  powers <- si_powers_of(base)

  i <- findInterval(abs(x), 10^powers)
  prefix <- names(powers)[pmax(i, 1L)]
  prefix[is.infinite(x) | x == 0] <- ""

  prefix
}
