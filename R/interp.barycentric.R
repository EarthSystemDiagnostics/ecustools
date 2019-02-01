#' ND barycentric interpolation at points Xi for a function with values f
#' measured at locations X
#'
#' @param X An n-by-d matrix. The rows of x represent n points in d-dimensional
#'   space.
#' @param f A vector of values nrow(X) long to be interpolated.
#' @param Xi An ni-by-d matrix. The rows of xi represent n points in
#'   d-dimensional space whose positions in the mesh are being sought.
#' @references Adapted from code here:
#'   https://dahtah.wordpress.com/2013/03/06/barycentric-interpolation-fast-interpolation-on-arbitrary-grids/
#'
#' @author Andrew Dolman <andrew.dolman@awi.de>
#' @return matrix
#' @import geometry Matrix
#' @export
#'
#' @examples
#' n <- 100
#' df <- data.frame(x = runif(n, -1, 1), y = runif(n, -1, 1), z = runif(n, -1, 1))
#' df$v <- with(df, 2*x + 1*y^2 + 3*z)
#' df.reg <- with(df, expand.grid(x = seq(min(x), max(x), length.out = 5),
#'                                y = seq(min(y), max(y), length.out = 5),
#'                                z = seq(min(z), max(z), length.out = 5)
#' ))
#'
#' out <- InterpBarycentric(X = df[,c(1,2,3)],
#'                           f = df$v,
#'                           Xi = df.reg)
#'
#' out <- as.data.frame(out)
#'
#' out$v_true <-  with(out, 2*x + 1*y^2 + 3*z)
#'
#' plot(v~v_true, data = out)
#' abline(0, 1)
#'
#' hist(out$v - out$v_true)
InterpBarycentric <- function(X, f, Xi) {
  X <- as.matrix(X)
  Xi <- as.matrix(Xi)
  dn <- geometry::delaunayn(X)
  tri <- geometry::tsearchn(X, dn, Xi, fast = TRUE)

  # points in Xi that fall inside polygons
  good.ind <- which(is.na(tri$idx) == FALSE)
  Xi.2 <- Xi[good.ind, ]
  tri <- geometry::tsearchn(X, dn, Xi.2, fast = TRUE)

  # For each line in Xi.2, defines which points in X contribute to the
  # interpolation
  active <- dn[tri$idx, ]
  # Define the interpolation as a sparse matrix operation. Faster than using apply,
  # probably slower than a C implementation
  M <- Matrix::sparseMatrix(i = rep(1:nrow(Xi.2), each = ncol(Xi.2) + 1), j = as.numeric(t(active)),
                            x = as.numeric(t(tri$p)), dims = c(nrow(Xi.2), length(f)))
  v <- as.numeric(M %*% f)

  out <- cbind(Xi, v = NA)
  out[good.ind, "v"] <- v
  return(out)
}
