## Helper functions for ggplot
 # Custom scales, themes, palettes etc.

#' @title Transform axis to reversed log scale
#' @description Custom axis transformation function
#' @param base base for the log transformation, Default: exp(1)
#' @seealso
#'  \code{\link[scales]{trans_new}},\code{\link[scales]{log_breaks}}
#' @rdname reverselog_trans
#' @source https://stackoverflow.com/a/11054781/1198125
#' @export
#' @importFrom scales trans_new log_breaks
reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  scales::trans_new(paste0("reverselog-", format(base)), trans, inv,
                    scales::log_breaks(base = base),
                    domain = c(1e-100, Inf))
}
