#' Reference class for lab color
#'
#' @examples lab <- Lab(L = 50, a = 2.6772, b = -79.7751)
#' @export
Lab <- setRefClass("Lab",
                   fields = list(
                     L = "numeric",
                     a = "numeric",
                     b = "numeric"
                   ))

#' RGB Class
#' @export
rgb <- setRefClass("rgb",
                   fields = list(
                     r = "numeric",
                     g = "numeric",
                     b = "numeric"
                   ))

#' RGB class
#' @export
RGB <- setRefClass("RGB",
                   fields = list(
                     R = "numeric",
                     G = "numeric",
                     B = "numeric"
                   ))
