#' Base color class
#'
base.color <- setRefClass("base.color",
                          methods = list(
                            to = function(to){
                              if(to == "gray" | to == "grey")
                                color.convert.gray(.self)
                              else
                                color.convert(.self, to)
                            }
                          )
              )

test <- setRefClass("test")

#' RGB Class [0-1]
#'
#' @param r Red [0;1]
#' @param g Green [0;1]
#' @param b Blue [0;1]
#' @examples metallicseaweed <- rgb(r = 0.039, g = 0.494, b =  0.549)
#' @export
rgb <- setRefClass("rgb",
                   fields = list(
                     r = "numeric",
                     g = "numeric",
                     b = "numeric"
                   ),
                   methods = c(
                     initialize = function(r,g,b) {
                       if( 0 > r | r > 1 |
                           0 > g | g > 1 |
                           0 > b | b > 1) {
                         stop("rgb out of bounds")
                       } else {
                         r <<- r
                         g <<- g
                         b <<- b
                       }
                     }
                   ),
                   contains = "base.color")

#' RGB class [0-255]
#'
#' @param R Red [0;255]
#' @param G Green [0;255]
#' @param B Blue [0;255]
#' @examples midnightblue <- RGB(R = 25, G = 25, B = 112)
#' @export
RGB <- setRefClass("RGB",
                   fields = list(
                     R = "numeric",
                     G = "numeric",
                     B = "numeric"
                   ),
                   methods = c(
                     initialize = function(R,G,B) {
                       if( 0 > R | R > 255 |
                           0 > G | G > 255 |
                           0 > B | B > 255) {
                         stop("RGB out of bounds")
                       } else {
                         R <<- R
                         G <<- G
                         B <<- B
                       }
                     }
                   ),
                   contains = "base.color")

#' RGB Class [hex]
#'
#' @param hex RGB as hex string
#' @examples majorelle <- hex("#6050DC")
#' @examples majorelle <- hex("6050DC")
#' @export
hex <- setRefClass("hex",
                   fields = list(
                     hex = "character"
                    ),
                   methods = c(
                     initialize = function(hex) {
                       if(stringr::str_detect(hex, "^#?[a-fA-F0-9]{6}$")) {
                         # remove '#'´
                         if (nchar(hex) == 7)
                           hex <<- sub(".", "", hex)
                         else
                           hex <<- hex
                       }
                       else
                         stop("Not a valid hex code")
                     }
                   ),
                   contains = "base.color")


#' CMYK Class
#'
#' @param C Cyan
#' @param M Magenta
#' @param Y Yellow
#' @param K K?
#'
#' @export
CMYK <- setRefClass("CMYK",
                   fields = list(
                     C = "numeric",
                     M = "numeric",
                     Y = "numeric",
                     K = "numeric"
                   ),
                   methods = c(
                     initialize = function(C,M,Y,K) {
                      C <<- C
                      M <<- M
                      Y <<- Y
                      K <<- K
                     }
                   ),
                   contains = "base.color")


#' XYZ Class
#'
#' The CIE 1931 color spaces are the first defined quantitative links between
#' distributions of wavelengths in the electromagnetic visible spectrum,
#' and physiologically perceived colors in human color vision. (Wikipedia)
#'
#' @param X X
#' @param Y Y
#' @param Z Z
#' @example brickred <- XYZ(X = 0.2022, Y = 0.1043, Z = 0.009478)
#' @export
XYZ <- setRefClass("XYZ",
                   fields = list(
                     X = "numeric",
                     Y = "numeric",
                     Z = "numeric"
                   ),
                   methods = c(
                     initialize = function(X,Y,Z) {
                       X <<- X
                       Y <<- Y
                       Z <<- Z
                     }
                   ),
                   contains = "base.color")

#' Yxy Class
#'
#' @export
Yxy <- setRefClass("Yxy",
                   fields = list(
                     Y = "numeric",
                     x = "numeric",
                     y = "numeric"
                   ),
                   methods = c(
                     initialize = function(Y,x,y) {
                       Y <<- Y
                       x <<- x
                       y <<- y
                     }
                   ),
                   contains = "base.color")


#' CieLuv Class
#'
#' A color space adopted by the International Commission on Illumination (CIE)
#' in 1976, as a simple-to-compute transformation of the 1931 CIE XYZ color space.
#'
#' @param L Luminance [0;100]
#' @param u Green-Red axis [-100;+100]
#' @param v Blue-Purple axis [-100;+100]
#' @export
CieLuv <- setRefClass("CieLuv",
                      fields = list(
                        L = "numeric",
                        u = "numeric",
                        v = "numeric"
                      ),
                      methods = c(
                        initialize = function(L,u,v) {
                          L <<- L
                          u <<- u
                          v <<- v
                        }
                      ),
                      contains = "base.color")

#' Reference class for Cielab color
#'
#' @examples hexaorange <- CieLab(L = 68.44, a = 48.57, b = 71.13)
#' @param L Luminance [0;100]
#' @param a Green-Red axis [-128;+127]
#' @param b Blue-Yellow axis [-128;+127]
#' @export
CieLab <- setRefClass("CieLab",
                      fields = list(
                        L = "numeric",
                        a = "numeric",
                        b = "numeric"
                      ),
                      methods = c(
                        initialize = function(L,a,b) {
                          L <<- L
                          a <<- a
                          b <<- b
                        }
                      ),
                      contains = "base.color")

#' Reference class for CieLCh color
#'
#' @param L Luminance [0;100]
#' @param C Chroma [-128;+127]
#' @param h hue [-128;+127]
#' @export
CieLCh <- setRefClass("CieLCh",
                      fields = list(
                        L = "numeric",
                        C = "numeric",
                        h = "numeric"
                      ),
                      methods = c(
                        initialize = function(L,C,h) {
                          L <<- L
                          C <<- C
                          h <<- h
                        }
                      ),
                      contains = "base.color")

#' Reference class for HSL color
#'
#' An alternative representations of the RGB color model.
#' @param H Hue [0;360]
#' @param S Saturation [0;1]
#' @param L Lightness [0;1]
#' @export
HSL <- setRefClass("HSL",
                   fields = list(
                     H = "numeric",
                     S = "numeric",
                     L = "numeric"
                   ),
                   methods = c(
                     initialize = function(H,S,L) {
                       H <<- H
                       S <<- S
                       L <<- L
                     }
                   ),
                   contains = "base.color")

#' Reference class for HSV color
#'
#' An alternative representations of the RGB color model.
#' @param H Hue [0;360]
#' @param S Saturation [0;1]
#' @param V Value (Brightness) [0;1]
#' @export
HSV <- setRefClass("HSV",
                   fields = list(
                     H = "numeric",
                     S = "numeric",
                     V = "numeric"
                   ),
                   methods = c(
                     initialize = function(H,S,V) {
                       H <<- H
                       S <<- S
                       V <<- V
                     }
                   ),
                   contains = "base.color")
