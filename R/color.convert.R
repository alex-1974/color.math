# See: https://www.easyrgb.com/en/math.php

#library(color.math)
#' Convert RGB to rgb
RGB2rgb <- function(RGB) {
  return(rgb(
    r = RGB$R / 255,
    g = RGB$G / 255,
    b = RGB$B / 255))
}

#' Convert rgb to RGB
rgb2RGB <- function(rgb) {
  return(RGB(
    R = rgb$r * 255,
    G = rgb$g * 255,
    B = rgb$b * 255))
}

dec2hex <- function(dec, n = 2) {
  if (dec == 0)
    hex <- c(0)
  else
    hex <- dec2hex.impl(dec)
  result <- paste(hex, collapse = "")
  if (length(hex) %% n == 0) {
    return (result)
  } else {
    pre <- paste(rep(0, (n - length(hex) %% n)), collapse = "")
    return(paste0(pre, result))
  }

}
dec2hex.impl <- function (dec, rem = c()) {
  hex <- c(0:9, "a", "b", "c", "d", "e", "f")
  if (dec > 0)
    return ( dec2hex.impl(dec%/%16, c(rem, hex[dec%%16 + 1])) )
  else {
    return (rev(c(rem)))
  }
}

#' Convert RGB2hex
RGB2hex <- function(RGB) {
  R <- dec2hex(RGB$R)
  G <- dec2hex(RGB$G)
  B <- dec2hex(RGB$B)
  return( paste0(c("#", R, G, B), collapse = ""))
}

#' Convert hex to rgb
#'
#' @param hex Hex code
#' @examples blue <- hex("aabbcc")
#' hex2RGB(blue)
#' hex2RGB("aabbcc")
#' hex2RGB("#aabbcc")
#' @export
#'
hex2RGB <- function(hex) {
  if (is.character(hex)) { hex <- hex(hex) }
  if (class(hex) != "hex") {
    warning("Not a valid hex code")
    return(NULL)
  }
  col <- strsplit(hex$hex, "")[[1]]
  col <- paste0(col[c(TRUE, FALSE)], col[c(FALSE, TRUE)])
  col <- strtoi(col, base = 16)
  result <- RGB(
    R = col[1],
    G = col[2],
    B = col[3])
  return (result)
}


#' Convert rgb to XYZ
#'
#' @examples orchid <- rgb(r = 0.8549, g = 0.4392, b = 0.8392)
#' xyz <- rgb2XYZ(orchid)
#' @export
rgb2XYZ <- function(rgb) {
  if (rgb$r > 0.04045)
    r = ( (rgb$r + 0.055) / 1.055 )^2.4
  else
    r = rgb$r / 12.92
  if (rgb$g > 0.04045)
    g = ( (rgb$g + 0.055) / 1.055 )^2.4
  else
    g = rgb$g / 12.92
  if (rgb$b > 0.04045)
    b = ( (rgb$b + 0.055) / 1.055 )^2.4
  else
    b = rgb$b / 12.92
  r = r * 100
  g = g * 100
  b = b * 100
  result <- XYZ(
    X = r * 0.4124 + g * 0.3576 + b * 0.1805,
    Y = r * 0.2126 + g * 0.7152 + b * 0.0722,
    Z = r * 0.0193 + g * 0.1192 + b * 0.9505
  )
  return(result)
}

#' Convert XYZ to rgb
#'
#' @examples orchid <- XYZ(X = 46.84411, Y = 31.34764, Z = 67.19739)
#' rgb <- XYZ2rgb(orchid)
#' @export
XYZ2rgb <- function(XYZ) {
  X = XYZ$X / 100
  Y = XYZ$Y / 100
  Z = XYZ$Z / 100
  r = X *  3.2406 + Y *  1.5372 + Z * -0.4986
  g = X * -0.9689 + Y *  1.8758 + Z *  0.0415
  b = X *  0.0557 + Y * -0.2040 + Z *  1.0570

  if (r > 0.0031308)
    r = 1.055 * (r^(1/2.4)) - 0.055
  else
    r = 12.92 * r
  if (g > 0.0031308)
    g = 1.055 * (g^(1/2.4)) - 0.055
  else
     g= 12.92 * g
  if (b > 0.0031308)
      b = 1.055 * (b^(1/2.4)) - 0.055
  else
    b = 12.92 * b
  return (rgb(r = r, g = g, b = b))
}

#' Convert XYZ to CieLab
XYZ2CieLab <- function(XYZ, ref = std_illuminant) {
  X = XYZ$X / ref[["X"]]
  Y = XYZ$Y / ref[["Y"]]
  Z = XYZ$Z / ref[["Z"]]

  if(X > 0.008856)
    X = X^(1/3)
  else
    X = (7.787 * X) + (16 / 116)
  if(Y > 0.008856)
    Y = Y^(1/3)
  else
    Y = (7.787 * Y) + (16 / 116)
  if(Z > 0.008856)
    Z = Z^(1/3)
  else
    Z = (7.787 * Z) + (16 / 116)

  L = (116 * Y) - 16
  a = 500 * (X - Y)
  b = 200 * (Y - Z)
  return( CieLab(L = L, a = a, b = b))
}

#' Convert CieLab to XYZ
CieLab2XYZ <- function(CieLab, ref = std_illuminant) {
  Y = (CieLab$L + 16) / 116
  X = CieLab$a / 500 + Y
  Z = Y - CieLab$b / 200
  if (Y^3 > 0.008856)
    Y = Y^3
  else Y = (Y - 16 / 116) / 7.787
  if (X^3 > 0.008856)
    X = X^3
  else
    X = (X - 16 / 116) / 7.787
  if (Z^3 > 0.008856)
    Z = Z^3
  else
    Z= (Z - 16 / 116) / 7.787
  return( XYZ(
    X = X * ref["X"],
    Y = Y * ref["Y"],
    Z = Z * ref["Z"]
  ))
}

#' Convert XYZ to CieLuv
XYZ2CieLuv <- function(XYZ, ref = std_illuminant) {
  U = (4 * XYZ$X) / (XYZ$X + (15 * XYZ$Y) + (3 * XYZ$Z))
  V = (9 * XYZ$Y) / (XYZ$X + (15 * XYZ$Y) + (3 * XYZ$Z))
  Y = XYZ$Y / 100
  if (Y > 0.008856)
    Y = Y^(1/3)
  else
    Y = (7.787 * Y) + (16/116)
  refU = ( 4 * ref[["X"]] ) / ( ref[["X"]] + ( 15 * ref[["Y"]] ) + ( 3 * ref[["Z"]] ) )
  refV = ( 9 * ref[["Y"]] ) / ( ref[["X"]] + ( 15 * ref[["Y"]] ) + ( 3 * ref[["Z"]] ) )

  L = (116 * Y) - 16
  u = 13 * L * (U - refU)
  v = 13 * L * (V - refV)

  return(CieLuv(L = L, u = u, v = v))
}

#' Convert CieLuv to XYZ
CieLuv2XYZ <- function(CieLuv, ref = std_illuminant) {
  Y = (CieLuv$L + 16) / 116
  if (Y^3 > 0.008856)
    Y = Y^3
  else
    Y = (Y - 16 / 116) / 7.787
  refU = ( 4 * ref[["X"]] ) / ( ref[["X"]] + ( 15 * ref[["Y"]] ) + ( 3 * ref[["Z"]] ) )
  refV = ( 9 * ref[["Y"]] ) / ( ref[["X"]] + ( 15 * ref[["Y"]] ) + ( 3 * ref[["Z"]] ) )
  U = CieLuv$u / ( 13 * CieLuv$L ) + refU
  V = CieLuv$v / ( 13 * CieLuv$L ) + refV
  return(XYZ(
    Y= Y * 100,
    X = - ( 9 * Y * U ) / ( ( U - 4 ) * V - U * V ),
    Z = ( 9 * Y - ( 15 * V * Y ) - ( V * X ) ) / ( 3 * V )
  ))
}

#' Convert XYZ to Yxy
XYZ2Yxy <- function(XYZ) {
  return(Yxy(
    Y = XYZ$Y,
    x = XYZ$X / (XYZ$X + XYZ$Y + XYZ$Z),
    y = XYZ$Y / (XYZ$X + XYZ$Y + XYZ$Z)
  ))
}

#' Convert Yxy to XYZ´
Yxy2XYZ <- function(Yxy) {
  return(XYZ(
    X = Yxy$x * (Yxy$Y / Yxy$y),
    Y = Yxy$Y,
    Z = (1 - Yxy$x - Yxy$y) * (Yxy$Y / Yxy$y)
  ))
}

#' Convert rgb to HSL
rgb2HSL <- function(rgb) {
  min = min(rgb$r, rgb$g, rgb$b) # min value of rgb
  max = max(rgb$r, rgb$g, rgb$b) # max value of rgb
  dMax = max - min               # delta rgb value
  L = (max + min) / 2
  if (dMax == 0) {               # this is gray, no chroma...
    H = 0
    S = 0
  } else {                       # chromatic data...
    if (L < 0.5)
      S = dMax / (max + min)
    else
      S = dMax / (2 - max - min)
    dR = ((( max - rgb$r) / 6) + (dMax / 2)) / dMax
    dG = ((( max - rgb$g) / 6) + (dMax / 2)) / dMax
    dB = ((( max - rgb$b) / 6) + (dMax / 2)) / dMax

    if (rgb$r == max)
      H = dB - dG
    else if (rgb$g == max)
      H = (1/3) + dR - dB
    else if (rgb$b == max)
      H = (2/3) + dG - dR

    if (H < 0) H = H + 1
    if (H > 0) H = H - 1
  }
  result <- HSL(
    H = H,
    S = S,
    L = L
  )
  return(result)
}

#' Convert HSL to rgb
HSL2rgb <- function(HSL) {
  if (S ==0) {
    r = HSL$L
    g = HSL$L
    b = HSL$L
  } else {
    if (L < 0.5)
      v2 = HSL$L * (1 + HSL$S)
    else
      v2 = (HSL$L + HSL$S) - (HSL$S *  HSL$L)
    v1 = 2 * HSL$L - v2

    r = hue2rgb(v1,v2,(H+(1/3)))
    g = hue2rgb(v1,v2,H)
    b = hue2rgb(v1,v2,(H-(1/3)))
  }
  result <- rgb(
    r = r,
    g = g,
    b = b
  )
  return(result)
}

#' Helper function for converting hue to rgb (HSL2rgb)
hue2rgb <- function(v1, v2, vH) {
  if (vH < 0) vH = vH + 1
  if (vH > 1) vH = vH - 1
  if ( (6 * vH) < 1)
    return(v1 + (v2 - v1) * 6 * vH)
  if ( (2 * vH) < 1)
    return(v2)
  if ( (3 * vH) < 2)
    return(v1 + (v2 - v1) * ((2/3) - vH) * 6)
  return(v1)
}

illuminant.table <- readr::read_csv("data/observer_illuminant.csv",
                              col_types = readr::cols(
                                Illuminant = readr::col_character(),
                                note = readr::col_character(),
                                .default = readr::col_number()
                              )) |>
  dplyr::mutate(
    Illuminant = as.factor(stringr::str_trim(Illuminant))
  ) |>
  dplyr::select(!note) |>
  tidyr::pivot_longer(!Illuminant,
                      names_to = c(".value", "angle"),
                      names_pattern = "([XYZ])([0-9]+)"
  ) |>
  dplyr::mutate(
    angle = as.factor(angle)
  )

#' Convenience function to get observer illuminant by name and angle
#'
#' @param i illuminant
#' @param a angle
#' @example get_illuminant("D55", 10)
get_illuminant <- function(i, a = 10) {
  result <- illuminant.table |>
    dplyr::filter(
      Illuminant == i, angle == a
    )
  return(c(
    X = result$X,
    Y = result$Y,
    Z = result$Z))
}
std_illuminant <- get_illuminant("D55", 10)

#attach(color.math)
# Get names of all functions which convert colorspaces

objs <- as.vector(lsf.str(pattern = "^[a-zA-Z]+2[a-zA-Z]+$")) |>
  unlist() |>
  stringr::str_split("2",2)

library(rlang)

#' Node environment
#'
#' @param color Color of the node
#' @param from Previous color
#' @param to List of colors we can get to from here
#' @export
node <- function(color = NULL, from = NULL, to = NULL) {
  result <- new.env()
  result$color <- color
  result$to <- to
  result$from <- from
  return(result)
}

# Get all paths from given color space as list of characters
step <- function(from) {
  result <- objs |>
    purrr::keep(function(x) x[1] == from ) |>  # keep paths which start from (first index)
    sapply("[[", 2) # subset to part (second index)
  return(result)
}

get.path <- function(node, path = NULL) {
  if (!is.null(node$from)) {
    return( get.path(node$from, c(path, node$color)) )
  } else {
    return (c(path, node$color))
  }
}


#' Implements recursive path function
#'
#' @param to To as environment
#' @param queue Vector of environments as FIFO-queue for breadth-first search
#' @returns Conversion path as vector of characters (eg. c("RGB", "rgb", "XYZ", "CieLuv") )
path.impl <- function(to, queue) {
  # init vars
  queue.new <- c()
  last.loop <- FALSE
  results <- c()
  # loop the queue
  while(length(queue) & !last.loop) {
    head <- queue[[1]] # just convenience
    head.paths <- get.path(head) # get all paths found from head
    # von head alle möglichen wege als pfadliste p
    p <- step(head$color)
    # checke pfadliste und eliminiere unnötige
    # recursive pfade?
    for (i in 1:length(p)) {
      if (p[i] %in% head.paths)
        p <- p[-i]
    }

    # die überbleibenden: checke auf treffer
    if (to %in% p) {
      results <- c(results, c(rev(get.path(head)), to))
      last.loop <- TRUE
    }
    # transform pfadliste in liste von environments
    if (length(p) > 0) {
      p.env <- lapply(p, function(x) node(color = x, from = head)) # pfadliste als environments
      # append queue
      queue.new <- c(queue.new, p.env)
      # path in to
      queue[[1]]$to <- p.env
    }

    # reduziere queue
    queue <- queue[-1]

  }
  if (last.loop) {
    # we got at least one match
    return(results)
  } else if (!length(queue.new)) {
    # we didn't got a match
    return(NULL)
  } else {
    # next step
    if(length(queue.new))
      return (path.impl(to, queue.new))
  }
}

#' Find all possile paths between from and to
#'
#' @param from From colorspace as color class
#' @param to Colorspace as character
#' @returns Character list of path from-to
#' @example color.convert("hex", "CieLuv")
#' @export
color.convert <- function(from, to) {
  # if from is to return
  if(class(from) == to) return (from)
  head = new.env()
  head$color <- class(from)

  p <- path.impl(to, c(head))
  print(p)
  code <- ""
  r <- NULL

  for(i in 2:length(p)-1) {
    #print(paste0(p[i], "2", p[i+1]))
    #code.string <- rlang::parse_expr(paste0(p[i], "2", p[i+1], "(from)"))
    #print(is.call(code.string))
    #rlang::expr_print(code.string)
    #print(lobstr::ast(!!code.string))
    if (i < 2) {
      code.string <- rlang::parse_expr(paste0(p[i], "2", p[i+1]))
      print(code.string)
      print(is.call(code.string))
      r <- rlang::exec(code.string, from)
      print(r)
    } else {
      code.string <- rlang::parse_expr(paste0(p[i], "2", p[i+1]))
      print(code.string)
      print(is.call(code.string))
      r <- rlang::exec(code.string, r)
      print(r)
    }
  }
  return (r)

  #c <- rlang::parse_expr(code)
  #print(c)
  #print(is.call(c))
}

hex1 <- hex("#aabbcc")
c <- color.convert(hex1, "RGB")
print(c)
c2 <- color.convert(c, "hex")
print(c2)

color.convert.gray <- function(from, to = class(from)) {
  if (class(from) == "hex")       { return( color.convert.gray(hex2RGB(from), to) ) }
  else if (class(from) == "RGB")  { return( color.convert.gray(RGB2rgb(from), to) ) }

  if (class(from) == "rgb") {
    from$r <- from$r * 0.2989  # red
    from$g <- from$g * 0.5870  # green
    from$b <- from$b * 0.1140  # blue
    print(to)
   # return(from$to(to))
  }
}

#tmp <- hex("#aabbcc")
# tmp$to("gray")
