#' Convert decimal to hex
#'
#' @param dec Decimal number
#' @param n Padding
#' @example dec2hex(255)
dec2hex <- function(dec, n = 2) {
  if (dec == 0)
    hex <- c(0)
  else
    hex <- .dec2hex.impl(dec)
  result <- paste(hex, collapse = "")
  if (length(hex) %% n == 0) {
    return (result)
  } else {
    pre <- paste(rep(0, (n - length(hex) %% n)), collapse = "")
    return(paste0(pre, result))
  }

}

#' Recursive implementation of dec2hex
.dec2hex.impl <- function (dec, rem = c()) {
  hex <- c(0:9, "a", "b", "c", "d", "e", "f")
  if (dec > 0)
    return ( .dec2hex.impl(dec%/%16, c(rem, hex[dec%%16 + 1])) )
  else {
    return (rev(c(rem)))
  }
}

# Read illuminant data from csv file
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

# Standard illuminant in CIE colorspaces
std_illuminant <- get_illuminant("D55", 10)


library(rlang)

#' Node environment
#'
#' @param color Color of the node
#' @param from Previous color
#' @param to List of colors we can get to from here
#' @export
.node <- function(color = NULL, from = NULL, to = NULL) {
  result <- new.env()
  result$color <- color
  result$to <- to
  result$from <- from
  return(result)
}

# Get all paths from given color space as list of characters
.step <- function(from) {
  if (!length(objs)) stop("No functions for conversions found!")
  result <- objs |>
    purrr::keep(function(x) x[1] == from ) |>  # keep paths which start from (first index)
    sapply("[[", 2) # subset to part (second index)
  return(result)
}

# Get path from this node upwards (recursive)
.get.path <- function(node, path = NULL) {
  if (!is.null(node$from)) {
    return( .get.path(node$from, c(path, node$color)) )
  } else {
    return (c(path, node$color))
  }
}


#' Implements recursive path function
#'
#' @param to To as environment
#' @param queue Vector of environments as FIFO-queue for breadth-first search
#' @returns Conversion path as vector of characters (eg. c("RGB", "rgb", "XYZ", "CieLuv") )
.path.impl <- function(to, queue) {
  # init vars
  queue.new <- c()
  last.loop <- FALSE
  results <- c()
  # loop the queue
  while(length(queue) & !last.loop) {
    head <- queue[[1]] # just convenience
    head.paths <- .get.path(head) # get all paths found from head
    # von head alle möglichen wege als pfadliste p
    p <- .step(head$color)
    # checke pfadliste und eliminiere unnötige
    # recursive pfade?
    for (i in 1:length(p)) {
      if (p[i] %in% head.paths)
        p <- p[-i]
    }

    # die überbleibenden: checke auf treffer
    if (to %in% p) {
      results <- c(results, c(rev(.get.path(head)), to))
      last.loop <- TRUE
    }
    # transform pfadliste in liste von environments
    if (length(p) > 0) {
      p.env <- lapply(p, function(x) .node(color = x, from = head)) # pfadliste als environments
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
      return (.path.impl(to, queue.new))
  }
}

#' Find all possile paths between from and to
#'
#' @param from From colorspace as color class
#' @param to Colorspace as character
#' @returns Character list of path from-to
#' @examples
#'  majorelle <- hex(#6050dc)
#'  color.convert(majorelle, "CieLuv")
#' @export
color.convert <- function(from, to) {
  message(paste0("Convert ", class(from), " to ", to))
  # if from is to return
  if(class(from) == to) return (from)
  head = new.env()
  head$color <- class(from)

  p <- .path.impl(to, c(head))
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
      r <- rlang::exec(code.string, from)
    } else {
      code.string <- rlang::parse_expr(paste0(p[i], "2", p[i+1]))
      r <- rlang::exec(code.string, r)
    }
  }
  return (r)

  #c <- rlang::parse_expr(code)
  #print(c)
  #print(is.call(c))
}

majorelle <- hex("#6050dc")
color.convert(majorelle, "CieLuv")

color.convert.gray <- function(from) {
  #origin <- class(from)
  col <- from$to("rgb")
  r <- col$r * 0.2989  # red
  g <- col$g * 0.5870  # green
  b <- col$b * 0.1140  # blue
  c <- r + g + b
  col$r <- c
  col$g <- c
  col$b <- c
  #print(col)
  if (class(from) == "rgb")
    return(col)
  else
    return(col$to(class(from)))
}
majorelle <- hex("#6050dc")
print(majorelle)
color.convert.gray(majorelle)

