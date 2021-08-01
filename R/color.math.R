#' Get deltaE of two colors
#'
#' The calculation is performed in the CieLab color space.
#' @param hexA Color A hex value
#' @param hexB Color B hex value
#'
#' @examples deltaE("easternblue", "coral")
#'
deltaE76 <- function(hexA, hexB) {
  labA = hexA$to("CieLab")
  labB = hexB$to("CieLab")
  l = ( (labA$L - labB$L) ) ^ 2
  a = ( (labA$a - labB$a) ) ^ 2
  b = ( (labA$b - labB$b) ) ^ 2
  return (sqrt(l + a + b))
}

#' Convert degree to radius
#'
#' @param Arc in degree
#' @returns Arc in radians
#'
#' @examples deg2rad(180)
#'
deg2rad <- function(deg) { return (deg * (pi / 180)) }

#' Convert radius to degree
#'
#' @param rad Arc in radians
#' @returns Arc in degrees
#'
#' @examples rad2deg(3.14)
#'
rad2deg <- function(rad) { return ((180 / pi) * rad) }

#' Get deltaE of two colors
#'
#' The calculation is performed in the CieLab color space.
#' @param col1 First color
#' @param col2 Second color
#' @param k Correctur ("default", "screen", "textil")
#'
#' See https://github.com/gfiumara/CIEDE2000
deltaE2000 <- function(col1, col2, k = "default") {
  lab1 = col1$to("CieLab")
  lab2 = col2$to("CieLab")
  if (k == "default") {
    k_L = 1.0
    k_C = 1.0
    k_H = 1.0
  } else if (k == "screen") {

  } else if (k == "textil") {

  } else {}

  deg360InRad <- deg2rad(360.0)
  deg180InRad <- deg2rad(180.0)
  pow25To7 <- 25^7

  # Eq 2
  C1 = sqrt( (lab1$a*lab1$a) + (lab1$b*lab1$b) )
  C2 = sqrt( (lab2$a*lab2$a) + (lab2$b*lab2$b) )
  # Eq 3
  barC = (C1 + C2) / 2.0
  # Eq 4
  G = 0.5 * (1 - sqrt( (barC^7) / (barC^7 + pow25To7) ))
  # Eq 5
  a1Prime = (1.0 + G) * lab1$a
  a2Prime = (1.0 + G) * lab2$a
  # Eq 6
  CPrime1 = sqrt((a1Prime * a1Prime) + (lab1$b * lab1$b))
  CPrime2 = sqrt((a2Prime * a2Prime) + (lab2$b * lab2$b))
  # Eq 7
  if (lab1$b == 0 & a1Prime == 0) {
    hPrime1 = 0.0
  } else {
    hPrime1 = atan2(lab1$b, a1Prime)
    if (hPrime1 < 0) {
      hPrime1 = hPrime1 + deg360InRad
    }
  }
  if (lab2$b == 0 & a2Prime == 0) {
    hPrime2 = 0.0
  } else {
    hPrime2 = atan2(lab2$b, a2Prime)
    if (hPrime2 < 0) {
      hPrime2 = hPrime2 + deg360InRad
    }
  }

  # Step 2

  # Eq 8
  deltaLPrime = lab2$L - lab1$L
  # Eq 9
  deltaCPrime = CPrime2 - CPrime1
  # Eq 10
  CPrimeProduct = CPrime1 * CPrime2
  if (CPrimeProduct == 0) {
    deltahPrime = 0
  } else {
    # Avoid the fabs call
    deltahPrime = hPrime2 - hPrime1
    if (deltahPrime < -deg180InRad) {
      deltahPrime = deltahPrime + deg360InRad
    } else if (deltahPrime > deg180InRad) {
      deltahPrime = deltahPrime - deg360InRad
    }
  }
  # Eq 11
  deltaHPrime = 2.0 * sqrt(CPrimeProduct) *
    sin(deltahPrime / 2.0)

  # Step 3

  # Eq 12
  barLPrime = (lab1$L + lab2$L) / 2.0
  # Eq 13
  barCPrime = (CPrime1 + CPrime2) / 2.0
  # Eq 14
  hPrimeSum = hPrime1 + hPrime2
  if (CPrime1 * CPrime2 == 0) {
    barhPrime = hPrimeSum
  } else {
    if (abs(hPrime1 - hPrime2) <= deg180InRad) {
      barhPrime = hPrimeSum / 2.0
    } else {
      if (hPrimeSum < deg360InRad) {
        barhPrime = (hPrimeSum + deg360InRad) / 2.0
      } else {
        barhPrime = (hPrimeSum - deg360InRad) / 2.0
      }
    }
  }
  # Eq 15
  T = 1.0 - (0.17 * cos(barhPrime - deg2rad(30.0))) +
    (0.24 * cos(2.0 * barhPrime)) +
    (0.32 * cos((3.0 * barhPrime) + deg2rad(6.0))) -
    (0.20 * cos((4.0 * barhPrime) - deg2rad(63.0)))
  # Eq 16
  deltaTheta = deg2rad(30.0) *
    exp( -((barhPrime - deg2rad(275.0)) / deg2rad(25.0))^2.0 )
  # Eq 17
  R_C = 2.0 * sqrt( barCPrime^7 / (barCPrime^7 + pow25To7) )
  # Eq 18
  S_L = 1 + ( (0.015 * (barLPrime - 50.0)^2) /
                sqrt(20 + (barLPrime - 50)^2) )
  # Eq 19
  S_C = 1 + (0.045 * barCPrime)
  # Eq 20
  S_H = 1 + (0.015 * barCPrime * T)
  # Eq 21
  R_T = (-sin(2.0 * deltaTheta)) * R_C

  # Eq 22
  deltaE = sqrt(
    (deltaLPrime / (k_L * S_L))^2 +
      (deltaCPrime / (k_C * S_C))^2 +
      (deltaHPrime / (k_H * S_H))^2 +
      (R_T * (deltaCPrime / (k_C * S_C)) * (deltaHPrime / (k_H * S_H)) )
  )

  return (deltaE)
}

#' APCA contrast algorithm
#'
#' (dev 0.98G 4g development version)
#' General Guidelines on levels
#' * 90 Suggested for extremly thin fonts
#' * 75 Preferred level for columns of body text
#' * 60 The minimum level for readable content
#' * 45 The minimum for larger text (headlines)
#' * 30 The absolute minimum for any text
#' * 15 The absolute minimum for any non-text
#'
#' The calculation is performed in the RGB color space
#' @param bg Background color as color class
#' @param fg Foreground (Text) color as color class (default white)
#' @returns APCA contrast level
#'
#' @examples apca(RGB(255,255,255), RGB(234,116,57))
#'
apca <- function(bg, fg = RGB(255,255,255)) {
  bg = bg$to("RGB")
  fg = fg$to("RGB")

  trcExpon = 2.4
  Rco = 0.2126
  Gco = 0.7152
  Bco = 0.0722

  normBGexp = 0.55
  normTXTexp = 0.58
  revTXTexp = 0.57
  revBGexp = 0.62

  scale = 1.25

  blkThrs = 0.03
  blkClmp = 1.45

  deltaYmin = 0.0005

  loConThresh = 0.078
  loConFactor = 12.82051282051282
  loConOffset = 0.06

  loClip = 0.001

  Ybg = (bg$R/255)^trcExpon * Rco +
    (bg$G/255)^trcExpon * Gco +
    (bg$B/255)^trcExpon * Bco
  Ytxt = (fg$R/255)^trcExpon * Rco +
    (fg$G/255)^trcExpon * Gco +
    (fg$B/255)^trcExpon * Bco

  if (Ytxt > blkThrs) {Ytxt + (blkThrs - Ytxt)^blkClmp}
  if (Ybg > blkThrs) {Ybg + (blkThrs - Ybg)^blkClmp}

  if(abs(Ybg - Ytxt) < deltaYmin) return (0.0)

  if (Ybg > Ytxt) {
    SAPC = (Ybg^normBGexp - Ytxt^normTXTexp) * scale

    if(SAPC < loClip)
      return (0.0)
    if(SAPC < loConThresh)
      return ((SAPC - SAPC * loConFactor * loConOffset) * 100)
    else
      return((SAPC - loConOffset) * 100)
  } else {
    SAPC = (Ybg^revBGexp - Ytxt^revTXTexp) * scale

    if(SAPC > -loClip)
      return(0.0)
    if(SAPC > -loConThresh)
      return(100 * (SAPC - SAPC * loConFactor * loConOffset))
    else
      return( abs(100 * (SAPC + loConOffset)) )
  }
}


#' Calculate the relative luminance of color
#'
#' The calculation is performed in the rgb  color space
#' @param col Color
#' @examples luminance("#aabbcc")
#'
luminance <- function(color) {
  channel <- function(c) {
    if (c < 0.03928) {
      return(c / 12.92)
    } else {
      return( (c + 0.055) / (1.055^2.4) )
    }
  }
  rgb <- color$to("rgb")
  r <- channel(rgb$r)
  g <- channel(rgb$g)
  b <- channel(rgb$b)
  return(r * 0.2126 + g * 0.7152 +b * 0.0722)
}

#' Calculate contrast ratio of two colors
#'
#' @param colorA First color
#' @param colorB Second color
#' @examples contrast_ratio("#aabbcc", "#ddeeff")
#'
contrast_ratio <- function(colorA, colorB) {
  colorA <- luminance(colorA)
  colorB <- luminance(colorB)
  if (colorA > colorB) {
    return( 1 / ((colorB + 0.05) / (colorA + 0.05)) )
  } else {
    return( 1 / ((colorA + 0.05) / (colorB + 0.05)) )
  }
}

#' W3C contrast ratio
w3c_contrast_ratio <- function(ratio) {
  if (ratio > 7) {
    return (c("AAA"))
  } else if ( ratio > 4.5) {
    return(c("AA"))
  } else if ( ratio > 3.1) {
    return(c("AA Large"))
  } else if ( ratio < 3.1) {
    return (c("NA"))
  }
}
