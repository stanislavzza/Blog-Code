#' Latent variable statistics
#' Rather than assume an ordinal scale is a scalar, we can induce a normally-distributed latent
#' variable and adjust the intervals between levels accordingly, then compute mean and standard deviation.
#' See reference at https://osf.io/3tkz4/. Also see MASS polrToOrdScale as per blog post
#' http://doingbayesiandataanalysis.blogspot.com/2014/11/ordinal-probit-regression-transforming.html
#' @param x a vector of integers, usually 1-5, and NAs
#' @param method One of c("median", "cuts", "ols", "logistic", "probit", "loglog", "cloglog", "cauchit"),
#' the method to use to fit the data to a normal distribution and transform scale values.
#' \itemize{
#'  \item{"median"}{uses cutpoints between response choice frequencies to find the median z-scores for each,
#'  and then uses the first and last of these to define the linear transformation.}
#'  \item{"cuts"}{same as median, but uses the first and last cut point instead. }
#'  \item{"ols"}{uses all the frequencies, not just first and last, with a weighted least-squares fit to find
#'  the linear transformation}
#'  \item{"logistic"}{uses the MASS package to fit first and last cut to a logistic curve}
#'  \item{"probit"}{uses the MASS package to fit first and last cut to a probit curve, giving the same results
#'  as the "cuts" option}
#'  }
#'  For the MASS options, see https://www.rdocumentation.org/packages/MASS/versions/7.3-51.4/topics/polr for
#'  details.
#' @return A list with
#' \itemize{
#'  \item{method}{The method used}
#'  \item{scale}{The response scale names, which must be integers}
#'  \item{proportions}{The proportions for each response, summing to 1.}
#'  \item{cuts}{The transformed cut-points between response values.}
#'  \item{centers}{The transformed median points for each response value.}
#'  \item{mapped_values}{The map between original scale and transformed scale}
#'  \item{sigma}{The standard deviation of the distribution}
#'  \item{mu}{The mean of the distribution}
#'  \item{xt}{A vector the same length as x with the transformed x values into the induced latent scale.}
#' }
#' latent mean and standard deviation
#' latent_stats(c(rep(1,100),2,2,rep(3,50), rep(4,25)))
#' @export
#'

latent_stats <- function(x, method = "median"){
  # x is a vector of integers, usually 1-5, and NAs
  # We'll assume that x is integers and that they should include any missing
  # intermediate values. For example, if x = c(1,1,3,3,3), we assume that 2 is a valid
  # response as well, but that it's missing from this sample (has frequency zero)

  # we can only accept integer values for x. Sometimes they arrive as numerics.
  if(class(x) == "numeric") x <- as.integer(x)
  if(class(x) != "integer") stop("Requires integer vector input")

  # we can't deal with NAs, so delete them
  x <- x[!is.na(x)]

  xtab  <- table(x)                  # frequency table
  K <- length(xtab)                  # number of levels

  # if only one level, there's not much to do
  if(K == 1){ # all values are the same so take the first as the mean, etc.
    return(list(scale = x[1], proportions = 1, cuts = NA, centers = 0, mapped_values = x[1], sigma = 0, mu = x[1],  xt = x))
  }

  xlevels <- as.integer(names(xtab))       # integer values of levels that we are given as the response names
                                           # e.g. 1 = strongly disagree
  xprop <- unname(prop.table(xtab))        # proportion
  xcum  <- cumsum(xprop)[1:K-1]            # cumulative scale probabilities
  cuts  <- qnorm(xcum)                     # with K responses, there are K-1 cuts between them

  # We'll create a linear map between N(0,1) z-scores and the response scale.
  # First find the slope using the two end points: first and last response value,
  # which is our y-value, and first and last cut point z-scores, which are the x values.
  # Because the response value (e.g. 1 = strongly disagree) has the part of the distribution
  # to the left of the cut point, we assume that the cutpoint occurs at that value plus .5,
  # (e.g. strongly disagree ends at 1.5, where disagree begins). So for the last cut point we
  # subtract .5 and for the first one we add .5 to get
  #            xlevels[K] - .5 - (xlevels[1] + .5)
  # Using this results in the formula
  # slope <- (xlevels[K] - xlevels[1] - 1) / (cuts[K-1] - cuts[1])

  # But the division between scale levels probably doesn't actually occur at x + .5.
  # Instead, we'll calculate the median z-score for each response category and use that instead.
  center_from_cut <- function(.cuts, .method = "cuts"){

    if(.method %in% c("median","cuts","ols","ols2", "probit")) {
       .xcum <- pnorm(.cuts)
        return (  qnorm( (c(.xcum,1) + c(0,.xcum)) / 2 ) )
    }
    if(.method == "logistic") {
       .xcum <- plogis(.cuts)
       return (  qlogis( (c(.xcum,1) + c(0,.xcum)) / 2 ) )
    }
  }

    # Create a mapping function y = f(z) depending on the method we want to use.

  if (method == "median") {  # use the median z-scores for first and last levels.
      scale_centers <- center_from_cut(cuts, method)
      slope <- (xlevels[K] - xlevels[1]) / (scale_centers[K] - scale_centers[1])
      f <- function(z) slope * (z - scale_centers[1]) + xlevels[1]
  }

  if (method == "cuts") {
    scale_centers <- center_from_cut(cuts, method)
    slope <- (xlevels[K] - xlevels[1] - 1) / (cuts[K-1] - cuts[1])
    f <- function(z) slope * (z - cuts[1]) + xlevels[1] + .5
  }

  if (method == "ols") {
    scale_centers <- center_from_cut(cuts, method)
    cm <- lm(xlevels ~ scale_centers, weights = unname(xtab))
    f <- function(z) coef(cm)[1] + z*coef(cm)[2]
  }

  if (method == "ols2") {
    # function for assigning values based on zscores and cuts
    cut_n <- function(z, .cuts, .xlevels){

      out <- rep(0,length(z))

      for(i in 1:length(z)){
        segment <- sum(z[i] > .cuts) +1
        out[i] <- .xlevels[ segment  ]
      }
      return(out)
    }

    scale_centers <- center_from_cut(cuts, method)

    # choose some reasonable points to interpolate
    z <- seq(from = -4, to = 4, length.out = 500)
    y <- cut_n(z, cuts, xlevels)  # y-values are the labels for each segment

    cm <- lm(y ~ z, weights = dnorm(z))  # weight the points by their probability
    f <- function(z) coef(cm)[1] + z*coef(cm)[2]
  }

  # the maximum likelihood estimates from the MASS packages
  if (method %in% c("logistic", "probit")) {
    cuts <- MASS::polr(as.factor(x) ~ 1, method = method)$zeta %>% unname()
    slope <- (xlevels[K] - xlevels[1] - 1) / (cuts[K-1] - cuts[1])
    f <- function(z) slope * (z - cuts[1]) + xlevels[1] + .5
    scale_centers <- center_from_cut(cuts,method)
  }

  # On the N(0,1) curve, the mean is at zero and 1 SD is at 1 by definition.
  # Use the function to find standard deviation and mean of the latent scale
  mu    <- f(0)
  sigma <- f(1) - mu  # note: not really the standard deviation for the logistic, which is at pi/sqrt(3)


  # Now we map the original x values to the transformed ones by creating a look-up table.
  # We can't use a linear map, because it's almost certainly non-linear.
  # At this point we only have the points between each response value, e.g. 1.5, 2.5, identified
  # by their z-scores and imputed scale values. We can see them with f(cuts). The simplest method
  # for interpolating the scale values at their centers (i.e. not at cut points) is to find
  # the mid-point of the cuts on either end of the response range. This isn't entirely accurate
  # because the probability is not evenly distributed on a normal curve. We can adjust for that
  # using the cumulative distribution function.

  # create a map. Note that this adjusts ALL the values, even the end points.
  xt_map <- f(scale_centers)          # e.g a 1-5 scale maps to numbers near 1 - 5

  # Do the transform by indexing x on the name of the scale. Usually these are the same, but you
  # might have a case where "2" is not represented in your data, for example.
  xt <- xt_map[ match(x, xlevels) ]

  # Send back all the information needed to investigate further: the scale names,
  # imputed mean and sd, transformed cut values, and the transformed data.
  return(list(method = method,
              scale = xlevels,
              proportions = xprop,
              cuts = f(cuts),
              centers = scale_centers,
              mapped_values = xt_map,
              sigma = sigma,
              mu = mu,
              xt = xt))
}

