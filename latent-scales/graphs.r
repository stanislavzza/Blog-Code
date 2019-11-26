#' Plot latent value distribution
#' Shades a normal distribution according to cutpoints from latent values.
#' @param lstats is the output from latent_stats(x)--a list with mean, sd, and transformed x
#' @param step sets the resolution for plotting, defaulting to .01. Using .001 makes for smoother
#' graphs, but takes longer.
#' @param lollipop Add vertical lines with points at the top marking the probability of
#' each original scale value, situated along the x-axis where the transformation has mapped it to.
#' @export
plot_latent_x <- function(lstats, step = .01, lollipop = FALSE){

  if(!(lstats$method %in% c("median","ols","ols2", "cuts","probit","logistic"))) stop("Unrecognized method.")

  # create a centered normal curve with three standard deviations on each side

  if (lstats$method == "logistic") {
    df <- data.frame(x = seq(-6,6,step))
    df$y <- dlogis(df$x)                      # logistic density
  } else {
    df <- data.frame(x = seq(-3,3,step))
    df$y <- dnorm(df$x)                      # normal curve density
  }
  df$xt <- df$x*lstats$sigma + lstats$mu   # transform z-scores into the latent response scale

  # Get the integer names of the responses, e.g. 1-5
  xnames <- lstats$scale

  # number of scale values
  K <- length(xnames)

  # Tag each transformed x value with the response it belongs to to create groups.
  # First make a function to find out where the point is relative to cut points.
  cut_n <- function(t){
    out <- rep(0,length(t))
    for(i in 1:length(t)){
      out[i] <- sum(t[i] > lstats$cuts)
    }
    return(out)
  }

  df$group <- xnames[cut_n(df$xt) + 1] %>% as.factor()

  plot_out <- ggplot(df, aes(x = xt, y = y, group = group, fill = group)) +
    geom_area() +
    theme_minimal() +
    scale_x_continuous(name = "Latent response scale",breaks = lstats$scale, labels = as.character(lstats$scale)) +
    ylab("Probability density") +
    guides(fill=guide_legend(title="Item Responses")) +
    scale_fill_manual(values = colorRampPalette(c("steelblue","lightgray"))(K))

  # Add a reference line if requested
  # It shows the probabilites of responses and their mapped values

  # need a dataframe for ggplot to work with
  pdf <- data.frame(x = lstats$centers, y = lstats$mapped_values, group = xnames[1])

  if(lollipop == TRUE){
    plot_out <- plot_out +
      annotate("segment", x = lstats$mapped_values,
                          xend = lstats$mapped_values,
                          y = 0,
                          yend = lstats$proportions,
                          color = "red") +
      annotate("point", x = lstats$mapped_values, y = lstats$proportions, color = "red")

  }

  return(plot_out)

}

#' Compare latent scale methods
#' Creates a combined plot of all the available methods for latent scales for an
#' integer vector of values.
#' @param x The integer vector of values.
#' @return A ggplot with the nominal scale as the x-axis and the transformed scales
#' plotted vertically. A dashed 1:1 line is added for reference.
#' @export
plot_latent_method_comparison <- function(x){

  lstats_median <- latent_stats(x, method = "median")

  lstats_cuts <- latent_stats(x, method = "cuts")

  lstats_ols    <- latent_stats(x, method = "ols")

  lstats_ols2    <- latent_stats(x, method = "ols2")

  lstats_probit <- latent_stats(x, method = "probit")

  lstats_logistic <- latent_stats(x, method = "logistic")

  output <- data.frame(nominal = 1:length(lstats_median$scale),
                       median = lstats_median$mapped_values,
                       cuts = lstats_cuts$mapped_values,
                       ols    = lstats_ols$mapped_values,
                       ols2    = lstats_ols2$mapped_values,
                       probit = lstats_probit$mapped_values,
                       logistic = lstats_logistic$mapped_values)

  ggplot(output %>%
           gather(method, value, - nominal),
         aes(x = nominal, y = value - nominal, group = method, color = method)) +
    geom_point() +
    geom_line() +
    geom_abline(slope = 0, intercept = 0, linetype = "dashed") +
    theme_minimal()


}
