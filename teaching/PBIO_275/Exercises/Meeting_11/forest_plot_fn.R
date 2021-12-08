#' Meta-analysis forest plots from brms output
#'
#' This function draws a forest plot from a random-effects meta-analysis
#' model fitted with brms.
#'
#' @param data Data frame with study labels, effect sizes and standard errors.
#' A \code{data.frame} or \code{tibble}.
#' @param model A meta-analytic model estimated with brms.
#' @param level The "Confidence" level for the Credible Intervals.
#' Defaults to 0.95.
#' @param xlim Limits for the x-axis. The defaults are the smallest observed
#' ES - 4 Standard errors to the largest observed ES + 4 SEs.
#' @param show_data Logical; whether to show the observed effect size
#' and standard error below the meta-analytic estimates. Defaults to FALSE.
#' @param sort_estimates Logical; whether to sort the estimates in ascending
#' order of magnitude from bottom to top. Defaults to FALSE.
#' @param dens_fill String; color to fill the densities. Defaults to "grey60".
#' @param dens_col String; color for the outlines of the densities. Default: NA.
#'
#' @details
#'
#' \subsection{Requirements for data}{
#'
#' The data frame must contain the following columns:
#'
#' \describe{
#'  \item{study}{Unique sequential integers for each row in the data.}
#'  \item{label}{Text labels for each study (e.g. \code{"author (year)"}).}
#'  \item{yi}{Observed effect sizes.}
#'  \item{sei}{Standard errors.}
#' }
#' }
#'
#' \subsection{Requirements for brms model}{
#'
#' The meta-analytic model must be fitted with the following brms formula:
#' \code{yi | se(sei) ~ 1 + (1|study)}
#'
#' }
#'
#' This function reproduces code for the \code{geom_flat_violin.R}
#' (without which this function wouldn't work) by
#' David Robinson (\url{https://github.com/dgrtwo}):
#' \url{https://gist.github.com/dgrtwo/eb7750e74997891d7c20}, and
#' Ben Marwick (\url{https://github.com/benmarwick}):
#' \url{https://gist.github.com/benmarwick/2a1bb0133ff568cbe28d}.
#'
#' @export
#' @importFrom brms posterior_samples
#' @importFrom tidyr gather_
#' @importFrom dplyr select_ group_by_ summarise_ full_join arrange_ filter_
#' @importFrom stats quantile reorder
#'
#' @examples
#' \dontrun{
#' ## Use a data frame called d
#' fit <- brm(yi | se(sei) ~ 1 + (1|study), data = d)
#' brms_forest(data = d, model = fit)
#' }
#'
brms_forest <- function(data,
                        model,
                        level = .95,
                        xlim = NULL,
                        show_data = FALSE,
                        sort_estimates = FALSE,
                        dens_fill = "grey60",
                        dens_col = NA) {
  
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("brms needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("tidyr needed for this function to work. Please install it.",
         call. = FALSE)
  }
  # Data frame of posterior samples, including average effect
  samps <- brms::posterior_samples(model)
  samps <- select_(samps,
                   quote(-sd_study__Intercept),
                   quote(-lp__))
  names(samps) <- c("ME", data$study)
  # Hack 0-centered REs to 'random coefficients' by adding fixed effect
  samps.m <- as.matrix(samps)
  samps.m[,2:dim(samps.m)[2]] <- samps.m[,2:dim(samps.m)[2]] + samps.m[,1]
  sampsdf <- as.data.frame(samps.m)
  names(sampsdf) <- names(samps)
  # Transform to long data
  samps.l <- gather_(sampsdf,
                     key_col = quote(study),
                     value_col = "value",
                     gather_cols = names(sampsdf))
  
  # Summaries based on "level" argument
  samples_s <- group_by_(samps.l, "study")
  samples_s <- summarise_(samples_s,
                          mean = ~mean(value),
                          lwr = ~quantile(value, probs = .5-level/2),
                          upr = ~quantile(value, probs = .5+level/2))
  samples_s$s <- paste0(round(
    samples_s$mean, 2), " [",
    round(samples_s$lwr, 2), ", ",
    round(samples_s$upr, 2), "]")
  # Join with 'data' for plotting
  data$study <- as.character(data$study)
  samples_s <- full_join(data, samples_s, by = "study")
  samples_s$label <- ifelse(samples_s$study=="ME", "ME", samples_s$label)
  # Calculate some plotting parameters
  ylim <- ifelse(
    c(is.null(xlim), is.null(xlim)),  # Coerce test to length 2
    c(min(samples_s$yi, na.rm=T) - 4*max(samples_s$sei, na.rm=T),
      max(samples_s$yi, na.rm=T) + 4*max(samples_s$sei, na.rm=T)),
    xlim)
  # Order the studies and ME
  if (sort_estimates) {
    samples_s <- arrange_(samples_s, "mean")
    samples_s$order <- 1:nrow(samples_s)
    samples_s$order <- ifelse(samples_s$study=="ME", -Inf, samples_s$order)
    samples_s$study <- reorder(samples_s$study, samples_s$order)
  } else {
    samples_s <- arrange_(samples_s, "study")
    samples_s$order <- 1:nrow(samples_s)
    samples_s$order <- ifelse(samples_s$study=="ME", -Inf, samples_s$order)
    samples_s$study <- reorder(samples_s$study, samples_s$order)
  }
  p1 <- ggplot(samples_s, aes_(quote(study), quote(mean))) +
    # Posterior distributions
    geom_linerange(aes_(ymin=quote(lwr), ymax=quote(upr))) +
    geom_flat_violin(data = samps.l, fill = dens_fill, col = dens_col,
                     width = .95, alpha = .5, aes_(y=quote(value))) +
    geom_point(aes_(y=quote(mean))) +
    # Annotations and lines
    geom_vline(xintercept = 1.5) +
    geom_vline(xintercept = max(as.integer(samples_s$study))+.5) +
    geom_hline(yintercept = 0.0, lty = 2, size = .25, alpha = .6) +
    geom_text(data=filter_(samples_s, quote(study != "ME")),
              hjust = "inward", vjust = "middle",
              aes_(label = quote(s), y = quote(ylim[2]))) +
    geom_text(data=filter_(samples_s, quote(study == "ME")),
              hjust = "inward", vjust = "middle",
              aes_(label = quote(s), y = quote(ylim[2])), fontface = "bold") +
    # Scale and theme adjustments
    scale_x_discrete(breaks = samples_s$study,
                     labels = samples_s$label,
                     expand = c(0, 0.5)) +
    scale_y_continuous(limits = ylim,
                       expand = c(0, 0)) +
    coord_flip() +
    theme(axis.title.y = element_blank(),
          panel.border = element_rect(fill = NA, colour = NA, size=.60),
          axis.line.x = element_line(size = .7),
          plot.title = element_text(face = "bold", hjust = .5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.y = element_blank(),
          panel.background = element_rect(fill="transparent", color=NA))
  if (show_data) {
    p1 <- p1 +
      # Data mean and SEs
      geom_point(size = 1.2, aes_(y=quote(yi)), shape=4,
                 position = position_nudge(x=-.1), na.rm = T) +
      geom_linerange(size = .2,
                     aes_(ymin = quote(yi-sei*2), ymax=quote(yi+sei*2)),
                     position = position_nudge(x=-.1), na.rm = T)
  }
  return(p1)
}

# Requires the geom_flat_violin ggplot2 object from
# https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R
#
# somewhat hackish solution to:
# https://twitter.com/EamonCaddigan/status/646759751242620928
# based mostly on copy/pasting from ggplot2 geom_violin source:
# https://github.com/hadley/ggplot2/blob/master/R/geom-violin.r
#

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x,
                     xmax = x + width / 2)
            
          },
          
          draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data, xminv = x,
                              xmaxv = x + violinwidth * (xmax - x))
            
            # Make sure it's sorted properly to draw the outline
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))
            
            # Close the polygon: set first and last point the same
            # Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1,])
            
            ggplot2:::ggname("geom_flat_violin",
                             GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                            alpha = NA, linetype = "solid"),
          
          required_aes = c("x", "y")
  )
