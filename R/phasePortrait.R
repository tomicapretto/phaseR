#' Phase portrait plot
#'
#' For a one-dimensional autonomous ODE, it plots the phase portrait, i.e., the
#' derivative against the dependent variable. In addition, along the dependent
#' variable axis it plots arrows pointing in the direction of dependent
#' variable change with increasing value of the independent variable. From this
#' stability of equilibrium points (i.e., locations where the horizontal axis is
#' crossed) can be determined.
#'
#' @param deriv A function computing the derivative at a point for the ODE
#' system to be analysed. Discussion of the required structure of these
#' functions can be found in the package vignette, or in the help file for the
#' function \code{\link[deSolve]{ode}}.
#' @param ylim Sets the limits of the dependent variable for which the
#' derivative should be computed and plotted. Should be a
#' \code{\link[base]{numeric}} \code{\link[base]{vector}} of
#' \code{\link[base]{length}} two.
#' @param ystep Sets the step length of the dependent variable vector for which
#' derivatives are computed and plotted. Decreasing \code{ystep} makes the
#' resulting plot more accurate, but comes at a small cost to computation time.
#' Defaults to \code{0.01}.
#' @param parameters Parameters of the ODE system, to be passed to \code{deriv}.
#' Supplied as a \code{\link[base]{numeric}} \code{\link[base]{vector}}; the
#' order of the parameters can be found from the \code{deriv} file. Defaults to
#' \code{NULL}.
#' @param points Sets the density at which arrows are plotted along the
#' horizontal axis; \code{points} arrows will be plotted. Fine tuning here, by
#' shifting \code{points} up and down, allows for the creation of more
#' aesthetically pleasing plots. Defaults to \code{10}.
#' @param frac Sets the fraction of the theoretical maximum length line
#' segments can take without overlapping, that they actually attain. Fine
#' tuning here assists the creation of aesthetically pleasing plots. Defaults
#' to \code{0.75}.
#' @param arrow.head Sets the length of the arrow heads. Passed to
#' \code{\link[graphics]{arrows}}. Defaults to \code{0.075}.
#' @param col Sets the colour of the line in the plot, as well as the arrows.
#' Should be a \code{\link[base]{character}}
#' \code{\link[base]{vector}} of \code{\link[base]{length}} one. Will be reset
#' accordingly if it is of the wrong \code{\link[base]{length}}. Defaults to
#' \code{"black"}.
#' @param xlab Label for the x-axis of the resulting plot.
#' @param ylab Label for the y-axis of the resulting plot.
#' @param add.grid Logical. If \code{TRUE}, a grid is added to the plot.
#' Defaults to \code{TRUE}.
#' @param \dots Additional arguments to be passed to either plot or arrows.
#' @inheritParams .paramDummy
#' @return Returns a list with the following components:
#' \item{add.grid}{As per input.}
#' \item{arrow.head}{As per input.}
#' \item{col}{As per input, but with possible editing if a
#' \code{\link[base]{character}} \code{\link[base]{vector}} of the wrong
#' \code{\link[base]{length}} was supplied.}
#' \item{deriv}{As per input.}
#' \item{dy}{A \code{\link[base]{numeric}} \code{\link[base]{vector}} containing
#' the value of the derivative at each evaluated point.}
#' \item{frac}{As per input.}
#' \item{parameters}{As per input.}
#' \item{points}{As per input.}
#' \item{xlab}{As per input.}
#' \item{y}{A \code{\link[base]{numeric}} \code{\link[base]{vector}} containing
#' the values of the dependent variable for which the derivative was evaluated.}
#' \item{ylab}{As per input.}
#' \item{ylim}{As per input.}
#' \item{ystep}{As per input.}
#' @author Michael J Grayling
#' @seealso \code{\link[graphics]{arrows}}, \code{\link[graphics]{plot}}
#' @export
#' @examples
#' # A one-dimensional autonomous ODE system, example2.
#' example2_phasePortrait <- phasePortrait(example2,
#'                                         ylim   = c(-0.5, 2.5),
#'                                         points = 10,
#'                                         frac   = 0.5)
phasePortrait <- function(deriv, ylim, ystep = 0.01, parameters = NULL,
                          points = 10, frac = 0.75, arrow.head = 0.075,
                          col = "black", add.grid = TRUE, state.names = "y",
                          xlab = state.names, ylab = paste0("d", state.names),
                          ...) {
    if (any(!is.vector(ylim), length(ylim) != 2)) {
        stop("ylim is not a vector of length 2, as is required")
    }
    if (ylim[2] <= ylim[1]) {
        stop("ylim[2] is less than or equal to ylim[1]")
    }
    if (ystep <= 0) {
        stop("ystep is less than or equal to zero")
    }
    if (!is.vector(col)) {
        stop("col is not a vector as required")
    }
    if (length(col) > 1) {
        col <- col[1]
        message("Note: col has been reset as required")
    }
    if (!is.logical(add.grid)) {
      stop("add.grid must be set to either TRUE or FALSE")
    }
    y              <- seq(ylim[1], ylim[2], ystep)
    dy             <- numeric(length(y))
    for (i in 1:length(y)) {
      dy[i]        <- deriv(0, stats::setNames(y[i], state.names[1]),
                            parameters)[[1]]
    }
    graphics::plot(y, dy, col = col, type = "l", xlab = xlab, ylab = ylab, ...)
    if (add.grid) {
      graphics::grid()
    }
    y.arrows       <- seq(ylim[1], ylim[2], length.out = points)
    dy.arrows      <- numeric(points)
    y.shift        <- 0.5*frac*(y.arrows[2] - y.arrows[1])
    for (i in 1:points) {
      dy.arrows[i] <- deriv(0, stats::setNames(y.arrows[i], state.names[1]),
                            parameters)[[1]]
    }
    pos            <- which(dy.arrows > 0)
    graphics::arrows(y.arrows[pos] - y.shift, numeric(length(y.arrows[pos])),
                     y.arrows[pos] + y.shift, numeric(length(y.arrows[pos])),
                     length = arrow.head, col = col, ...)
    neg            <- which(dy.arrows < 0)
    graphics::arrows(y.arrows[neg] + y.shift, numeric(length(y.arrows[neg])),
                     y.arrows[neg] - y.shift, numeric(length(y.arrows[neg])),
                     length = arrow.head, col = col, ...)
    return(list(add.grid   = add.grid,
                arrow.head = arrow.head,
                col        = col,
                deriv      = deriv,
                dy         = dy,
                frac       = frac,
                parameters = parameters,
                points     = points,
                xlab       = xlab,
                y          = y,
                ylab       = ylab,
                ylim       =  ylim,
                ystep      = ystep))
}
