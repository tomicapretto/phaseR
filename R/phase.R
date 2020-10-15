# Here I am going to write the "launch" function.

# This function takes the objects that are usually repeated among
# other function calls, such as the derivative function, parameters passed
# to the function, and the system used.
# It returns an object

# Question: Should it use pipes or `+` operation?

phase <- function(func, params = NULL, system = NULL, state.names = "default") {
  if (!is.function(func)) {
    stop("`func` must be a function")
  }

  if (!(system %in% c("one.dim", "two.dim"))) {
    stop("system must be set to either \"one.dim\" or \"two.dim\"")
  }

  if (length(state.names) == 1 && state.names == "default") {
    state.names <- if (system == "two.dim") c("x", "y") else "y"
  }

  if (system == "one.dim") {
    if (length(state.names) != 1) {
      stop("length(state.names) must be 1 for system = \"one.dim\"")
    }
  } else if (system == "two.dim") {
    if (length(state.names) != 2) {
      stop("length(state.names) must be 2 for system = \"two.dim\"")
    }
  }

  launcher <- list("func" = func, "params" = params, "system" = system,
                   "state.names" = state.names)

  invisible(structure(launcher, class = "phaser-launcher"))
}


phase(sum, system = "two.dim", state.names = letters[1:2])
