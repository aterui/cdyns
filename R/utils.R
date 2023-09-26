#' Utility: get dynamics function
#'
#' @inheritParams cdynsim
#' @export

fn_model <- function(model) {

  if (model == "bh") {
    fun_dyn <- function(r, n1, n2, k, m_int, eps,
                        stochastic = FALSE, alpha_scale = "scaled") {

      if (alpha_scale == "scaled") {

        n_bar <- (n1 * exp(r)) / (1 + ((exp(r) - 1) / k) * (m_int %*% n2))

      } else {

        if (alpha_scale == "unscaled") {

          n_bar <- (n1 * exp(r)) / (1 + m_int %*% n2)

        } else {

          stop('"alpha_scaled" must be either "scaled" or "unscaled"')

        }
      }

      n_hat <- n_bar * exp(eps)

      if (stochastic) y <- rpois(n = length(n_hat), lambda = n_hat) else y <- drop(n_hat)

      return(y)
    }
  }

  # ricker
  if (model == "ricker") {
    fun_dyn <- function(r, n1, n2, k, m_int, eps,
                        stochastic = FALSE,
                        alpha_scale = "scaled") {

      if (alpha_scale == "scaled") {

        n_bar <- n1 * exp(r * (1 - ((m_int %*% n2) / k)))

      } else {

        if (alpha_scale == "unscaled") {

          n_bar <- n1 * exp(r - m_int %*% n2)

        } else {

          stop('"alpha_scaled" must be either "scaled" or "unscaled"')

        }
      }

      n_hat <- n_bar * exp(eps)

      if (stochastic) y <- rpois(n = length(n_hat), lambda = n_hat) else y <- drop(n_hat)

      return(y)
    }
  }

  return(fun_dyn)
}


#' Utility: set competition coefficients
#'
#' @inheritParams cdynsim
#' @export

set_competition <- function(n_species, int_type, alpha, alpha_scale) {

  ### off-diagonal elements
  if (int_type == "random") {
    if (length(alpha) > 1) stop("alpha must be a scalar")
    m_int <- matrix(rexp(n_species * n_species,
                         rate = 1 / alpha),
                    nrow = n_species,
                    ncol = n_species)
  }

  if (int_type == "constant") {
    if (length(alpha) > 1) stop("alpha must be a scalar")
    m_int <- matrix(alpha,
                    nrow = n_species,
                    ncol = n_species)
  }

  if (int_type == "manual") {
    if (!is.matrix(alpha)) stop("alpha must be a matrix")
    if (any(dim(alpha) != n_species)) stop("alpha must have dimensions of n_species")
    m_int <- alpha
  }

  if (!(int_type %in% c("random", "constant", "manual"))) {
    stop("int_type must be either random, constant, or manual")
  }

  ### diagonal elements
  if (alpha_scale == "scaled") {
    diag(m_int) <- 1
  } else {
    if (alpha_scale == "unscaled") {
      message('"alpha_scale = "unscaled"; carrying capacity is controlled by "r" & "alpha"')
    } else {
      stop('"alpha_scale" must be either "scaled" or "unscaled"')
    }
  }

  return(m_int)
}


#' Utility: set intrinsic growth rate
#'
#' @inheritParams cdynsim
#' @export

set_r <- function(n_species, r, r_type, r_min, r_max) {

  if (r_type == "random") {

    v_r <- runif(n = n_species,
                 min = r_min,
                 max = r_max)

  } else {

    if (r_type == "constant") {

      if (length(r) == 1) {
        v_r <- rep(r, n_species)
      } else {
        if (length(r) != n_species) stop("r must have a length of n_species")
        v_r <- r
      }

    } else {
      message("r_type must be either random or constant")
    }

  }

  return(v_r)
}


#' Utility: set immigration
#'
#' @param n_sim Total number of simulation run.
#' @inheritParams cdynsim
#' @export

set_im <- function(n_sim, n_species, immigration, sd_immigration, stochastic) {

  m_im <- matrix(NA, nrow = n_sim, ncol = n_species)

  ### vector mean immigration
  if (length(immigration) == 1) {

    v_im <- rep(immigration, n_species)

  } else {

    if (length(immigration) != n_species) stop("the number of elements in immigration must match n_species")
    v_im <- immigration

  }

  ### matrix immigration
  for (s in 1:n_species) {
    if (v_im[s] > 0) {

      v_log_im <- rnorm(n = n_sim,
                        mean = log(v_im[s]),
                        sd = sd_immigration)

      m_im[, s] <- exp(v_log_im)

    } else {

      m_im[, s] <- rep(0, n_sim)

    }
  }

  if (stochastic) {
    m_im <- matrix(rpois(n = n_sim * n_species, lambda = c(m_im)),
                   nrow = n_sim,
                   ncol = n_species)
  }

  return(m_im)
}
