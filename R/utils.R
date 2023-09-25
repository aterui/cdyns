#' Get dynamics function
#'
#' @inheritParams cdynsim
#' @export

mtype <- function(type) {
  if (type == "bh") {
    fun_dyn <- function(r, n1, n2, k, m_int, eps, stochastic = FALSE) {
      n_bar <- (n1 * exp(r)) / (1 + ((exp(r) - 1) / k) * (m_int %*% n2))
      n_hat <- n_bar * exp(eps)

      if(stochastic) y <- rpois(n = length(n_hat), lambda = n_hat) else y <- as.vector(n_hat)

      return(y)
    }
  }

  # ricker
  if (type == "ricker") {
    fun_dyn <- function(r, n1, n2, k, m_int, eps, stochastic = FALSE) {
      n_bar <- n1 * exp(r * (1 - ((m_int %*% n2) / k)))
      n_hat <- n_bar * exp(eps)

      if(stochastic) y <- rpois(n = length(n_hat), lambda = n_hat) else y <- as.vector(n_hat)

      return(y)
    }
  }

  return(fun_dyn)
}
