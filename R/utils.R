#' Get dynamics function
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

          stop ('"alpha_scaled" must be either "scaled" or "unscaled"')

        }
      }

      n_hat <- n_bar * exp(eps)

      if(stochastic) y <- rpois(n = length(n_hat), lambda = n_hat) else y <- drop(n_hat)

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

          stop ('"alpha_scaled" must be either "scaled" or "unscaled"')

        }
      }

      n_hat <- n_bar * exp(eps)

      if(stochastic) y <- rpois(n = length(n_hat), lambda = n_hat) else y <- drop(n_hat)

      return(y)
    }
  }

  return(fun_dyn)
}
