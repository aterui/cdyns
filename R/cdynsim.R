#' Community dynamics simulation with stock enhancement
#'
#' @param n_timestep Number of simulation time steps to be saved
#' @param n_warmup Number of warm-up time steps. Species are randomly seeded during this period with no stock enhancement.
#' @param n_burnin Number of burn-in time steps. Stock enhancement operates.
#' @param n_stock_start Time step at which stocking starts.
#' @param n_species Number of species in a simulated community.
#' @param k Carrying capacity.
#' @param r_type Generation method for intrinsic population growth rates. Either \code{"constant"} or \code{"random"}.
#' @param r Intrinsic population growth rate. Disabled if \code{r_type = "random"}.
#' @param r_min Minimum value of intrinsic population growth rate. Disabled if \code{r_type = "constant"}.
#' @param r_max Maximum value of intrinsic population growth rate. Disabled if \code{r_type = "constant"}.
#' @param sd_env SD of environmental stochasticity in a log scale.
#' @param stochastic Whether demographic stochasticity is induced or not. If TRUE, population & immigration outcomes will be a random draws from a Poisson distribution with the expected value of population density or immigration.
#' @param stock Number of released individuals.
#' @param phi Fitness of released individuals relative to wild individuals.
#' @param int_type Generation method for an interaction matrix.  Either \code{"constant"}, \code{"random"}, or \code{"manual"}.
#' @param alpha Interspecific competition coefficient. Constant if \code{int_type = "constant"}. Expected value of an exponential distribution if \code{int_type = "random"}. Provide a full matrix if \code{int_type = "manual"}.
#' @param alpha_scale "scaled" or "unscaled". If scaled, competition coefficients are scaled by carrying capacity.
#' @param immigration Mean immigration per generation. Immigration is determined as \code{m ~ N(log(immigration), sd_immigration^2)}
#' @param sd_immigration SD immigration over time in a log scale.
#' @param model Model for community dynamics. Either \code{"ricker"} (multi-species Ricker model) or \code{"bh"} (multi-species Beverton-Holt model).
#' @param seed Expected number of seeds.
#' @param seed_interval Time interval for seeding.
#' @param extinct Absorbing condition. Species with density < extinct will be removed from the simulation.
#'
#' @return \code{df_dyn}
#' @return \code{df_community}
#' @return \code{df_species}
#' @return \code{interaction_matrix}
#'
#' @importFrom dplyr %>%
#' @importFrom stats rnorm runif rpois rbinom rexp sd var
#' @importFrom rlang .data
#'
#' @author Akira Terui, \email{hanabi0111@gmail.com}
#'
#' @export

cdynsim <- function(n_timestep = 1000,
                    n_warmup = 100,
                    n_burnin = 100,
                    n_stock_start = NULL,
                    n_species = 10,
                    k = 100,
                    r_type = "constant",
                    r = 1.5,
                    r_min = 1.0,
                    r_max = 3.5,
                    sd_env = 0.1,
                    stochastic = FALSE,
                    stock = 0,
                    phi = 1,
                    int_type = "constant",
                    alpha = 0.5,
                    alpha_scale = "scaled",
                    immigration = 0,
                    sd_immigration = 0,
                    model = "ricker",
                    seed = 5,
                    seed_interval = 10,
                    extinct = 0
) {

  # model type --------------------------------------------------------------
  fun_dyn <- fn_model(model = model)

  # variables ---------------------------------------------------------------

  ## basic objects ####
  n_sim <- n_warmup + n_burnin + n_timestep
  n_discard <- n_warmup + n_burnin
  if (is.null(n_stock_start)) n_stock_start <- n_warmup + 1

  m_dyn <- matrix(NA,
                  nrow = n_timestep * n_species,
                  ncol = 4)
  colnames(m_dyn) <- c("timestep",
                       "species",
                       "density",
                       "immigrant")

  st_row <- seq(from = 1,
                to = nrow(m_dyn),
                by = n_species)

  v_n <- rep(seed, n_species)

  ## parameter: species interaction ####
  m_int <- set_competition(n_species = n_species,
                           int_type = int_type,
                           alpha = alpha,
                           alpha_scale = alpha_scale)

  ## parameter: population dynamics ####
  v_r <- set_r(n_species = n_species,
               r = r,
               r_type = r_type,
               r_min = r_min,
               r_max = r_max)

  ## parameter: environmental stochasticity ####
  m_eps <- matrix(rnorm(n = n_sim * n_species,
                        mean = 0,
                        sd = sd_env),
                  nrow = n_sim,
                  ncol = n_species,
                  byrow = TRUE) # time x species matrix

  ## parameter: immigration ####
  m_im <- set_im(n_sim = n_sim,
                 n_species = n_species,
                 immigration = immigration,
                 sd_immigration = sd_immigration,
                 stochastic = stochastic)

  ## seed interval ####
  if (n_warmup > 0) {

    if (seed_interval > n_warmup) stop("n_warmup must be equal to or larger than seed_interval")
    seeding <- seq(from = seed_interval,
                   to = max(c(1, n_warmup)),
                   by = seed_interval)

  }

  # dynamics ----------------------------------------------------------------

  for (i in seq_len(n_sim)) {

    # seeding
    if (n_warmup > 0) {
      if (i %in% seeding) {
        v_n <- v_n + rpois(n_species, seed)
      }
    }

    v_n1 <- v_n2 <- v_n

    # stock enhancement
    if (i > n_stock_start - 1) {
      v_n1[1] <- v_n1[1] + phi * stock # for reproduction
      v_n2[1] <- v_n2[1] + stock # for competition
    }

    v_n_hat <- fun_dyn(r = v_r,
                       n1 = v_n1,
                       n2 = v_n2,
                       k = k,
                       m_int = m_int,
                       eps = m_eps[i, ],
                       stochastic = stochastic,
                       alpha_scale = alpha_scale) %>%
      as.vector()

    v_n <- v_n_hat + m_im[i, ]

    if (i > n_discard) {

      v_n[v_n < extinct] <- 0

      row_id <- seq(from = st_row[i - n_discard],
                    to = st_row[i - n_discard] + n_species - 1,
                    by = 1)

      m_dyn[row_id, ] <- cbind(rep(i, n_species) - n_discard, # timestep
                               seq_len(n_species), # species ID
                               v_n, # density
                               m_im[i, ])
    }
  }


  # export ------------------------------------------------------------------

  df_dyn <- dplyr::as_tibble(m_dyn)

  df_species <- df_dyn %>%
    dplyr::group_by(.data$species) %>%
    dplyr::summarize(mean_density = mean(.data$density),
                     sd_density = sd(.data$density)) %>%
    dplyr::mutate(species = seq_len(n_species),
                  k = k,
                  r = v_r,
                  alpha_j1 = m_int[, 1]) %>%
    dplyr::relocate(.data$species)

  df_community <- df_dyn %>%
    dplyr::group_by(.data$timestep) %>%
    dplyr::summarize(summed_density = sum(.data$density)) %>%
    dplyr::summarize(mean_density = mean(.data$summed_density),
                     sd_density = sd(.data$summed_density))

  m_vcov <- df_dyn %>%
    tidyr::pivot_wider(id_cols = .data$timestep,
                       names_from = .data$species,
                       names_prefix = "species",
                       values_from = .data$density) %>%
    dplyr::select(dplyr::starts_with("species")) %>%
    data.matrix() %>%
    var()

  # return ------------------------------------------------------------------

  return(
    list(df_dyn = df_dyn,
         df_community = df_community,
         df_species = df_species,
         interaction_matrix = m_int,
         vcov_matrix = m_vcov)
  )

}
