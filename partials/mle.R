
log_determinant_chol <- function(A) {
  2 * sum(log(diag(A)))
}

neg_log_likelihood_spatial <- function(
  range,
  z,
  distance_spatial
){

  stopifnot(length(range) == 1)

  chol_Sigma <- chol(exp(-distance_spatial / range))
  stopifnot(nrow(chol_Sigma) == length(z))

  det_Sigma <- log_determinant_chol(chol_Sigma)

  # negative log-likelihood, up to the normalising constant
  as.numeric(det_Sigma + sum(backsolve(chol_Sigma, z, transpose = TRUE)^2))
}

neg_log_likelihood_spacetime <- function(
  range,
  z_mat = NULL,
  distance_spatial = NULL,
  distance_time = NULL
){

  stopifnot(length(range) == 2)

  chol_Sigma_space <- chol(exp(-distance_spatial / range[1]))
  chol_Sigma_time <- chol(exp(-distance_time / range[2]))
  stopifnot(
    ncol(chol_Sigma_space) == nrow(z_mat) &&
      ncol(z_mat) == nrow(chol_Sigma_time)
  )

  det_Sigma <- (
    nrow(chol_Sigma_space) * log_determinant_chol(chol_Sigma_time) +
      nrow(chol_Sigma_time) * log_determinant_chol(chol_Sigma_space)
  )
  Sigma_space_inv_zmat <- backsolve(chol_Sigma_space, z_mat, transpose = TRUE)
  Sigma_space_inv_zmat_Sigma_time_inv <- backsolve(
    chol_Sigma_time,
    t(Sigma_space_inv_zmat),
    transpose = TRUE
  )

  # negative log-likelihood, up to the normalising constant
  as.numeric(det_Sigma + sum(Sigma_space_inv_zmat_Sigma_time_inv^2))
}

mle_range_spatial <- function(data) {
  ## Estimate range parameter of exponential covariance function using maximum likelihood
  # Input:
  #   (data) dataframe with columns 'longitude', 'latitude', 'value'
  # Ouput:
  #   (range) a scalar of range parameter

  z <- scale(data$value)

  distance_spatial <- data %>%
    select(longitude, latitude) %>%
    as.matrix() %>%
    fields::RdistEarth(miles = FALSE)

  output <- optim(
    par = 500,
    fn = neg_log_likelihood_spatial,
    z = z,
    distance_spatial = distance_spatial,
    method = 'L-BFGS-B',
    lower = 100,
    upper = 1e4
  )

  if (output$convergence != 0) {
    log_warn('WARNING: optimisation did not converge: {output$convergence} \n {output$message}')
  }

  list(
    range_spatial = output$par,
    convergence = output$convergence
  )
}

mle_range_spacetime <- function(data) {
  ## Estimate range paramters of exponential covariance function using maximum likelihood
  # Input:
  #   (data) dataframe with columns 'longitude', 'latitude', 'time', 'value'
  # Notes:
  #   - space is in km, time is in hours
  # Ouput:
  #   (range) 2-vector of range parameters (space, time)

  data <- data %>%
    arrange(time, longitude, latitude)

  n_hours <- n_distinct(data$time)
  distance_time <- toeplitz(seq(0, n_hours - 1))

  # NOTE(jhj): distinct() preserves ordering
  distance_spatial <- data %>%
    select(longitude, latitude) %>%
    distinct() %>%
    as.matrix() %>%
    fields::RdistEarth(miles = FALSE)

  z_mat <- data %>%
    pull(value) %>%
    scale() %>%
    matrix(ncol = n_hours)

  # NOTE: should time upper bound be n_hours?
  output <- optim(
    par = c(500, 12),
    fn = neg_log_likelihood_spacetime,
    z_mat = z_mat,
    distance_spatial = distance_spatial,
    distance_time = distance_time,
    method = 'L-BFGS-B',
    lower = c(100, 1e-2),
    upper = c(1e4, 1e4)
  )

  if (output$convergence != 0) {
    log_warn('WARNING: optimisation did not converge: {output$convergence} \n {output$message}')
  }

  list(
    range_spatial = output$par[1],
    range_temporal = output$par[2],
    convergence = output$convergence
  )
}
