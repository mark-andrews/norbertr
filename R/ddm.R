#' Simulate a drift diffusion model
#'
#' Using the random walk approximation method described by Tuerlinckx et al
#' 2001.
#'
#' @param n Number of independent samples
#' @param b Starting point
#' @param a Inter-barrier distance
#' @param v Drift-rate
#' @param t_eps Time-step size
#'
#' @references
#' Tuerlinckx, F., Maris, E., Ratcliff, R., & De Boeck, P. (2001). A comparison
#' of four methods for simulating the diffusion process. Behavior Research
#' Methods, Instruments, & Computers, 33(4), 443-456.
#'
#' @return A data-frame with n rows and two columns, one the choice (which
#'   barrier crossed first) and the other the time of the crossing.
#' @export
#'
#' @examples
#' simulate_ddm(10, 0.5, 2, 1)
simulate_ddm <- function(n, b, a, v, t_eps = 0.0001) {
  purrr::map_dfr(seq(n), ~random_walk(b, a, v, t_eps))
}


#' Bivariate density of response time and choice in a drift-diffusion model
#'
#' The bivariate distribution over response time and choice, when choice is 0,
#' is given by Eq (1) in the Appendix of Wabersich & Vandekerckhove (2014).
#'
#' The bivariate distribution over response time and choice, when choice is 1,
#' is given by Eq (1) in the Appendix of Wabersich & Vandekerckhove (2014) by
#' replacing beta by 1 - beta, and replace delta by -delta.
#'
#' Here, we implement this density using the method described by Navarro & Fuss
#' (2009). This first approximates the density in a DDM with drift 0 and
#' inter-barrier distance (a) of 1 for time t/a^2, and then re-scales this.
#'
#' @references
#' Wabersich, D., & Vandekerckhove, J. (2014). The RWiener Package: an R Package
#' Providing Distribution Functions for the Wiener Diffusion Model. R Journal,
#' 6(1).
#' @references
#' Navarro, D. J., & Fuss, I. G. (2009). Fast and accurate calculations for
#' first-passage times in Wiener diffusion models. Journal of mathematical
#' psychology, 53(4), 222-230.
#'
#' @param t time
#' @param b Relative starting point
#' @param a Upper boundary
#' @param v Drift rate
#' @param choice A binary choice represented as 0 and 1
#'
#' @return A probability density
#' @export
#'
#' @examples
#' time_choice_density(t=1, b=0.5, a = 2, v = 1)
time_choice_density <- function(t, b, a, v, choice = 1){

  if (choice == 1){
    v <- -v
    b <- 1 - b
  }

  epsilon <- 0.001 # Navarro & Fuss p225 (second paragraph; first column)

  tt <- t/a^2 # See final term in Equation 2 of Navarro & Fuss

  # Navarro & Fuss Eq 10
  largetimek <-  sqrt(
    -2 * (log(pi) + log(tt) + log(epsilon)) / ((pi^2)*tt)
  )

  # Navarro & Fuss Eq 11
  smalltime_k <- 2 + sqrt(
    -2 * tt * (log(2)
              + log(epsilon)
              + 0.5*(log(2) + log(pi) + log(tt))
    )
  )

  # Navarro & Fuss Eq 12
  lambda_t <- smalltime_k - largetimek

  # Navarro & Fuss Eq 13
  if (lambda_t < 0) {

    K_div <- (ceiling(smalltime_k) - 1)/2

    p <- 0

    for (k in seq(-floor(K_div), ceiling(K_div))) {
      p <- p + (b + 2*k) * exp(- ((b + 2*k)^2) / (2*tt) )
    }

    p <- p / sqrt(2 * pi * tt^3)

  } else {

    K <- ceiling(largetimek)

    p <- 0
    for (k in seq(K)) {
      p <- p + k * exp( - k^2 * pi^2 * tt / 2) * sin(k * pi * b)
    }

    p <- p * pi;
  }

  # Given that p here is final term in Navarro & Fuss Eq 2
  # Here, we return Eq 2 (using the truncated sums approximation)
  p * exp(-v * a * b - v^2*t/2 )/(a^2)

}

#' Marginal probability of one of the two alternative choices in a DDM
#'
#' From Equation 3 of Tuerlinckx et al 2001. They use `z` in their equation,
#' which is `b*a` using parameters here. Eq 3 gives probability of crossing
#' upper barrier. Probability of crossing lower barrier uses the same formula
#' but with b = 1-b and v = -v.
#'
#' @param b Relative starting point, a value between 0 and 1
#' @param a Inter-barrier distance, a positive real
#' @param v Drift rate, negative or positive real
#' @param choice Binary, 0 or 1.
#'
#' @references
#' Tuerlinckx, F., Maris, E., Ratcliff, R., & De Boeck, P. (2001). A comparison
#' of four methods for simulating the diffusion process. Behavior Research
#' Methods, Instruments, & Computers, 33(4), 443-456.
#'
#' @return The probability of first crossing the upper or lower barrier
#' @export
#'
#' @examples
#' dchoice(b= 0.5, a = 2, v = 0.1)
dchoice <- function(b, a, v, choice = 1) {

  stopifnot(choice %in% c(0, 1))

  if (choice == 0){
    b = 1-b
    v = -v
  }

  (exp(-2*a*b*v) - 1) / (exp(-2*a*v) - 1)
}



#' Bayesian inference of a drift diffusion model
#'
#' @param formula A formula of the kind `y + z ~ 1`
#' @param data A data frame with response times and response choices
#' @param tau The non-decision time
#' @param alpha_ub The upper bound on the uniform prior on alpha
#' @param sigma The standard deviation of the normal prior on delta
#' @param ... Arguments passed to `rstan::sampling` (e.g. chains, cores)
#'
#' @return A stan model with MCMC samples
#' @export
#'
#' @examples
#' set.seed(10101)
#' sims_df <- simulate_ddm(1000, b = 0.75, a = 2, v = 0.5) %>%
#'     dplyr::mutate(time = time + 0.1)
#' ddm_stan(time + choice ~ 1, data = sims_df, cores = 4, refresh = 0)
ddm_stan <- function(formula, data, tau = 0.1, alpha_ub = 5, sigma = 2, ...){

  if (!formula.tools::is.two.sided(formula) ) {
    stop(sprintf('The formula should be two-sided, e.g. x + y ~ 1, not %s.'),
         as.character(formula))
  }

  if (formula.tools::rhs(formula) != 1) {
    stop(sprintf("The rhs of the formula can only be '1' in the current implementation"))
  }

  lhs_vars <- formula.tools::lhs.vars(formula)

  if (length(lhs_vars) != 2){
    stop(sprintf("There must be two variables on the lhs of the formula"))
  }

  # the first var on lhs is the response time variable
  # it must be numeric and non-negative
  y <- dplyr::pull(data, lhs_vars[1])

  if (!is.numeric(y) | any(y < 0)) {
    stop(sprintf("The first variable on lhs must be the response time; numeric and non-negative"))
  }

  # the second var on lhs is the choice
  # for now is must be numeric with values of exactly 0 or 1
  # For future versions, we will accept character vectors, or logical vectors, or factor vectors too
  z <- dplyr::pull(data, lhs_vars[2])

  if (!is.numeric(z) | !all(sort(unique(z)) == c(0, 1))) {
    stop(sprintf("The second var on lhs must be the response choice; numeric with values of either 0 or 1"))
  }

  # sample size
  n <- nrow(data)

  stan_data = list(z = z,
                   y = y,
                   n = n,
                   tau = 0.1,
                   alpha_ub = 5,
                   sigma = 2)

  rstan::sampling(stanmodels$ddm, data = stan_data, ...)

}
