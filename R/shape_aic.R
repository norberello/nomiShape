#' Fit Nominal Data to Theoretical Shapes Using AIC (Safe Exponential)
#'
#' Computes the multinomial log-likelihood of observed counts against
#' four theoretical distributions (uniform, triangular, normal-like,
#' and exponential/Pareto-like) and returns AIC and ΔAIC values.
#'
#' @importFrom stats lm coef dnorm
#' @param df A data.frame or tibble containing the nominal variable.
#' @param var Character string giving the name of the nominal variable in `df`.
#' @param rate_exp Numeric. Default exponential rate. Only used if tail not clearly exponential.
#' @param eps Small numeric value added to probabilities to avoid log(0). Default is 1e-12.
#'
#' @return A data.frame with columns: Shape, AIC, DeltaAIC.
#' @name shape_aic
#' @export

shape_aic <- function(df, var, rate_exp = 0.7, eps = 1e-12) {

  # ---- Observed counts ----
  obs <- sort(table(df[[var]]), decreasing = TRUE)
  obs_vec <- as.numeric(obs)
  n <- sum(obs_vec)
  k <- length(obs_vec)
  r <- seq_len(k)

  # ---- Check if tail looks exponential ----
  # Simple heuristic: proportion of tail in cumulative sum
  tail_prop <- sum(obs_vec[(k-3):k]) / n
  if (tail_prop > 0.2) {  # tail not light enough, likely not exponential
    safe_rate <- rate_exp
  } else {
    # Estimate rate using linear regression on log(freq)
    # Only if there are enough points in tail
    if (k >= 5) {
      y <- log(obs_vec + eps)
      x <- r - 1
      lm_fit <- lm(y ~ x)
      safe_rate <- -coef(lm_fit)[2]
      if (safe_rate < 0) safe_rate <- rate_exp
    } else {
      safe_rate <- rate_exp
    }
  }

  # ---- Theoretical probabilities ----
  shapes <- list(
    Uniform = rep(1 / k, k),
    Triangular = (rev(seq_len(k)) + eps) / sum(rev(seq_len(k)) + eps),
    Normal = (dnorm(r, mean = 1, sd = k / 6) + eps) / sum(dnorm(r, mean = 1, sd = k / 6) + eps),
    Exponential = (exp(-safe_rate * (r - 1)) + eps) / sum(exp(-safe_rate * (r - 1)) + eps)
  )

  # ---- Multinomial log-likelihood ----
  logLik_multinom <- function(obs_vec, p) sum(obs_vec * log(p))

  # ---- Number of free parameters ----
  k_par <- c(Uniform = 0, Triangular = 0, Normal = 1, Exponential = 1)

  # ---- AIC calculation ----
  AIC_vals <- sapply(names(shapes), function(shape) {
    ll <- logLik_multinom(obs_vec, shapes[[shape]])
    -2 * ll + 2 * k_par[shape]
  })

  out <- data.frame(
    Shape = names(shapes),
    AIC = as.numeric(AIC_vals),
    stringsAsFactors = FALSE
  )
  out <- out[order(out$AIC), ]
  out$DeltaAIC <- out$AIC - min(out$AIC)
  rownames(out) <- NULL
  out
}
