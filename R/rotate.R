#' @title rotate
#'
#' @description Computes the rotated factor model according to Altavilla et al. 2019
#'
#' @param data specify which data to process
#' @param crisis_date specify the starting date of the crisis for the rotation of the QE shocks
#' @param window which window do you want to rotate? choose between "release" or "conference"
#' @param extended logical; TRUE for extended rotation
#'
#' @return A data frame containing the rotated factors
#'
#' @examples
#' download_hfd("https://www.ecb.europa.eu/pub/pdf/annex/Dataset_EA-MPD.xlsx", getwd())
#' pcw <- load_hfd(pcw.csv,
#'   exclude_date = c("2001-09-17", "2008-10-08", "2008-11-06"),
#'   range = c("2001-12-31", "2018-09-13")
#' )
#' rotate(pcw, crisis_date = "2008-09-04", window = "conference")
#' @export
#' @importFrom NlcOptim solnl
#' @importFrom dplyr select mutate pull rename bind_cols
#' @importFrom stats lm coef
rotate <- function(data, crisis_date = "2008-09-04", window = "release", extended = FALSE) {
  ois_matrix <- data %>%
    dplyr::select(dplyr::starts_with("ois")) %>%
    as.matrix()

  date_vector <- data %>%
    dplyr::pull(date)

  # Estimate factor model
  fm <- factor_model(ois_matrix)

  # Scale factors
  scale <- apply(fm$factors, 2, sd)
  factors <- sweep(fm$factors, 2, scale, "/")[, 1:3] # Use a maximum of 3 factors

  idx_pre <- 1:(which(date_vector == as.POSIXlt(crisis_date, tz = "UTC")) - 1)

  # Restrict and solve model
  id <- list(fa = factors[idx_pre, ], l = (fm$loadings[, 1:3] * scale[1:3]))

  con <- function(x) {
    loading <- id$l
    f <- NULL
    # Orthogonal restrictions
    f <- rbind(f, x[1]^2 + x[4]^2 + x[7]^2 - 1)
    f <- rbind(f, x[2]^2 + x[5]^2 + x[8]^2 - 1)
    f <- rbind(f, x[3]^2 + x[6]^2 + x[9]^2 - 1)
    f <- rbind(f, x[1] * x[2] + x[4] * x[5] + x[7] * x[8] - 0)
    f <- rbind(f, x[1] * x[3] + x[4] * x[6] + x[7] * x[9] - 0)
    f <- rbind(f, x[2] * x[3] + x[5] * x[6] + x[8] * x[9] - 0)
    # Second and third factors do not load on one month rate
    f <- rbind(f, x[4] * loading[1, 1] + x[5] * loading[1, 2] + x[6] * loading[1, 3] - 0)
    f <- rbind(f, x[7] * loading[1, 1] + x[8] * loading[1, 2] + x[9] * loading[1, 3] - 0)
    return(list(ceq = f, c = NULL))
  }

  obj <- function(x) {
    u <- matrix(c(x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9]), nrow = 3)
    factor_idx <- if (extended && window == "release") 2 else 3
    xx <- id$fa %*% u[, factor_idx]
    out <- 0.5 * t(xx) %*% xx / length(xx)
    as.numeric(out)
  }

  sol <- NlcOptim::solnl(c(diag(3)), objfun = obj, confun = con)

  # Rotate factors
  rotate_factors <- factors %*% matrix(sol$par, nrow = 3) %>%
    dplyr::as_tibble(.name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE))

  # Rename and scale based on corresponding ois rate
  rotate_factors <- rename_and_scale_factors(rotate_factors, data, ois_matrix, window, extended)

  factors_scaled <- dplyr::bind_cols(data %>% dplyr::select(date), rotate_factors)

  return(factors_scaled)
}

rename_and_scale_factors <- function(rotate_factors, data, ois_matrix, window, extended) {
  if (window == "release") {
    if (extended) {
      rotate_factors <- rotate_factors %>%
        dplyr::select(1:2) %>%
        dplyr::rename(target = 1, qe = 2)

      full <- bind_cols(data %>% dplyr::select(date), rotate_factors, ois_matrix %>% dplyr::as_tibble(.))
      scale_1 <- coef(lm(ois_1m ~ target, data = full))[2]
      scale_2 <- coef(lm(ois_10y ~ qe, data = full))[2]

      rotate_factors <- rotate_factors %>%
        dplyr::mutate(target = target * scale_1, qe = qe * scale_2)
    } else {
      rotate_factors <- rotate_factors %>%
        dplyr::select(1) %>%
        dplyr::rename(target = 1)

      full <- dplyr::bind_cols(data %>% dplyr::select(date), rotate_factors, ois_matrix %>% dplyr::as_tibble(.))
      scale_1 <- coef(lm(ois_1m ~ target, data = full))[2]

      rotate_factors <- rotate_factors %>%
        dplyr::mutate(target = target * scale_1)
    }
  } else {
    rotate_factors <- rotate_factors %>%
      dplyr::select(1:3) %>%
      dplyr::rename(timing = 1, fg = 2, qe = 3)

    full <- bind_cols(data %>% dplyr::select(date), rotate_factors, ois_matrix %>% dplyr::as_tibble(.))
    scale_4 <- coef(lm(ois_6m ~ timing, data = full))[2]
    scale_5 <- coef(lm(ois_2y ~ fg, data = full))[2]
    scale_6 <- coef(lm(ois_10y ~ qe, data = full))[2]

    rotate_factors <- rotate_factors %>%
      dplyr::mutate(timing = timing * scale_4, fg = fg * scale_5, qe = qe * scale_6)
  }
  return(rotate_factors)
}