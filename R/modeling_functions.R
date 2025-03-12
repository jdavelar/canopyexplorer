#' Prepare data for Bayesian modeling
#'
#' This function prepares Canopy data for modeling by pivoting wide tag columns
#' into long format and scaling numeric covariates specified.
#'
#' @param data A dataframe of Canopy data including tags and covariates.
#' @param prefix The prefix used for the tags to identify relevant variables.
#' @param id A column of unique identifiers for schools. Default is `"school_id"`.
#' @param covariates A vector of covariates to include in the model.
#'
#' @return A prepped dataframe ready for modeling.
#'
#' @seealso
#' \code{\link{tag_bayes}} to run Bayesian models on the prepped data.
#'
#' @export
tag_bayes_prep <- function(
    data,
    prefix = "practices_",
    id = "school_id",
    covariates
) {
  # Identify relevant tag columns
  tag_cols <- grep(paste0("^", prefix), names(data), value = TRUE)

  if (length(tag_cols) < 1) {
    stop("No tag variables found with the specified prefix.")
  }

  # Subset and pivot
  mod_prep <- data %>%
    dplyr::select(dplyr::all_of(c(id, covariates, tag_cols))) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(tag_cols),
      names_to = "tag",
      values_to = "value"
    ) %>%
    stats::na.omit()

  # Ensure outcome is binary (0/1)
  if (any(!mod_prep$value %in% c(0, 1))) {
    stop("Outcome variables must be coded as 0/1. Please recode before running the function.")
  }

  # Scale numeric covariates only (skip non-numeric)
  numeric_covs <- covariates[sapply(data[covariates], is.numeric)]

  if (length(numeric_covs) > 0) {
    message("Scaling numeric covariates:")
    message(numeric_covs)

    mod_prep <- mod_prep %>%
      dplyr::mutate(across(all_of(numeric_covs), ~ as.numeric(scale(.))))
  }

  return(mod_prep)
}


#' Run Bayesian GLM models for tags
#'
#' This function runs a Bayesian GLM model for each tag using prepped Canopy data
#' and specified covariates. The model is fitted using `rstanarm::stan_glm()`
#' and returns tidy results.
#'
#' @param data A dataframe of prepped data from `tag_bayes_prep`.
#' @param covariates A vector of covariates to include in the model.
#' @param exponentiate Logical. If `TRUE`, exponentiates the output to show odds ratios
#' rather than log-odds. Default is `FALSE`.
#'
#' @return A tidy dataframe of model output.
#'
#' @seealso
#' \code{\link{tag_bayes_prep}} to prepare data for modeling.
#' \code{\link{tag_bayes_plot}} to plot the model results.
#'
#' @export
tag_bayes <- function(
    data,
    covariates,
    exponentiate = FALSE
) {
  if (!requireNamespace("rstanarm", quietly = TRUE)) {
    stop("The `rstanarm` package is required for this function. Please install it using `install.packages('rstanarm')`.")
  }

  if (!"tag" %in% names(data) || !"value" %in% names(data)) {
    stop("Data is not prepped correctly. Please use `tag_bayes_prep` to prepare the data.")
  }

  # Create model formula
  form <- as.formula(paste("value", "~", paste(covariates, collapse = " + ")))

  # Pull unique tags
  tags <- unique(data$tag)

  # Run models — loop through each tag
  bayes_mods <- purrr::map(
    setNames(tags, tags),
    ~ rstanarm::stan_glm(
      formula = form,
      data = dplyr::filter(data, tag == .x),
      family = binomial(link = "logit"),
      prior = rstanarm::student_t(df = 7, location = 0, scale = 2.5),
      seed = 123
    )
  )

  # Remove NULL models
  bayes_mods <- bayes_mods[!sapply(bayes_mods, is.null)]

  if (length(bayes_mods) == 0) {
    stop("No valid models were created. Please check data.")
  }

  # Tidy Results (with confidence intervals)
  bayes_tidy <- bayes_mods %>%
    purrr::map_dfr(
      ~ broom.mixed::tidy(.x, conf.int = TRUE),
      .id = "tag"
    ) %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::mutate(
      estimate = if (exponentiate) exp(estimate) else estimate,
      conf.low = if (exponentiate) exp(conf.low) else conf.low,
      conf.high = if (exponentiate) exp(conf.high) else conf.high
    )

  return(bayes_tidy)
}


#' Plot results from `tag_bayes`
#'
#' A function that creates a plot of the top `n` absolute effects for a selected
#' tag from the results of `tag_bayes`. If no tag is specified, the function
#' will select the tag with the largest absolute effect.
#'
#' @param data A data frame of model results created by `tag_bayes`.
#' @param tag A specific tag to plot. If `NULL`, the tag with the largest
#'   absolute effect will be selected automatically. Default is `NULL`.
#' @param n Number of terms to show in the plot. Default is `10`.
#' @param theme Character; `"crpe"`, `"transcend"`, or `"minimal"` to apply a
#'   specific theme to the plot. Default is `"crpe"`.
#'
#' @return A ggplot object showing the top `n` absolute effects for the specified tag.
#'
#' @seealso
#' \code{\link{tag_bayes}} to create the model results used in this plot.
#'
#' @export
tag_bayes_plot <- function(
    data,
    tag = NULL,
    n = 10,
    exponentiate = FALSE,
    theme = "crpe"
) {
  if (!"tag" %in% names(data) || !"term" %in% names(data) || !"estimate" %in% names(data)) {
    stop("Data is not correctly formatted. Please use tag_bayes() to generate the input.")
  }

  # If tag is NULL, select the tag with the largest absolute effect
  if (is.null(tag)) {
    tag <- data %>%
      dplyr::filter(!is.na(estimate)) %>%
      dplyr::arrange(desc(abs(estimate))) %>%
      dplyr::slice(1) %>%
      dplyr::pull(tag)

    message(paste("Selecting tag with largest effect:", tag))
  }

  # Subset to the selected tag and take top n terms by absolute effect
  plot_data <- data %>%
    dplyr::filter(tag == !!tag) %>%
    dplyr::arrange(desc(abs(estimate))) %>%
    dplyr::slice_head(n = n)

  if (nrow(plot_data) == 0) {
    stop("No valid terms to plot for this tag.")
  }

  # Exponentiate values if specified
  if (exponentiate) {
    plot_data <- plot_data %>%
      dplyr::mutate(
        estimate = exp(estimate),
        conf.low = exp(conf.low),
        conf.high = exp(conf.high)
      )
  }

  # Create plot
  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = estimate,
      y = forcats::fct_reorder(term, estimate)
    )
  ) +
    ggplot2::geom_point(size = 3) + # Points instead of bars
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = conf.low, xmax = conf.high),
      height = 0.3
    ) +
    ggplot2::labs(
      x = if (exponentiate) "Odds Ratio" else "Estimate",
      y = "",
      title = paste0("Estimates for ", label_tags(tag)[[1]]),
      caption = if (exponentiate) "Odds ratio > 1 = more likely; < 1 = less likely" else ""
    ) +
    ggplot2::scale_x_continuous(
      trans = if (exponentiate) "log" else "identity"
    ) +
    ggplot2::geom_vline(xintercept = if (exponentiate) 1 else 0, linetype = "dashed", color = "gray")

  # Apply theme
  if (theme == "crpe") {
    plot <- plot + theme_crpe()
  } else if (theme == "transcend") {
    plot <- plot + theme_transcend()
  } else {
    plot <- plot + ggplot2::theme_minimal()
  }

  return(plot)
}
