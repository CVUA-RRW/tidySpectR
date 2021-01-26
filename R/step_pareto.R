#' Applies Pareto scaling on numeric data
#'
#' `step_pareto` creates a *specification* of a recipe
#'  step that will perform Pareto scaling on the columns.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose which
#'  variables are affected by the step. See [selections()]
#'  for more details. For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param trained A logical to indicate if the quantities for
#'  preprocessing have been estimated.
#' @param means A named numeric vector of means. This is
#'  `NULL` until computed by [prep.recipe()].
#' @param sdroots A named numeric vector of standard deviation square roots. This
#'  is `NULL` until computed by [prep.recipe()].
#' @param na_rm A logical value indicating whether `NA`
#'  values should be removed when computing the standard deviation and mean.
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [bake.recipe()]? While all operations are baked
#'  when [prep.recipe()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations
#' @param id A character string that is unique to this step to identify it.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  selectors or variables selected), `value` (the
#'  standard deviations and means), and `statistic` for the type of value.
#' @keywords datagen
#' @concept preprocessing
#' @concept normalization_methods
#'
#' @importFrom recipes add_step rand_id ellipse_check step bake prep terms_select
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr group_by summarize select
#' @importFrom generics tidy required_pkgs
#'
#' @export
#' @details Pareto scaling is a variant of autoscaling whereby the data is scaled 
#'  by the square root of its standard deviation. `step_pareto` estimates the standard deviations
#'  and means from the data used in the `training` argument of
#'  `prep.recipe`. `bake.recipe` then applies the scaling to new data sets using
#'  these estimates.
#'
#' @references
#' van den Berg, R. A., Hoefsloot, H. C., Westerhuis, J. A., Smilde, A. K., & 
#' van der Werf, M. J. (2006). Centering, scaling, and transformations: 
#' improving the biological information content of metabolomics data. 
#' BMC genomics, 7, 142. https://doi.org/10.1186/1471-2164-7-142
#' \url{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1534033/}
#'
#' @examples
#' library(tidySpectR)
#' library(recipes)
#' pareto <- 
#'   recipe(Species ~. , iris) %>%
#'   step_pareto(all_predictors())


step_pareto <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           means = NULL,
           sdroots = NULL,
           na_rm = TRUE,
           skip = FALSE,
           id = rand_id("pareto")) {
           
    terms = ellipse_check(...)
    
    add_step(
      recipe,
      step_pareto_new(
        terms = terms,
        role = role,
        trained = trained,
        means = means,
        sdroots = sdroots,
        na_rm = na_rm,
        skip = skip,
        id = id
      )
    )
  }

step_pareto_new <-
  function(terms, role, trained, means, sdroots, na_rm, skip, id) {
    step(
      subclass = "pareto",
      terms = terms,
      role = role,
      trained = trained,
      means = means,
      sdroots = sdroots,
      na_rm = na_rm,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_pareto <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info)

  check_type(training[, col_names])

  means <- vapply(training[, col_names], mean, c(mean = 0), na.rm = x$na_rm)
  sds <- vapply(training[, col_names], sd, c(sd = 0), na.rm = x$na_rm)
  sdroots <- sqrt(sds)
  
  step_pareto_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    means = means,
    sdroots = sdroots,
    na_rm = x$na_rm,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_pareto <- function(object, new_data, ...) {
  res <- sweep(as.matrix(new_data[, names(object$means)]), 2, object$means, "-")
  res <- sweep(res, 2, object$sdroots, "/")
  res <- tibble::as_tibble(res)
  new_data[, names(object$sdroots)] <- res
  as_tibble(new_data)
}

#' @export
print.step_pareto <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Pareto scaling for ", sep = "")
    printer(names(x$sdroots), x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_pareto
#' @param x A `step_pareto` object.
#' @export
tidy.step_pareto <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = c(names(x$means), names(x$sdroots)),
                  statistic = rep(c("mean", "sdroot"), each = length(x$means)),
                  value = c(x$means, x$sdroots))
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names,
                  statistic = rlang::na_chr,
                  value = rlang::na_dbl)
  }
  res$id <- x$id
  res
}

#' @rdname required_pkgs.step
#' @export
required_pkgs.step_pareto <- function(x, ...) {
  c("NMRrecipes")
}