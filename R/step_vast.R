#' Applies VAST scaling on numeric data
#'
#' `step_vast` creates a *specification* of a recipe
#'  step that will perform VAST scaling on the columns
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose which
#'  variables are affected by the step. See [selections()]
#'  for more details. For the `tidy` method, these are not
#'  currently used.
#' @param scaling Either `autoscale` or `pareto`. Controls the scaling method. 
#'  See notes below.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param trained A logical to indicate if the quantities for
#'  preprocessing have been estimated.
#' @param means A named numeric vector of means. This
#'  is `NULL` until computed by [prep.recipe()].
#' @param sds A named numeric vector of stadard deviations. This
#'  is `NULL` until computed by [prep.recipe()].
#' @param cvs A named numeric vector of variation coeficients. This
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
#' @details Variable Stability (VAST) scaling preforms centering and scaling followed 
#' by a weighting of each variable by its variation coeficient. 
#' 
#' The argument `scaling` controls which scaling method should be used before
#' variable weighting. `autoscale` will perform mean-centering and standard deviation
#' scaling while `pareto` will scale by the square-root of the standard deviation.
#'
#' @references
#' Keun H. C., Ebbels T. M. D., Antti H., Bollard M. E., Beckonert O., Holmes E., 
#' et al. (2003). Improved analysis of multivariate data by variable stability 
#' scaling: application to NMR-based metabolic profiling. Anal. Chim. Acta 490, 
#' 265–276 10.1016/S0003-2670(03)00094-1 
#' \url{https://www.sciencedirect.com/science/article/abs/pii/S0003267003000941}
#'
#' @examples
#' library(tidySpectR)
#' library(recipes)
#' autoscale_vast <- 
#'   recipe(Species ~. , iris) %>%
#'   step_vast(all_predictors(), scaling = 'autoscale')
#'
#' pareto_vast <- 
#'   recipe(Species ~. , iris) %>%
#'   step_vast(all_predictors(), scaling = 'pareto')

step_vast <-
  function(recipe,
           ...,
           scaling = "autoscale",
           role = NA,
           trained = FALSE,
           means = NULL,
           sds = NULL,
           cvs = NULL,
           na_rm = TRUE,
           skip = FALSE,
           id = rand_id("vast")) {
           
    terms = ellipse_check(...)
    
    if (!scaling %in% c("autoscale", "pareto")){
        rlang::abort("`scaling` should be either `autoscale` or `pareto`.")
    }
    
    add_step(
      recipe,
      step_vast_new(
        terms = terms,
        scaling = scaling,
        role = role,
        trained = trained,
        means = means,
        sds = sds,
        cvs = cvs,
        na_rm = na_rm,
        skip = skip,
        id = id
      )
    )
  }

step_vast_new <-
  function(terms, scaling, role, trained, means, sds, cvs, na_rm, skip, id) {
    step(
      subclass = "vast",
      terms = terms,
      scaling = scaling,
      role = role,
      trained = trained,
      means = means,
      sds = sds,
      cvs = cvs,
      na_rm = na_rm,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_vast <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info)

  check_type(training[, col_names])

  means <- vapply(training[, col_names], mean, c(mean = 0), na.rm = x$na_rm)
  sds <- vapply(training[, col_names], sd, c(sd = 0), na.rm = x$na_rm)
  cvs <- sds / means
  
  step_vast_new(
    terms = x$terms,
    scaling = x$scaling,
    role = x$role,
    trained = TRUE,
    means = means,
    sds = sds,
    cvs = cvs,
    na_rm = x$na_rm,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_vast <- function(object, new_data, ...) {
  # centering
  res <- sweep(as.matrix(new_data[, names(object$means)]), 2, object$means, "-")
  
  # scaling (normale or pareto)
  if (object$scaling == "autoscale") {
    res <- sweep(res, 2, object$sds, "/")
  } else if (object$scaling == "pareto") {
    sdroots <- sqrt(object$sds)
    res <- sweep(res, 2, sdroots, "/")
  }
  
  # Weigthing by CV
  res <- sweep(res, 2, object$cvs, "/")
  
  # Returning processed tibble
  res <- tibble::as_tibble(res)
  new_data[, names(object$sds)] <- res
  as_tibble(new_data)
}

#' @export
print.step_vast <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("VAST scaling (", x$scaling ,") for ", sep = "")
    printer(names(x$means), x$terms, x$trained, width = width)
    invisible(x)
  }


#' @rdname step_vast
#' @param x A `step_vast` object.
#' @export
tidy.step_vast <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = c(names(x$means), names(xsds), names(x$cvs)),
                  statistic = rep(c("mean", "sd", "cv"), each = length(x$sds)),
                  value = c(x$means, x$sds, x$cvs))
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
required_pkgs.step_vast <- function(x, ...) {
  c("NMRrecipes")
}