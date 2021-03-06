#' Applies x-VAST scaling on numeric data
#'
#' `step_svast` creates a *specification* of a recipe
#'  step that will perform x-VAST scaling on the columns
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
#' @param outcome When a single outcome is available, character
#'  string or call to [dplyr::vars()] can be used to specify a single outcome
#'  variable.
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
#' @importFrom recipes add_step rand_id ellipse_check step bake prep 
#' @importFrom recipes printer terms_select check_type is_trained sel2char
#' @importFrom tibble tibble as_tibble 
#' @importFrom dplyr group_by summarize select
#' @importFrom generics tidy required_pkgs
#' @importFrom stats sd
#'
#' @export
#' @details supervised maximum Variable Stability (x-VAST) scaling preforms centering and scaling followed 
#' by a weighting of each variable by the maximum of the class-specific variation coeficients. 
#' 
#' The argument `scaling` controls which scaling method should be used before
#' variable weighting. `autoscale` will perform mean-centering and standard deviation
#' scaling while `pareto` will scale by the square-root of the standard deviation.
#'
#' @references
#' Yang, J., Zhao, X., Lu, X., Lin, X., & Xu, G. (2015). A data preprocessing 
#' strategy for metabolomics to reduce the mask effect in data analysis. 
#' Frontiers in molecular biosciences, 2, 4. https://doi.org/10.3389/fmolb.2015.00004
#' \url{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4428451/}
#'
#' @examples
#' library(tidySpectR)
#' library(recipes)
#' autoscale_xvast <- 
#'   recipe(Species ~. , iris) %>%
#'   step_xvast(all_predictors(), scaling = 'autoscale', outcome = 'Species')
#'
#' pareto_svast <- 
#'   recipe(Species ~. , iris) %>%
#'   step_xvast(all_predictors(), scaling = 'pareto', outcome = 'Species')

step_xvast <-
  function(recipe,
           ...,
           scaling = "autoscale",
           role = NA,
           trained = FALSE,
           outcome = NULL,
           means = NULL,
           sds = NULL,
           cvs = NULL,
           na_rm = TRUE,
           skip = FALSE,
           id = rand_id("xvast")) {
    
    if (is.null(outcome)) {
        rlang::abort("`outcome` should select at least one column.")
    }
    
    if (!scaling %in% c("autoscale", "pareto")){
        rlang::abort("`scaling` should be either `autoscale` or `pareto`.")
    }
    
    terms = ellipse_check(...)
    
    add_step(
      recipe,
      step_xvast_new(
        terms = terms,
        scaling = scaling,
        role = role,
        trained = trained,
        outcome = outcome,
        means = means,
        sds = sds,
        cvs = cvs,
        na_rm = na_rm,
        skip = skip,
        id = id
      )
    )
  }

step_xvast_new <-
  function(terms, scaling, role, trained, outcome, means, sds, cvs, na_rm, skip, id) {
    step(
      subclass = "xvast",
      terms = terms,
      scaling = scaling,
      role = role,
      trained = trained,
      outcome = outcome,
      means = means,
      sds = sds,
      cvs = cvs,
      na_rm = na_rm,
      skip = skip,
      id = id
    )
  }

get_max_cv <- function(x, outcome, na_rm){
    res <- tibble(x=x, outcome = outcome)
    
    res <- res %>%
            group_by(outcome) %>%
            summarize(cvs = sd(x, na.rm = na_rm)/mean(x, na.rm = na_rm)) 
            
    max(res$cvs)
}

#' @export
prep.step_xvast <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info)

  check_type(training[, col_names])
  
  outcome <- training %>% select(x$outcome)
  means <- vapply(training[, col_names], mean, c(mean = 0), na.rm = x$na_rm)
  sds <- vapply(training[, col_names], sd, c(sd = 0), na.rm = x$na_rm)
  cvs <- vapply(training[, col_names], get_max_cv, c(get_max_cv = 0), outcome = outcome, na_rm = x$na_rm)
  
  step_xvast_new(
    terms = x$terms,
    scaling = x$scaling,
    role = x$role,
    trained = TRUE,
    outcome = x$outcome,
    means = means,
    sds = sds,
    cvs = cvs,
    na_rm = x$na_rm,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_xvast <- function(object, new_data, ...) {
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
  res <- as_tibble(res)
  new_data[, names(object$sds)] <- res
  as_tibble(new_data)
}

#' @export
print.step_xvast <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("VAST scaling (", x$scaling ,") for ", sep = "")
    printer(names(x$cvs), x$terms, x$trained, width = width)
    invisible(x)
  }


#' @rdname step_xvast
#' @param x A `step_xvast` object.
#' @export
tidy.step_xvast <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = c(names(x$cvs)),
                  statistic = rep(c("cv"), each = length(x$cvs)),
                  value = c(x$cvs))
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
required_pkgs.step_xvast <- function(x, ...) {
  c("tidySpectR")
}
