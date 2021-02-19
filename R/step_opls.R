#' Applies OPLS noise filtering on numeric data
#'
#' `step_opls_denoise` creates a 'specification' of a recipe 
#'   step that will filter the first orthogonal component of the OPLS
#'   transfomation on the columns.
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
#' @param outcome When a single outcome is available, character
#'  string or call to [dplyr::vars()] can be used to specify a single outcome
#'  variable.
#' @param Wortho A vector a weights for the first orthogonal component. This is
#'  `NULL` until computed by [prep.recipe()].
#' @param Portho A vector of loadings for the first orthogonal component. This is
#'  `NULL` until computed by [prep.recipe()].
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
#'
#' @importFrom recipes add_step rand_id ellipse_check step bake prep 
#' @importFrom recipes printer terms_select check_type is_trained sel2char
#' @importFrom tibble tibble as_tibble 
#' @importFrom generics tidy required_pkgs
#'
#' @export
#' @details 
#' Orthogonal Projection to Latent Structurees (OPLS) allows the separation 
#'   of the predictor variations that are correlated and orthogonal to the response.
#'   This allows to remove systematic variation that are not correlated to the response.
#'
#' The OPLS algorithm is implemented only for binary outcomes!
#'
#' OPLS calculation uses the implementation of the R package:
#' \url{https://bioconductor.org/packages/release/bioc/html/ropls.html}
#'
#'
#' @references
#' Trygg, J., & Wold, S. (2002). Orthogonal projections to latent structures 
#' (O-PLS). Journal of Chemometrics, 16(3), 119–128. doi:10.1002/cem.695 
#' \url{https://onlinelibrary.wiley.com/doi/abs/10.1002/cem.695}
#'
#' Thévenot, E. A., Roux, A., Xu, Y., Ezan, E., & Junot, C. (2015). Analysis 
#' of the Human Adult Urinary Metabolome Variations with Age, Body Mass Index, 
#' and Gender by Implementing a Comprehensive Workflow for Univariate and OPLS 
#' Statistical Analyses. Journal of Proteome Research, 14(8), 3322–3335. 
#' doi:10.1021/acs.jproteome.5b00354 
#' \url{https://pubs.acs.org/doi/10.1021/acs.jproteome.5b00354}
#'
#' @examples
#' library(ropls)
#' library(tidymodels)
#' library(tidySpectR)
#'
#' data(sacurine)
#' attach(sacurine)
#'
#' genderFc <- sampleMetadata[, "gender"]
#'
#' urinedata <- dataMatrix %>% 
#'    cbind(genderFc) %>% 
#'    as_tibble() %>% 
#'    add_column(id = rownames(dataMatrix), .before = 1) %>%
#'    select(-id)
#'
#' rec <- recipe(urinedata, genderFc ~.) %>%
#'      step_normalize(all_predictors()) %>%
#'      step_opls_denoise(all_predictors(), outcome = "genderFc") 
#' tidy(rec)
#' rec %>% prep() %>% bake(NULL)
step_opls_denoise <- 
    function(recipe,
             ...,
             role = NA,
             trained = FALSE, 
             outcome = NULL,
             Wortho = NULL,
             Portho = NULL,
             skip = FALSE,
             id = rand_id("opls_denoise")){
             
    if (is.null(outcome)) {
        rlang::abort("`outcome` should select one column.")
    }
    
    terms = ellipse_check(...)
    
    add_step(
        recipe,
        step_opls_denoise_new(
            terms = terms,
            role = role,
            trained = trained,
            outcome = outcome,
            Wortho = Wortho,
            Portho = Portho,
            skip = skip,
            id = id
        )
    )
}

step_opls_denoise_new <- 
    function(terms, role, trained, outcome, Wortho, Portho, skip, id){
    
    step(
        subclass = "opls_denoise",
        terms = terms,
        role = role,
        trained = trained,
        outcome = outcome,
        Wortho = Wortho,
        Portho = Portho,
        skip = skip,
        id = id
    )
}

#' @importFrom ropls opls getWeightMN getLoadingMN
#' @importFrom dplyr select
#' @export
prep.step_opls_denoise <- function(x, training, info = NULL, ...){

    col_names <- terms_select(x$terms, info)
    
    check_type(training[, col_names])

    predictors <- training[, col_names]
    outcomes <- select(training, x$outcome) %>% as.matrix()
    
    model <- opls(predictors, outcomes, predI = 1, orthoI = 1)
    
    Wortho <- getWeightMN(model, orthoL = TRUE)
    Portho <- getLoadingMN(model, orthoL = TRUE)

    step_opls_denoise_new(
        terms = x$terms,
        role = x$role,
        trained = TRUE,
        outcome = x$outcome,
        Wortho = Wortho,
        Portho = Portho,
        skip = x$skip,
        id = x$identify
    )
}

#' @importFrom dplyr bind_cols
#' @importFrom tibble as_tibble
#' @export
bake.step_opls_denoise <- function(object, new_data, ...){
    opls_vars <- rownames(object$Wortho)
    dat <- new_data[, opls_vars] %>%
            as.matrix()
    
    # Calculate new scores and remove noise
    Tortho <- dat %*% object$Wortho
    res <- dat - Tortho %*% t(object$Portho)

    # Update data 
    new_data <- new_data[, !(colnames(new_data) %in% opls_vars), drop = FALSE]
    new_data <- bind_cols(new_data, as_tibble(res))
    as_tibble(new_data)
}

#' @export
print.step_opls_denoise <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("OPLS denoising for ", sep = "")
    printer(rownames(x$Wortho), x$terms, x$trained, width = width)
    invisible(x)
  }
  
#' @rdname step_opls_denoise
#' @param x A `step_opls_denoise` object.
#' @export
tidy.step_opls_denoise <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = c(rownames(x$Wortho)),
                  statistic = rep(c("orthogonal weigths", "orthogonal loadings"), each = length(x$Wortho)),
                  value = c(x$Wortho, x$Portho))
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
required_pkgs.step_opls_denoise <- function(x, ...) {
  c("tidySpectR")
}
