library(testthat)
library(rlang)
library(tidySpectR)

context("Testing xvast scaling")

library(recipes)

iris_num <- iris %>% 
        select(-Species) %>% 
        tibble::tibble()
means <- vapply(iris_num, mean, c(mean=0))
sds <- vapply(iris_num, sd, c(sd=0))
sdroots <- sqrt(sds)

cvs <- iris %>% 
            group_by(Species) %>%
            summarise(across(where(is.numeric), ~ sd(.x)/mean(.x))) %>%
            summarise(across(where(is.numeric), ~ max(.x))) %>%
            as.matrix()

test_that('autoscale-xvast', {
    processed <- sweep(as.matrix(iris_num), 2, means, "-") %>%
                 sweep(., 2, sds, "/") %>% 
                 sweep(., 2, cvs, "/") %>%
                 tibble::as_tibble()
    
    xvast_res <- recipe(Species ~ ., data = iris) %>%
                  step_xvast(all_predictors(), scaling = "autoscale", outcome = "Species") %>%
                  prep() %>%
                  juice() %>% select(-Species)
    
    expect_equal(processed, xvast_res)
})

test_that('pareto-xvast', {
    processed <- sweep(as.matrix(iris_num), 2, means, "-") %>%
                 sweep(., 2, sdroots, "/") %>% 
                 sweep(., 2, cvs, "/") %>%
                 tibble::as_tibble()
    
    paretoxvast_res <- recipe(Species ~ ., data = iris) %>%
                  step_xvast(all_predictors(), scaling = "pareto", outcome = "Species") %>%
                  prep() %>%
                  juice() %>% select(-Species)
    
    expect_equal(processed, paretoxvast_res)
})