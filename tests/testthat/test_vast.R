library(testthat)
library(rlang)
library(tidySpectR)

context("Testing vast scaling")

library(recipes)

iris_num <- iris %>% 
        select(-Species) %>% 
        tibble::tibble()
means <- vapply(iris_num, mean, c(mean=0))
sds <- vapply(iris_num, sd, c(sd=0))
sdroots <- sqrt(sds)
cvs <- sds/means

test_that('autoscale-vast', {
    processed <- sweep(as.matrix(iris_num), 2, means, "-") %>%
                 sweep(., 2, sds, "/") %>% 
                 sweep(., 2, cvs, "/") %>%
                 tibble::as_tibble()
    
    vast_res <- recipe(Species ~ ., data = iris) %>%
                  step_vast(all_predictors(), scaling = "autoscale") %>%
                  prep() %>%
                  juice() %>% select(-Species)
    
    expect_equal(processed, vast_res)
})

test_that('pareto-vast', {
    processed <- sweep(as.matrix(iris_num), 2, means, "-") %>%
                 sweep(., 2, sdroots, "/") %>% 
                 sweep(., 2, cvs, "/") %>%
                 tibble::as_tibble()
    
    paretovast_res <- recipe(Species ~ ., data = iris) %>%
                  step_vast(all_predictors(), scaling = "pareto") %>%
                  prep() %>%
                  juice() %>% select(-Species)
    
    expect_equal(processed, paretovast_res)
})