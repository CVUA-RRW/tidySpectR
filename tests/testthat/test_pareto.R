library(testthat)
library(rlang)
library(tidySpectR)

context("Testing pareto scaling")

library(tidyverse)
library(recipes)

iris_num <- iris %>% 
        select(-Species) %>% 
        tibble::tibble()
means <- vapply(iris_num, mean, c(mean=0))
sds <- vapply(iris_num, sd, c(sd=0))
sdroots <- sqrt(sds)

test_that('correct processing', {
    processed <- sweep(as.matrix(iris_num), 2, means, "-") %>%
                 sweep(., 2, sdroots, "/") %>% 
                 tibble::as_tibble()
    
    pareto_res <- recipe(Species ~ ., data = iris) %>%
                  step_pareto(all_predictors()) %>%
                  prep() %>%
                  juice() %>% select(-Species)
    
    expect_equal(processed, pareto_res)
})