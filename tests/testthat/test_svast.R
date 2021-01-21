library(testthat)
library(rlang)
library(NMRrecipes)

context("Testing svast scaling")

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
            summarise(across(where(is.numeric), ~ mean(.x))) %>%
            as.matrix()

test_that('autoscale-svast', {
    processed <- sweep(as.matrix(iris_num), 2, means, "-") %>%
                 sweep(., 2, sds, "/") %>% 
                 sweep(., 2, cvs, "/") %>%
                 tibble::as_tibble()
    
    svast_res <- recipe(Species ~ ., data = iris) %>%
                  step_svast(all_predictors(), scaling = "autoscale", outcome = "Species") %>%
                  prep() %>%
                  juice() %>% select(-Species)
    
    expect_equal(processed, svast_res)
})

test_that('pareto-svast', {
    processed <- sweep(as.matrix(iris_num), 2, means, "-") %>%
                 sweep(., 2, sdroots, "/") %>% 
                 sweep(., 2, cvs, "/") %>%
                 tibble::as_tibble()
    
    paretosvast_res <- recipe(Species ~ ., data = iris) %>%
                  step_svast(all_predictors(), scaling = "pareto", outcome = "Species") %>%
                  prep() %>%
                  juice() %>% select(-Species)
    
    expect_equal(processed, paretosvast_res)
})