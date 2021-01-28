library(testthat)
library(rlang)
library(tidySpectR)

context("Testing normalization")

library(tidyverse)

coll <- collection() %>% 
        add_spectrum(c(10,10,10,10,10), left=0, right=2, id ="test")

test_that('normalize_factor', {
    norm <- coll$data %>%
                mutate(values = values * 0.5)
    
    coll_norm <- coll %>%
                 normalize_factor(tibble(id = "test", factors = 0.5))
    
    expect_equal(norm, coll_norm$data)
})

test_that('normalize_totalSpectrum', {
    norm <- coll$data %>%
                mutate(values = values * 1/sum(values))
    
    coll_norm <- coll %>%
                 normalize_totalSpectrum()
                 
    expect_equal(norm, coll_norm$data)
})

test_that('normalize_internalStandard', {
    integral <- coll %>% 
                extract(from=0, to= 0.5) %>%
                .$data %>%
                pull(values) %>% 
                sum()
        
    norm <- coll$data %>%
                mutate(values = values * 1/integral)
    
    coll_norm <- coll %>%
                 normalize_internalStandard(from = 0, to = 0.5)
                 
    expect_equal(norm, coll_norm$data)
})