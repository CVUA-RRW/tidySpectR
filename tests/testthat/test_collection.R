library(testthat)
library(rlang)
library(tidySpectR)

context("Testing collection")

library(tidyverse)

test_that('collection instanciation', {
    coll <- collection() 
    expect_s3_class(coll, "collection")
})

test_that('add_spectrum and pull_ids',{
    sample <- nmr_spectra %>% filter(ID == "20198124123")

    coll_one <- collection() %>%
        add_spectrum(sample$values, 
                    c(min(sample$bins), max(sample$bins)),
                    id = "20198124123")
                    
    expect_equal(pull_ids(coll_one), "20198124123")
    coll_many <- collection()
    
    nmr_spectra %>% 
    group_by(ID) %>%
    group_walk(~ {coll_many <<- coll_many %>% 
                   add_spectrum(.x$values, 
                                c(min(.x$bins), max(.x$bins)),
                                id = .y$ID)})
    
    samples <- nmr_spectra %>% 
        select(ID) %>% 
        unique() %>%
        pull(ID)
    
    expect_equal(pull_ids(coll_many), samples)
})

test_that('pull_numbin',{
})

test_that('pull_breaks',{
})

test_that('tidy',{
})