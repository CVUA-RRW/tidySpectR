library(testthat)
library(rlang)
library(tidySpectR)

context("Testing collection")

library(tidyverse)

test_that('collection instanciation', {
    coll <- collection() 
    expect_s3_class(coll, "collection")
})

coll <- collection() %>%
        add_spectrum(c(1,1,1,1,1), c(0,4), id = "sample", label = "test")

coll2sample <- coll %>%
        add_spectrum(c(1,1,1,1,1,1), c(0,5), id = "sample2", label = "test_2")
        
test_that('add_spectrum',{
    datadf <- tibble(id = "sample",
                     bins = c(0,1,2,3,4),
                     bin_start = bins -0.5,
                     bin_end = bins + 0.5,
                     values = c(1,1,1,1,1)) %>%
               mutate(id = as.factor(id))
    
    expect_equal(datadf, coll$data)
    
    labeldf <- tibble(id = "sample",
                     label = "test") %>%
              mutate(id = as.factor(id),
                     label = as.factor(label))
    
    expect_equal(labeldf, coll$labels)
})

test_that('pull_numbin',{
    numbin <- tibble(id = c("sample", "sample2"),
                     numbin = c(5, 6)) %>%
              mutate(id = as.factor(id), 
                     numbin = as.integer(numbin))
                     
    expect_equal(numbin, pull_numbin(coll2sample))
})

test_that('pull_breaks',{
    breaks <- c(0,1,2,3,4,5) -0.5
    breaks <- append(breaks, 5.5)
    expect_equal(pull_breaks(coll2sample), breaks)
})

test_that('pull_ids',{
    expect_equal(pull_ids(coll2sample), c("sample", "sample2"))
})

test_that('tidy',{
    tidy_df <- tibble(id = c("sample", "sample2"),
                      label = c("test", "test_2"),
                      `0` = c(1,1),
                      `1` = c(1,1),
                      `2` = c(1,1),
                      `3` = c(1,1),
                      `4` = c(1,1),
                      `5` = c(NA,1)) %>%
               mutate(id = as.factor(id),
                      label = as.factor(label))
    
    expect_equal(tidy_df, tidy(coll2sample))
})