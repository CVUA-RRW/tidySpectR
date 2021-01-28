library(testthat)
library(rlang)
library(tidySpectR)

context("Testing masking and extraction")

library(tidyverse)

coll <- collection() %>%
    add_spectrum(c(1,1,1,1,1,1), left = 0, right = 5, id = "sample", label = "test")
    
test_that("mask", {
    mask1 <- coll %>% 
             mask(-Inf, 4, overlaps = "keep") %>% 
             pull_breaks()
             
    mask2 <- coll %>% 
             mask(-Inf, 4, overlaps = "remove") %>% 
             pull_breaks()
             
    mask3 <- coll %>% 
             mask(-Inf, 4.5, overlaps = "keep") %>% 
             pull_breaks()
             
    mask4 <- coll %>% 
             mask(-Inf, 4.5, overlaps = "remove") %>% 
             pull_breaks()
             
    mask5 <- coll %>% 
             mask(1.5, Inf, overlaps = "keep") %>% 
             pull_breaks()
             
    mask6 <- coll %>% 
             mask(1.5, Inf, overlaps = "remove") %>% 
             pull_breaks()
             
    expect_equal(mask1, c(3.5, 4.5, 5.5))
    expect_equal(mask2, c(4.5, 5.5))
    expect_equal(mask3, c(3.5, 4.5, 5.5))
    expect_equal(mask4, c(4.5, 5.5))
    expect_equal(mask5, c(-0.5, 0.5, 1.5))
    expect_equal(mask6, c(-0.5, 0.5))
})

test_that("extract", {
    mask1 <- coll %>% 
             extract(2, 4, overlaps = "keep") %>% 
             pull_breaks()
             
    mask2 <- coll %>% 
             extract(2, 4, overlaps = "remove") %>% 
             pull_breaks()
             
    mask3 <- coll %>% 
             extract(1.5, 4.5, overlaps = "keep") %>% 
             pull_breaks()
             
    mask4 <- coll %>% 
             extract(1.5, 4.5, overlaps = "remove") %>% 
             pull_breaks()
             
    expect_equal(mask1, c(1.5, 2.5, 3.5, 4.5))
    expect_equal(mask2, c(2.5, 3.5))
    expect_equal(mask3, c(1.5, 2.5, 3.5, 4.5))
    expect_equal(mask4, c(2.5, 3.5))
})