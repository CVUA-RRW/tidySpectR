library(testthat)
library(rlang)
library(tidySpectR)

context("Testing average_spectrum")

library(tidyverse)

test_that('average all', {
    
    av_all <- fa_nmr %>%
              average_spectrum(group = 'all') %>%
              .$data
    
    av <- fa_nmr$data %>%
          group_by(bins, bin_start, bin_end) %>%
          summarise(values = mean(values), .groups = 'drop') %>%
          add_column(id = as.factor("all"), .before = 1) 
    
    expect_equal(av, av_all)
    })

test_that('average labels', {
    
    av_lab <- fa_nmr %>%
              average_spectrum(group = 'labels') %>%
              .$data
    
    av <- fa_nmr$data %>%
          inner_join(fa_nmr$labels, by = "id") %>%
          mutate(id = label) %>%
          group_by(id, bins, bin_start, bin_end) %>%
          summarise(values = mean(values), .groups = 'drop') 
    
    expect_equal(av, av_lab)
    })