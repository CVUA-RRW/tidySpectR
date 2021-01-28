library(testthat)
library(rlang)
library(tidySpectR)

context("Testing add_labels")

library(tidyverse)

test_that('add_labels', {
    
    new_labs = tibble(id = c("20199305928", "20199306281", "20199504645",
                        "20198124123", "20198124124", "20198124125"),
                      label = c("foo", "bar", "foo", "bar", "foo", "bar")) %>%
               mutate(id = as.factor(id),
                      label = as.factor(label))
    
    fa_nmr <- fa_nmr %>% add_labels(new_labs, ids_from = 'id', labels_from = 'label')
    
    expect_equal(fa_nmr$labels, new_labs)
})
