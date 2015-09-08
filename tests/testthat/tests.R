library(dplyr)
test_that("cluster.vcov will drop factors that are empty", {
    expect_equal({
      data(petersen)
      petersen <- petersen %>% sample_n(500)
      
      petersen$firmid <- factor(petersen$firmid)
      
      # Drop a factor
      petersen <- petersen[petersen$firmid != petersen$firmid[1],]
      
      # This works fine and firmid500 is not estimated.
      m1 <- lm(y ~ x + firmid, data = petersen)
      
      # This fails
      vcov_firm <- cluster.vcov(m1, petersen$firmid)
      vcov_firm %>% is.na() %>% all()
    }, FALSE)
})

test_that("cluster.boot is compatible with factors", {
  expect_equal({
    data(petersen)
    petersen <- petersen %>% sample_n(500)
    petersen <- petersen %>% mutate(firmid = factor(firmid))
    # This works fine.
    m1 <- lm(y ~ x + firmid, data = petersen)
    
    # This fails
    vcov_firm <- cluster.boot(m1, petersen$firmid, R = 3)
    vcov_firm %>% is.na() %>% all()
  }, FALSE)
})

test_that("cluster.boot will handle factor variables", {
  expect_equal({
    data(petersen)
    petersen <- petersen %>% sample_n(500)
    petersen <- petersen %>% filter(firmid != petersen$firmid[1])
    petersen <- petersen %>% mutate(firmid = factor(firmid),
                                    x2 = x^2)
    # This works fine.
    my_formula_tmp <- as.formula(y ~ x + x2 + firmid)
    m1 <- lm(my_formula_tmp, data = petersen)
    
    # This fails
    vcov_firm <- cluster.boot(m1, as.numeric(petersen$firmid), R = 3)
    vcov_firm %>% is.na() %>% all()
  }, FALSE)
})

