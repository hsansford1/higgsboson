test_that("Ns and Nb return values we expect", {
  expect_equal(Ns(), 691.988607711987)
  expect_equal(Nb(), 410999.847321811)
})

test_that("Reweighting gives weights of signal (background) summing to Ns (Nb)", {
  w <- runif(100, 1, 2)
  l <- rep(0:1, 50)
  rw <- reweight(w, l, Ns(), Nb())
  expect_equal( sum(rw[l==1]) , Ns())
  expect_equal( sum(rw[l==0]) , Nb())
})

test_that("AMS_base behaves correctly", {

  expect_equal( AMS_base(0, 123) , 0)               # zero if s==0

  expect_true( AMS_base(1, 123) < AMS_base(2, 123)) # increasing in s
  expect_true( AMS_base(123, 2) < AMS_base(123, 1)) # decreasing in b
  expect_true( AMS_base(123, 123, b_reg=2) < AMS_base(123, 123, b_reg=1)) # decreasing in b_reg
})

test_that("AMS_weighted==0 if no events predicted signal", {

  w <- runif(50, 1, 2)
  r <- rep(0, 50)
  t <- rep(0:1, 25)
  expect_equal( AMS_weighted(t, r, w), 0)
})

test_that("AMS_measure returns object of class 'Measure'", {
  expect_equal( attr(AMS_measure(), 'class'), 'Measure')
})
