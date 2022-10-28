library(MASS)


test_that("Correct beta values", {
  mod1 <- lm.ridge(data, formula=Petal.Length~Species, lambda=0)
  mod2 <- ridgereg(data, formula=Petal.Length~Species, lambda=0)
  print1<-as.matrix(coef(mod1))
  row.names(print1)[1] <- "(Intercept)"
  print2<-mod2$coef()
  expect_equal(print1,print2)
})
