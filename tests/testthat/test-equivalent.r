library(testthat)

context("Equivalence testing")

data(iris)

a <- rnorm(10)
b <- 2 * a + 4
# TRUE because they are the same (up to an affine transformation).
expect_true(equiv(a, b))

a <- c("a", "a", "b", "c")
b <- c("b", "b", "a", "c")
# TRUE because they are the same (up to a label change).
expect_true(equiv(a, b))

expect_true(equiv(factor(a), b))

expect_false(equiv(factor(a), b, factor_equiv_character = FALSE))

expect_true(equiv(factor(a), factor(b)))

iris$Sepal.Length2 <- 3 * iris$Sepal.Length + 3

equiv_col_matrix(iris)

iris$Sepal.Length3 <- 4 * iris$Sepal.Length + 3

has_equiv_column(iris, "Sepal.Length2")

iris$Petal.Length2 <- 4 * iris$Petal.Length+ 3
has_equiv_column(iris, "Sepal.Length2")

al <- c(list(c("a", "a"), c("b", "b")))
bl <- c(list(c("b", "a"), c("b", "b")))


expect_true(equiv(NULL, NULL))

expect_true(equiv(al, al))

expect_false(equiv(al, bl))
