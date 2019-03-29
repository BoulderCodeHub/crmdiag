library(RWDataPlyr)
library(testthat)
library(assertr)
library(validate)
library(tidyverse)
library(miniCRAN)

# test how the three libraries would report the same error/success differently
# check that morrow point and blue mesa outflow are >= 0
# morrow point should fail, blue mesa should succeed

rdf <- rdf_to_rwtbl2("inst/UBRes.rdf") %>%
  spread(ObjectSlot, Value)

rr <- rdf_to_rwtbl2("inst/UBRes.rdf")

# testthat ---------------------------------
tt_deps <- pkgDep("testthat")
message("There are ", length(tt_deps), " dependencies in testthat")

# define custom expectation
expect_not_negative <- function(object, val) {
  act <- quasi_label(rlang::enquo(object))

  expect(
    all(act$val$Value >= 0),
    print(
      act$val %>%
        filter(Value < 0) %>%
        select(Timestep, TraceNumber, ObjectSlot, Value)
    )
  )

  invisible(act$val)
}

tt1 <- filter(rr, ObjectSlot == "BlueMesa.Outflow") %>%
  expect_not_negative(TRUE)

tt2 <- filter(rr, ObjectSlot == "MorrowPoint.Outflow") %>%
  expect_not_negative(TRUE)

# then have to capture errors and error output somehow and send to correct
# device....

# assertr ----------------------------------
aa_deps <- pkgDep("assertr")
message("There are ", length(aa_deps), " dependencies in assertr")

my_error <- function(errs, data = NULL, ...) {
  #browser()
  data[errs[[1]]$error_df$index,] %>%
    select(Timestep, TraceNumber, MorrowPoint.Outflow)
}

# aa does not exist
aa <- rdf %>%
  verify(BlueMesa.Outflow >= 0) %>%
  verify(MorrowPoint.Outflow >= 0)

# here it does; could use verify or assertr
aa <- rdf %>%
  chain_start() %>%
  assert(within_bounds(0, Inf), BlueMesa.Outflow) %>%
  assert(within_bounds(0, Inf), MorrowPoint.Outflow) %>%
  chain_end(error_fun = my_error)

aa

aa <- rdf %>%
  chain_start() %>%
  verify(BlueMesa.Outflow >= 0) %>%
  verify(MorrowPoint.Outflow >= 0) %>%
  chain_end(error_fun = my_error)

aa

# and then summarize as needed


# validate ---------------------------------
vv_deps <- pkgDep("validate")
message("There are ", length(vv_deps), " dependencies in validate")

vv <- check_that(
  rdf,
  BlueMesa.Outflow >= 0,
  MorrowPoint.Outflow >= 0,
  BlueMesa.outflow >= 0
)

summary(vv)
aggregate(vv, by = "record")
sort(vv)
errors(vv)

# get the timesteps and traces that failed
failed <- rdf[!values(vv)[,2], ] %>%
  select(Timestep, TraceNumber, MorrowPoint.Outflow)

# can then summarize however you want
# can also use yaml as input

ub_rules <- validator(.file = "inst/check_ub_outflow.yaml")
vv2 <- confront(rdf, ub_rules)
summary(vv2)

failed2 <- rdf[!values(vv2)[,2], ] %>%
  select(Timestep, TraceNumber, MorrowPoint.Outflow)

all.equal(failed, failed2)
