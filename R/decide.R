library(RWDataPlyr)
library(validate)
library(tidyverse)
library(miniCRAN)

# test how the three libraries would report the same error/success differently
# check that morrow point and blue mesa outflow are >= 0
# morrow point should fail, blue mesa should succeed

rdf <- rdf_to_rwtbl2("inst/UBRes.rdf") %>%
  spread(ObjectSlot, Value)

# validate ---------------------------------
vv_deps <- pkgDep("validate")
message("There are ", length(vv_deps), " dependencies in validate")

# rules written in R
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

# can also use yaml as input rules
ub_rules <- validator(.file = "inst/check_ub_outflow.yaml")
vv2 <- confront(rdf, ub_rules)
summary(vv2)

failed2 <- rdf[!values(vv2)[,2], ] %>%
  select(Timestep, TraceNumber, MorrowPoint.Outflow)

all.equal(failed, failed2)
