library(testthat)
library(smoof)
library(xegaEnvMeta)

#
# Tests
#

test_that("smoofWrapperFactory()",
{
smoofFun<-makeSchwefelFunction(2)
xegaFun<-smoofWrapperFactory(smoofFun)
expect_identical(xegaFun$name(), smoof::getName(smoofFun))
}
) 

