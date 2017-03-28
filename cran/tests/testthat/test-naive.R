# file corset/tests/testthat/test-naive.R
# Copyright (C) 2016 Merck Sharp & Dohme Corp. a subsidiary of Merck & Co.
# , Inc., Kenilworth, NJ, USA.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

context('Testing Naive method - Default functionality')

test_that("Corset - class integer", {
  set.seed(0)
  x <- sample(-10:10, 100, replace = T)
  cx <- corset(x, 'naive')
  x[x < 0] <- 0
  expect_equal(x, as.numeric(cx))
  expect_equal(class(cx), c('numeric', 'corset'))
})

test_that("Corset - class numerical", {
  set.seed(1)
  x <- rnorm(100)
  cx <- corset(x, 'naive')
  x[x < 0] <- 0
  expect_equal(x, as.numeric(cx))
  expect_equal(class(cx), c('numeric', 'corset'))
})

test_that("Corset - class ts", {
  set.seed(2)
  x <- ts(rnorm(100))
  cx <- corset(x, 'naive')
  x[x < 0] <- 0
  expect_equal(as.numeric(x), as.numeric(cx))
  expect_equal(class(cx), c('ts', 'corset'))
})

test_that("Corset - class forecast ", {
  skip_on_cran()
  if ('forecast' %in% installed.packages()) {
  set.seed(3)
  x <- forecast::forecast(ts(rnorm(100)))
  cx <- corset(x, 'naive')

  expect_equal(x$mean, cx$mean)
  expect_equal(x$upper, cx$upper)
  expect_equal(as.numeric(cx$lower[1, 1]), 0.00369813795252792)
  expect_equal(as.numeric(cx$lower[1, 2]), 0)
  }
})

test_that("Corset - class gts/hts ", {
  skip_on_cran()
  if ('hts' %in% installed.packages()) {
  set.seed(7)
  htseg <-
    hts::hts(ts(replicate(5, rnorm(10) + 15:6)), nodes = list(1, 5))
  x <-
    hts::forecast.gts(htseg,
                      h = 10,
                      method = "bu",
                      fmethod = "arima")
  cx <- corset(x, 'naive')
  expect_equal(cx$bts,
               structure(
                 c(
                   8.18997810732938,
                   8.18997810732938,
                   8.18997810732938,
                   8.18997810732938,
                   8.18997810732938,
                   8.18997810732938,
                   8.18997810732938,
                   8.18997810732938,
                   8.18997810732938,
                   8.18997810732938,
                   6.98816414949994,
                   6.98816414949994,
                   6.98816414949994,
                   6.98816414949994,
                   6.98816414949994,
                   6.98816414949994,
                   6.98816414949994,
                   6.98816414949994,
                   6.98816414949994,
                   6.98816414949994,
                   3.97404186898007,
                   3.20352606272425,
                   1.77463061190466,
                   0.788325859233616,
                   0,
                   0,
                   0,
                   0,
                   0,
                   0,
                   5.44675556352646,
                   4.57851622216501,
                   3.71027688080355,
                   2.8420375394421,
                   1.97379819808064,
                   1.10555885671919,
                   0.237319515357733,
                   0,
                   0,
                   0,
                   5.89912993563243,
                   4.86718787574477,
                   3.83524581585711,
                   2.80330375596944,
                   1.77136169608178,
                   0.739419636194118,
                   0,
                   0,
                   0,
                   0
                 ),
                 .Dim = c(10L, 5L),
                 .Dimnames = list(
                   NULL,
                   c("Series 1",
                     "Series 2", "Series 3", "Series 4", "Series 5")
                 ),
                 .Tsp = c(11,
                          20, 1),
                 class = c("mts", "ts", "matrix")
               ))
  }
})

test_that("Corset - class mts / ts / matrix ", {
  set.seed(4)
  x <- ts(replicate(10, rnorm(10)))
  cx <- corset(x, 'naive')
  x[x < 0] <- 0
  expect_equal(as.numeric(x), as.numeric(cx))
  expect_equal(class(cx), c("mts", "ts", "matrix", "corset"))
})


context('Testing Naive method - Arbitrary Boundaries')

test_that("Corset - class ts", {
  set.seed(5)
  x <- ts(rnorm(100, 0, 100))
  cx <- corset(x, 'naive', -1:-100, 1:100)
  #plot(x); lines(cx, col = 'red')
  expect_equal(cx, structure(
    c(
      -1,
      2,
      -3,
      4,
      5,
      -6,
      -7,
      -8,
      -9,
      10,
      11,
      -12,
      -13,-14,
      -15,
      -13.8986140549846,
      -17,
      -18,
      19,
      -20,
      21,
      22,
      23,
      24,
      25,
      -26,
      27,
      28,
      -29,
      -30,
      31,
      32,
      33,
      34,
      35,
      36,
      -37,
      -38,-39,
      -14.260812595506,
      41,
      -42,
      -7.45789198826713,
      44,
      -45,
      46,-47,
      -46.0244576195178,
      -49,
      -6.92111558341109,
      51,
      18.7726097433136,
      53,
      -54,
      -11.2200655036137,
      -56,
      57,
      -11.2609070203049,
      -6.40909282197963,
      23.3275293545762,
      -61,
      62,
      -57.8370418961854,
      49.6361539030152,-65,
      -34.1386270399429,
      -67,
      -30.1702281368895,
      -69,
      -27.9666109809229,-20.4097320819576,
      -22.5614185517355,
      34.7028452022099,
      3.23678425979233,
      41.3531289671798,
      -15.5348476625379,
      77,
      12.1090142773478,
      18.9173691477455,-56.2885069825959,
      49.8416165001331,
      -82,
      83,
      -2.40828727364371,
      67.5684475314084,
      -71.0309605053391,
      87,
      -47.3432012196463,
      -7.57725566667704,-52.1840056478283,
      91,
      -92,
      55.7033866297959,
      90.0730584912171,
      95,
      38.3608087579678,
      -34.6583813698718,
      -54.0189250004419,
      -18.2555593266753,-5.92996499937566
    ),
    .Tsp = c(1, 100, 1),
    class = c("ts", "corset")
  ))
})

test_that("Corset - class numerical", {
  set.seed(6)
  x <- rnorm(100)
  cx <- corset(x, 'naive', 0.5, Inf)
  x[x < 0.5] <- 0.5
  expect_equal(x, as.numeric(cx))
  expect_equal(class(cx), c('numeric', 'corset'))
})

