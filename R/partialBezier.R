# file corset/R/partialBezier.R
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

partialBezier <- function(y, proximity = 0)
{
  # Function to calculate a fitting solution using a modified version of
  # Bezier curves. The proximity parameter expresses how close the partial
  # bezier solution will be to the original series.
  # Args:
  #   y: numerical series
  #   proximity: silimilarity desired on the output, if larger than 100 wil
  #   the function will return the very same input.
  #
  # Returns:
  #   A numerical series with the same lenght of 'y' smoothed witha  semi
  #   bezier approach.

  pbezy <- NULL
  i <- 1
  b <- 0
  for (t in seq(0, 1, length.out = length(y)))
  {
    b <- 0
    n <- length(y) - 1
    for (j in 0:n) {
      binc <- choose(n, j) * ((1 - t) ^ (n - j)) * t ^ j * y[j + 1]
      if (!is.finite(binc)) {
        stop('Not enough computational precision; try reducing the number of points.')
      }
      b <- b + binc
    }
    pbezy[i] <- b
    i <- i + 1
  }

  if (proximity%%1!=0 | proximity < 0) {
    proximity <- round(abs(proximity))
    warning('proximity has been rounded. This parameter needs to be a positive or zero integer')
  }

  if (proximity > 100) {
    return(pbezy)
  } else {
    return((y * proximity + pbezy) / (1 + proximity))
  }
}