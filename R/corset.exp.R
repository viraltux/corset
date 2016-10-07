# file corset/R/corset.exp.R
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

corset.exp <- function(x, min,  max, ...){
  # Fast function to calculate a fitting solution using an exponential
  # decay/growth approach
  #
  # Args:
  #   x: numerical series or time series
  #   min: vector with lower boundaries
  #   max: vector with upper boundaries
  #
  # Returns:
  #   A numerical series with the same lenght of 'y' smoothed witha  semi
  #   bezier approach.

  if(length(min)!=length(x) | length(max)!=length(x)){
    stop('Boundaries do not have the right length')
  }

  lx <- length(x)
  wnna <- which(!is.na(x))
  x <- x[wnna]

  if (x[1] < min[1]) {x[1] <- min[1]}
  if (x[1] > max[1]) {x[1] <- max[1]}

  for(i in 2:(length(x)-1)){
    xm <- x[i-1]
    xi <- x[i]
    xp <- x[i+1]

    if (x[i] < min[i]){
      if (x[i+1] > min[i+1]){
        x[i] <- (x[i-1] + min(x[i+1],max[i+1]))/2
      } else {
        x[i] <- (x[i-1] + min[i])/2
      }
    }

    if (x[i] > max[i]){
      if (x[i+1] < max[i+1]){
        x[i] <- (x[i-1] + max(x[i+1], min[i+1]))/2
      } else {
        x[i] <- (x[i-1] + max[i])/2
      }
    }

  }

  if (xp < min[length(min)]) {x[i+1] <- min[length(min)]}
  if (xp > max[length(max)]) {x[i+1] <- max[length(max)]}

  x.out <- rep(NA,lx)
  x.out[wnna] <- x
  x.out
}