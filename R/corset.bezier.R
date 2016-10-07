# file corset/R/corset.bezier.R
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

corset.bezier <- function(x, min, max,  proximity){
  # Function to calculate a fitting solution using a modified version of
  # Bezier curves. The proximity parameter expresses how close the partial
  # bezier solution will be to the original series.
  # Args:
  #   x: numerical series or time series
  #   min: vector with lower boundaries
  #   max: vector with upper boundaries
  #   proximity: silimilarity desired on the output, if larger than 100 wil
  #   the function will return the very same input.
  #
  # Returns:
  #   A numerical series with the same lenght of 'y' smoothed witha  semi
  #   bezier approach.

  if(length(min)!=length(x) | length(max)!=length(x)){
    stop('Boundaries do not have the right length')
  }


  ## Partial Bezier Method
  cx <- corset.naive(x, min, max)

  lx <- length(cx)
  wnna <- which(!is.na(cx))
  cx <- cx[wnna]

  pcx <- partialBezier(cx, proximity)

  # Finding cutting points between bezier and naive
  cp <- which(diff(sign(cx - pcx))!=0)
  cp <- cp + 1
  cp <- sort(unique(c(cp,1,length(cx))))

  # Marking which segments have a threshold violation
  v <- (x < min | x > max)

  #Applying bezier within the segments containing violation points
  bseg <- NULL
  for(i in 1:(length(cp)-1)){
    bseg <- c(bseg, rep(ifelse(sum(v[cp[i]:(cp[i+1]-1)]) > 0,TRUE,FALSE),
                        cp[i+1]-cp[i]))
  }
  bseg <- c(bseg, v[cp[length(cp)]])
  x[bseg] <- pcx[bseg]

  x.out <- rep(NA,lx)
  x.out[wnna] <- x
  x.out
}
