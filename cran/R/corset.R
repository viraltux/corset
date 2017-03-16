# file corset/R/corset.R
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

corset <- function(x, method = c('bezier', 'exp', 'naive'),
                      min = 0, max = Inf, proximity = 0,
                      centrality = FALSE){

  # Top function to handle the different methods to corset time series
  # decay/growth approach
  #
  # Args:
  #   x: numerical series or time series
  #   min: vector with lower boundaries,
  #   if a value it will converted into a vector
  #   max: vector with upper boundaries,
  #   if a value it will converted into a vector
  #   proximity: proximity for the partial bezier and
  #   exponential method, it has no effect on the naive method.
  #   centrality: when TRUE it forces the mean forecast to be placed
  #   in (upper[,1] + lower[,1]) / 2
  #
  # Returns:
  #   A the same object of class x corset with the chosen methodology


  ## Class check
  if (!any(is.vector(x),
           is.ts(x), 'forecast' %in% class(x),
           stats::is.mts(x), base::is.matrix(x),
           sum(class(x) %in% c('gts','hts')) > 0 )){
    stop('x class is not handled by corset.')
  }

  ## Method check
  method <- match.arg(method,  c('bezier', 'exp', 'naive'))

  ##  Boundary vectors
  if(any(max - min < 0)){
    stop('Boundary vectors intersect')
  }

  if ('forecast' %in% class(x)){
    min <- rep_len(min, length(x$mean))
    max <- rep_len(max, length(x$mean))
  }

  if ( sum(class(x) %in% c('gts','hts')) > 0 ){
    min <- rep_len(min, nrow(x$bts))
    max <- rep_len(max, nrow(x$bts))
  }

  if (stats::is.mts(x) | base::is.matrix(x)){
    min <- rep_len(min, nrow(x))
    max <- rep_len(max, nrow(x))
  }

  if (is.vector(x) | (is.ts(x) & !is.mts(x))){
    min <- rep_len(min, length(x))
    max <- rep_len(max, length(x))
  }


  ## TODO generic NA processing

  ## Internal functions
  corset.forecast <- function(corset.function){
    r <- (x$upper[,2] - x$mean)/(x$upper[,1] - x$mean)
    r[is.nan(r)] <- 1

    to.ts <- function(x,tspx){
      x <- ts(x)
      tsp(x) <- tspx
      return(x)
    }

    ## Applies a corset methodology to a forecast class object
    tspx <- tsp(x$mean)
    x$mean <- to.ts(corset.function(x$mean, min, max, proximity),tspx)

    # Keeping proportions in first CI level
    x$upper[,2] <-  to.ts(corset.function(x$upper[,2], min, max, proximity),tspx)
    x$lower[,2] <-  to.ts(corset.function(x$lower[,2], min, max, proximity),tspx)

    x$upper[,1] <- (x$upper[,2] - x$mean) / r + x$mean
    x$lower[,1] <- (x$lower[,2] - x$mean) / r + x$mean
    if (centrality) {
      x$mean <- to.ts((x$upper[,1] + x$lower[,1] ) / 2,tspx)
    }

    return(x)
  }

  corset.hts <- function(corset.function){
    #Applies corset to mts / matrix columns
    tspx <- tsp(x$bts)
    x$bts <- apply(x$bts, 2,
                   function(x) corset.function(x, min, max, proximity))
    if (!is.null(tspx)){
      x$bts <- ts(x$bts)
      tsp(x$bts) <- tspx
    }
    return(x)
  }

  corset.mtsx <- function(corset.function){
    #Applies corset to mts / matrix columns
    tspx <- tsp(x)
    x <- apply(x, 2,
          function(x) corset.function(x, min, max, proximity))
    if (!is.null(tspx)){
      x <- ts(x)
      tsp(x) <- tspx
    }
    return(x)
  }

  corset.ts <- function(corset.function) {
    #Applies corset to ts
    tspx <- tsp(x)
    x <- corset.function(x, min, max, proximity)
    x <- ts(x)
    tsp(x) <- tspx
    return(x)
  }

  apply.corset <- function(corset.function) {
    if ('forecast' %in% class(x)) {
      cst <- corset.forecast(corset.function)
      class(cst) <- c(class(cst), 'corset')
      return(cst)
    }

    if (stats::is.mts(x) | base::is.matrix(x)) {
      cst <- corset.mtsx(corset.function)
      class(cst) <- c(class(cst), 'corset')
      return(cst)
    }

    if (sum(class(x) %in% c('gts','hts')) > 0){
      cst <- corset.hts(corset.function)
      class(cst) <- c(class(cst), 'corset')
      return(cst)
    }

    if(stats::is.ts(x)){
      cst <- corset.ts(corset.function)
      class(cst) <- c(class(cst), 'corset')
      return(cst)
    }

    cst <- corset.function(x, min, max, proximity)
    class(cst) <- c(class(cst), 'corset')
    return(cst)

  }

  switch (method,
          exp = apply.corset(corset.exp),
          naive = apply.corset(corset.naive),
          bezier = apply.corset(corset.bezier)
  )

}

