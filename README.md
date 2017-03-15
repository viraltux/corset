# corset
Arbitrary bounding of series and time series objects

## Description

When working with times series we often have boundary constraints that cannot be easily introduced in mathematical models like ARIMA or ETS, or simply cannot be introduced in already existing R packages returning series and time series objects.

The *corset* package intends to be a companion to series and time series analysis to easily enhance and tune already existing results in a seamless way. A typical use case would be to force declining financial time series forecasts to converge to zero instead having negative values.

The *corset* function allows to introduce boundary constraints in series, time series as well as in *forecast* & *gts/hts* objects.

In particular, when applied on a *forecast* object it does not only apply the boundaries to the forecast but also to the confidence intervals.

![](/images/corseted.png)
