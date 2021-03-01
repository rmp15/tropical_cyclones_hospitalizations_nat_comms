# code for model used in Parks et al. Nature Communications 2021

# some packages used
library(gnm) ; library(splines) ; library(dlnm)

# model formula
mod = gnm(cases ~
            # tropical cyclone exposure slopes by lag day after tropical cyclone exposure
            event_lag0 + event_lag1 + event_lag2 + event_lag3 + event_lag4 + event_lag5 + event_lag6 + event_lag7 +
            # mean daily tempearture
            cb1.temp +
            # natural spline of year trends with
            ns(year, df=2) +
            # day of the week indicator to adjust by
            dow ,
            # input data
            data=dat_input,
            # log of population offset
            offset=logpop,
            # stratum matching on county and Julian day of year
            eliminate=factor(stratum),
            # quasi-Poisson model
            family=quasipoisson)
