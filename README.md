```r
require(devtools)
devtools::load_all()
station <- 51423
begin <- 2013
end <- 2018
#download.ClimateData(station, begin, end, "hourly", "curl")
#compile.ClimateData(station, begin, end, T)
```

```r
plot.Climate(station, years=c(begin:end))
```

```r
plot.Climate(station, years=c(begin:end), mode="Date")
```
