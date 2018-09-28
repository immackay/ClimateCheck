###########################
# Beta plotting functions #
###########################

require(plotly)

plot.Climate <- function(station, years=NA, begin=NA, end=NA, mode="MD") {
  if (!(is.na(begin) & is.na(end))) {
    years <- begin:end
  }
  if (anyNA(years)) {
    return("You must provide years to plot!")
  }
  p <- NA
  for (year in years) {
    temp <- read.ClimateCSV(station, year)
    if (anyNA(p)) {
      if (mode=="MD") p <- plot_ly(data=temp, x=~MD, y=~TempC, type="scatter", mode="lines", name=year, opacity=1/sqrt(length(years)))
      if (mode=="Date") p <- plot_ly(data=temp, x=~Date, y=~TempC, type="scatter", mode="lines", name=year)
    } else {
      if (mode=="MD") p <- add_trace(p, data=temp, x=~MD, y=~TempC, mode="lines", name=year, opacity=1/length(years))
      if (mode=="Date") p <- add_trace(p, data=temp, x=~Date, y=~TempC, mode="lines", name=year)
    }
  }
  if (max(years)-min(years)==length(years)-1)
    yearNames <- paste(min(years), max(years), sep="-")
  else
    yearNames <- gsub("(20|19){1}([[:digit:]]{2},?)", "'\\2", paste0(years, collapse=", "))
  testTitle <- paste(
    "Plot of station ", station, " (", 
    as.character(read.ClimateHeader(station)$Station.Name), 
    ") years ", yearNames, sep="")
  p <- layout(p, title=testTitle)
  p
}
plot.ClimateAnimation <- function() {
  
}
plot.ClimateMap <- function() {
  
}