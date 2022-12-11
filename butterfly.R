#install rCharts
#install.packages("devtools")
library(devtools)
install_github('ramnathv/rCharts')

#load packages
#install.packages("reshape2")
library(reshape2)
library(rCharts)

#----------------------------------------------------------------
# PYRAMID CODE

#library(XML)
#library(reshape2)
#library(rCharts)
#library(plyr)

# Highcharts pyramid

hPyramid <- function(dat, year, colors = NULL) {
  ord <- 1:nrow(dat)
  dat <- cbind(dat, ord)
  dat$Male <- -1 * dat$Male
  
  dat$Age <- factor(dat$Age, levels = rev(dat$Age), labels = rev(dat$Age))
  
  keep <- c("Male", "Female", "Age")
  
  dat.sub <- dat[,keep]
  
  dat.melt <- melt(dat.sub, 
                   value.name='Population', 
                   variable.name = 'Gender', 
                   id.vars='Age' )
  
  h1 <- hPlot(
    y = 'Population', 
    x = 'Age', 
    type = 'bar', 
    data = dat.melt,
    group = 'Gender')
  
  h1$plotOptions(series = list(stacking = 'normal', pointPadding = 0, borderWidth = 0))
  
  h1$tooltip(formatter = "#! function() { return '<b>'+ this.series.name +', age '+ this.point.category +'</b><br/>' + 'Population: ' + Highcharts.numberFormat(Math.abs(this.point.y), 0);} !#")
  
  h1$legend(reversed = "true")
  
  if (max(dat.melt$Population >= 1000000)) {
    h1$yAxis(labels = list(formatter = "#! function() { return (Math.abs(this.value) / 1000000) + 'M';} !#"), 
             title = list(enabled = TRUE, text = 'Population'))
  } else {
    h1$yAxis(labels = list(formatter = "#! function() { return (Math.abs(this.value) / 1000) + 'K';} !#"), 
             title = list(enabled = TRUE, text = 'Population'))
  }
  
  if (!is.null(colors)) {
    h1$colors(colors)
  }
  if (length(year) > 1) {
    stop('Right now, hPyramid only accepts one year')
  }
  
  h1$exporting(enabled = TRUE)
  
  h1
}

# NVD3 pyramid

nPyramid <- function(dat, year, colors = NULL) {
  ord <- 1:nrow(dat)
  dat <- cbind(dat, ord)
  dat$Male <- -1 * dat$Male
  
  dat <- dat[order(rev(dat$ord)), ]
  
  keep <- c("Male", "Female", "Age")
  
  dat.sub <- dat[,keep]
  
  dat.melt <- melt(dat.sub, 
                   value.name='Population', 
                   variable.name = 'Gender', 
                   id.vars='Age' )
  
  dat.melt$abs <- abs(dat.melt$Population)
  
  n1 <- nPlot(
    y = 'Population', 
    x = 'Age', 
    group = 'Gender', 
    type = 'multiBarHorizontalChart', 
    data = dat.melt)
  
  # n1$xAxis(axisLabel = "Age") ## Need to work out label placement
  
  n1$chart(stacked = TRUE)
  
  n1$chart(tooltipContent = "#! function(key, x, y, e){
        var format = d3.format('0,000');
        return '<h3>' + key + ', age ' + x + '</h3>' + 
        '<p>' + 'Population: ' + format(e.point.abs) + '</p>'
        } !#")
  
  
  if (max(dat.melt$Population >= 1000000)) {    
    n1$yAxis(axisLabel = "Population",  
             tickFormat = "#! function(d) {
                          return d3.format(',.1f')(Math.abs(d) / 1000000) + 'M'
                          } !#")
  } else {
    n1$yAxis(axisLabel = "Population",  
             tickFormat = "#! function(d) {
                          return d3.format(',.0f')(Math.abs(d) / 1000) + 'K'
                          } !#")    
    
  }
  
  if (!is.null(colors)) {
    n1$chart(color = colors)
  }
  
  n1
}

#----------------------------------------------------------------

#2021 data
dat2021 = read.csv("2021data.txt",sep="\t")

#Pyramid code expects a Male, Female, and Age column

#highcharts JS
h = hPyramid(dat2021, year=2021, colors = c('pink', 'blue'))
h

#NVD3 JS
n = nPyramid(dat2021, year=2021, colors = c('pink', 'blue'))
n




