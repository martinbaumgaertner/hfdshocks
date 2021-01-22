# hfdshocks
This Package estimates the monetary policy surprises from high-frequency data in R. It is based on

**Altavilla, C., Brugnolini, L., Gürkaynak, R. S., Motto, R., & Ragusa, G. (2019). Measuring euro area monetary policy. Journal of Monetary Economics, 108, 162-179.** [Link](https://www.sciencedirect.com/science/article/pii/S0304393219301497)

The corresponding Julia/R/Stata code and sample data can be found here: <http://www.bilkent.edu.tr/~refet/ABGMR_replication_files.zip> (by Refet S. Gürkaynak)

# Installation:

> library(“devtools”) 
>
> devtools::install_github("https://github.com/martinbaumgaertner/hfdshocks.git") 
>
> library(hfdshocks)

# Content

The package downloads the EA-MPD data and calculates the monetary policy shocks.

Please note that the current data of the EA-MPD have been slightly revised compared to the original data of the paper. Accordingly, the calculated surprises are (slightly) different from the original surprises. To reproduce the original data uses the original database from the replication files. So far, I have not been able to find any significant difference in VAR models.

There are various setting options:

​	**Exclude outlier**

​	If you want to exclude data as outliers, enter them here. No specific date format is required. The standard excludes 3 data points based on the original 
​    paper.  : 2001-09-17, 2008-10-08, 2008-11-06

​	**Specify data window**

​	Changes the window of the input data. This influences the final result! Until now, I recommend using all data up to the current point in time and then
​    shorten the time series afterwards. Nevertheless, the default in the package is the time window used in the original paper

​	**Specify Crisis date**

​	The third factor (QE) is calculated by minimising the variance before the financial crisis. The corresponding value can be changed here.

*Disclaimer: Although the methodology is not  mine, all errors are first to be credited to me personally. Please note  that this package has not yet been tested extensively. I am grateful for  every hint for improvement.*

# Example

```r
#load package
library(devtools)
devtools::install_github("https://github.com/martinbaumgaertner/hfdshocks.git") 

library(hfdshocks)
ecb_shocks("https://www.ecb.europa.eu/pub/pdf/annex/Dataset_EA-MPD.xlsx","",
               exclude_date=c("2001-09-17","2008-10-08","2008-11-06"),
               range=c("2001-12-31","2018-09-13"),crisis_date="2008-09-04")
```
