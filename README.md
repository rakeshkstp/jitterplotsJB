# Create Jitter plot for James Bay using R (ggplot2)

This project is to create jitter plot overlaid by violin plots for James Bay using the extracted data for all the stations from MODIS-Aqua images between 2003 and 2021.
This work is a part of [COast-JB Project - Coastal oceanography of eastern James Bay](https://www.creegeoportal.ca/climate-change/).

## How to use the R script?
* Put the extracted data (Extracted_*.RData) in the same folder as the Rscript [Jitterplots.R](https://github.com/rakeshkstp/jitterplotsJB/blob/main/Jitterplots.R).
* Install the required libraries.
  * [ggplot2](https://cran.r-project.org/web/packages/ggplot2/) 
  * [lubridate](https://cran.r-project.org/web/packages/lubridate/)
  * [pals](https://cran.r-project.org/web/packages/pals/)
  * [reshape2](https://cran.r-project.org/web/packages/reshape2/)
  * [dplyr](https://cran.r-project.org/web/packages/dplyr/)
  * [grid](https://cran.r-project.org/web/packages/grid/)
* Run the script on terminal or RStudio to generate [png image](https://github.com/rakeshkstp/jitterplotsJB/blob/main/JB_Jitterplot_Stations.png).
```
Rscript Jitterplots.R
```
## Author
**Rakesh Kumar Singh**
