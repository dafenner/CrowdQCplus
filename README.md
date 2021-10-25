# CrowdQC+

This R package performs a quality control (QC) and filters suspicious data from citizen weather stations (CWS). It is based on the package <a href="http://dx.doi.org/10.14279/depositonce-6740.3">'CrowdQC'</a> but offers several additions, improvements, and bug fixes. Both packages were originally designed for air-temperature data but should also work with other near-normally distributed data.

A detailed description of the functionalities and an evaluation of the performance of the QC can be found in this open-access journal article: CrowdQC+ – A quality-control for crowdsourced air-temperature observations enabling world-wide urban climate applications.

## Dependencies
CrowdQC+ requires an R version >= 3.5.0 to work.

It also requires the following packages: 
- data.table
- robustbase
- lubridate
- sp
- raster
- rgdal

Make sure to have these installed (and 'data.table' needs to be loaded) before running CrowdQC+.

When installing 'rgdal' package on a Linux system, you might run into issues. Try running 

`sudo apt-get install gdal-bin proj-bin libgdal-dev libproj-dev` 

first and then again installing rgdal in R with 

`install.packages("rgdal")`. 

For 'older' Linux versions this could also work: 

`install.packages('rgdal', configure.args = c(rgdal = "--with-proj_api=proj_api.h"))`

## Installation of the package

**Option 1:**

Directly pull the code from this repository into your programming environment, using the <a href="https://devtools.r-lib.org/">devtools</a> package:

```
install.packages("devtools")
devtools::install_github("dafenner/CrowdQCplus")
```

**Option 2:**

Download the <a href="https://github.com/dafenner/CrowdQCplus/archive/refs/heads/master.zip">zip-file</a> from this repository, save it locally, and install it in your programming environment using the <a href="https://devtools.r-lib.org/">devtools</a> package:
```
install.packages("devtools")
devtools::install_local(<PATH_TO_THE_ZIP_FILE>)
```

Once installed, load CrowdQC+ (and data.table) via

```
library(data.table)
library(CrowdQCplus)
```

## Using CrowdQC+
### Data
Data should be represented as a <a href="https://CRAN.R-project.org/package=data.table">data.table</a> with the following required columns:

`p_id`: unique ID of each station<br>
`time`: time as POSIX.ct. Keep in mind time zones!<br>
`ta`: air-temperature values (or other near-normally distributed variable)<br>
`lon`: longitude of the station<br>
`lat`: latitude of the station<br>

Optionally, the user can provide elevation information per station (column `z`), as to perform a height correction in some of the QC levels.
Any other column can be present, but is quietly ignored by CrowdQC+.

### Functionalities
The QC consists of five main filter levels (m1-m5) and four optional levels (o1-o4).<br>
Beside the actual QC filters, several helper functions are available to, e.g., add elevation information to each station, check the data.table for compliance with CrowdQC+, and output simple statistics an data availability after application of the QC.

A detailed description of each QC level can be found in the R help and in the corresponding journal article: CrowdQC+ – A quality-control for crowdsourced air-temperature observations enabling world-wide urban climate applications.

There is a sample data set (`CWSBer`) in the package, which includes one month of CWS data for Berlin, Germany (June 2017), for testing.

## How to contribute?
If you are using CrowdQC+ and have ideas how to make it better, improve its performance, resolve errors, please create <a href="https://github.com/dafenner/CrowdQCplus/issues">issues</a>.

## Reference
Please reference the following open-access journal article when using CrowdQC+:

Fenner, D., Bechtel, B., Demuzere, M., Kittner, J. and Meier, F.: CrowdQC+ – A quality-control for crowdsourced air-temperature observations enabling world-wide urban climate applications. Frontiers in Environmental Science (in revision).

## Licence
CrowdQC+ is distributed under the <a href="http://www.gnu.org/licenses/gpl-3.0.en.html">GNU General Public License v3</a>.

