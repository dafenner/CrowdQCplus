# CrowdQC+

This R package performs a quality control (QC) and filters suspicious data from citizen weather stations (CWS). It is based on the package 'CrowdQC' (http://dx.doi.org/10.14279/depositonce-6740.3) but offers several additions and improvements. Both packages are originally designed for air-temperature data but should also work with other near-normally distributed data.

## Data and functionality
Data should be represented as a data.table (https://CRAN.R-project.org/package=data.table) with the following required columns:

p_id: unique ID of each station<br>
time: time as POSIX.ct. Keep in mind time zones!<br>
ta: air-temperature values (or other near-normally distributed variable)<br>
lon: longitude of the station<br>
lat: latitude of the station<br>

Optionally, the user can provide elevation information per station (column 'z'), as to perform a height correction in some of the filter levels.
Any other column can be present, but is quietly ignored by CrowdQC+.

The QC consists of five main filter levels (m1-m5) and four optional levels (o1-o4).<br>
Beside the actual QC filters, several helper functions are available to, e.g., add elevation information to each station, check the data.table for compliance with CrowdQC+, and output simple statistics an data availability after application of the QC.

## Dependencies
CrowdQC+ requires an R version >= 3.5.0 to work.

It also requires the following packages:

data.table, robustbase, lubridate, sp, raster, rgdal

Make sure to have these installed before running CrowdQC+.

## Reference
Please reference the following paper when using CrowdQC+:

Fenner, D., Bechtel, B., Demuzere, M., Kittner, J. and Meier, F.: CrowdQC+ – A quality-control for crowdsourced air-temperature observations to enable world-wide application in urban climate studies.

