# LaTiP
LaTiP is small R project for time series analysis of Landsat data. There are not many codes in this project, but the framework might be useful. So I upload the code to share.
LaTiP provides utilities to model land surface changes on time-series Landsat data. Main computing modules are implemented in a parallel manner to reduce running time. The codes provided by LaTiP allows a user to perform all the steps of time series analysis workflow, from pre-processing raw surface reflectance Landsat data, inventorying and preparing them for analysis to the production and formatting of regression analysis results. 
## System requirements
### Computer
LandsatLinkr was developed and tested on computers running Windows 7 64-bit OS with >= 8 GB of RAM.
### Data storage
2 TB of data storage is recommended for processing a single WRS-2 scene project with ~250 images.
### Software
* R
* RStudio
* GDAL
