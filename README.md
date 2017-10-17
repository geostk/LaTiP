# LaTiP #

LaTiP is small R project for time series analysis of Landsat data. There are not many codes in this project, but the framework might be useful. So I upload the code to share.

LaTiP provides utilities to model land surface changes on time-series Landsat data. Main computing modules are implemented in a parallel manner to reduce running time. The codes provided by LaTiP allows a user to perform all the steps of time series analysis workflow, from pre-processing raw surface reflectance Landsat data, inventorying and preparing them for analysis to the production and formatting of regression analysis results.

## Environmental Requirements ##

LaTiP was developed and tested on both windows(7/10) and ubuntu(14.04/16.04).
For users who have a lot of data to process and a powerful computer with plenty of CPU cores and RAM sizes, they can choose ubuntu to run these codes. Because LaTiP provide multicore support on ubuntu.
For users who don't have so much data to process, and only have a normal computer with limited computing ability, windows is recommended.

### Software ###

- R
- RStudio
- GDAL

LaTiP requires the rgdal package, which is a binding between the gdal library and R. For rgdal to work, gdal should be installed and properly set-up. For that, please refer to the web page of the <a href="http://www.gdal.org/">gdal project</a> as well as <a href="https://cran.r-project.org/web/packages/rgdal/index.html">rgdal</a>.

## List of functions ##
- `main()`: An example of how to use LaTiP to process data and carry out time series analysis.
- `process()`: The base function that brings a single landsat scene from its archive to a spectral index raster layer, cropped to a certain extent and with clouds filtered out.
- `process_batch()`: The batch processing implementation of process() with parallel computing support.
- `time_stack()`: A function to create raster stacks of spectral index layers, with time written in the object properties.
- `sr_to_vi()`: Calculate Vegetation Index from Landsat surface reflectance data files. Extract layers, apply mask, crop, calculate spectral indices and write output to files.
- `calculator_vis()`: Including basic spectral indexs calculator. This module is extendable, if users want to calculate more indexs.
- `multicore_operate()`: Allows functions to be applied to raster objects with multicore support. Including some basic calculators of regression analysis. This module is extendable, users can add calculators according to their researches.
- `get_scene_info()`: Retrieve Landsat info from filenames. Parses through typical Landsat filenames and retrieves information on sensor and acquisition date.

## Processing Details ##

- Extract data from the tar.gz archive
- Calculate spectral indices from surface reflectance bands (when not provided by USGS)
- Crop the data to a desired spatial extent
- Apply one of the cloud/land mask supplied with the data
- Create a spatio-temporal object to be used in subsequent analyses
- Carry out regression analysis on pixel level in time dimension
- Calculate mean pixel values of stacked time series data
- Some commonly used time series models are provided

## Schematic Diagram ##

### 1. Modelling time series values on a specific pixel ###
<p><a href="https://github.com/jingge326/MaterialFolder/blob/master/plot_curves.png" target="_blank"><img src="https://github.com/jingge326/MaterialFolder/raw/master/plot_curves.png" width="600" height="600" alt="" align="center" style="max-width:100%;"></a></p>

### 2. Two typical categories of land surface change shown by six bands of surface reflectance: (a), (b) deciduous forest, and (c), (d) developed land. ###
<p><a href="https://github.com/jingge326/MaterialFolder/blob/master/samples.png" target="_blank"><img src="https://github.com/jingge326/MaterialFolder/raw/master/samples.png" width="700" height="485" alt="" align="center" style="max-width:100%;"></a></p>

The model used is
<p><a href="https://github.com/jingge326/MaterialFolder/blob/master/model.png" target="_blank"><img src="https://github.com/jingge326/MaterialFolder/raw/master/model.png" width="300" height="36" alt="" align="center" style="max-width:100%;"></a></p>
</article>
  