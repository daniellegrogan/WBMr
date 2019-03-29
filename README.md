# WBMr

### This repository conatains the following R files:

### WBM-specific:

**wbm_load.R**

Functions: wbm_load()
- Uses the raster package to load WBM output files into R
- Reads multiple yearly WBM files into a single raster stack
- Only loads one variable 

**spatial_aggregation.R** 

Functions:
  1. spatial_aggregation()
  - Given gridded data and a polygon shapefile, aggregates grid values to polygons.  
  - Option: sum or average over polygons.
     - If sum is used, the function assumes the data is in mm and aggregates to km3 using either input grid area, or calculates grid area.
  2. global.sum()
  - Given gridded data, sums or averages all grids. 
    - If sum, then input is assumed to be mm and output is in km3. Uses either input grid area, or calculates grid area.
  3. global.sum.stack()  *NOTE: this function should be updated/merged with global.sum()*
  4. spatial.agg.ts()
  

### General hydrology:

**NSE.R**

Functions: NSE()

### Links to other data or models:

**get_USGS_data.R**

Functions: get_USGS_data()
