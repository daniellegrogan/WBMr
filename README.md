# WBMr

## This repository conatains the following functions:

### WBM-specific:

wbm_load.R
- Uses the raster package to load WBM output files into R
- Reads multiple yearly WBM files into a single raster stack
- Only loads one variable 

spatial_aggregation.R
- Contains four functions:
  1. spatial_aggregation()
  2. global.sum()
  3. global.sum.stack()  *NOTE: this function should be updated/merged with global.sum()*
  4. spatial.agg.ts()
  

### General hydrology:

NSE.R

### Links to other data or models:

get_USGS_data.R

