U.S. Tornado Spatial Analysis (1980–2019) --
This repository contains the processing scripts for a CONUS-scale spatial analysis of F/EF1+ tornado activity across four decades (1980s–2010s), completed as a capstone project for GEOG 570 at Pennsylvania State University. The R script will create a local version of the Interactive Leaflet Map listed below. 

Project Overview --
This study examines how the spatial pattern of tornado activity changed across the contiguous United States from 1980 to 2019, with a focus on whether redistribution was uniform across seasons or concentrated in specific parts of the tornado calendar. Methods include kernel density estimation, Empirical Bayes-smoothed county-level rate estimation, Getis-Ord Gi* hotspot analysis, and temporal hotspot trajectory classification.

Interactive Map --
The companion interactive web map is available at:
https://tornado-map-capstone.s3.us-east-2.amazonaws.com/conus_county_tornado_map.html

Scripts --
conus_tornado_map_v5.R — Main processing script for county aggregation, EB smoothing, Gi* analysis, temporal classification, and Leaflet map generation
tornado_map_configurator.py — Python GUI tool for configuring and generating a customized version of the R script without manually editing code

Data Sources --
NOAA Storm Prediction Center Severe Weather GIS Archive
U.S. Census Bureau TIGER/Line county shapefiles
IPUMS NHGIS county-level population data
