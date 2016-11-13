#------------------------------------------------------------------------------------
# timeseries_webmaps.R
#------------------------------------------------------------------------------------
# This script creates time series animated and interactive webmaps
# from data in an excel spreadsheet using a customized version of the rMaps package.
#
# pattyf@berkeley.edu, June 2016
#
# Instructions
# 1) Run this script in a folder that contains this script, the Datamaps_customized.R script
# and the input data in an excel or csv file
# 2) Update the values in the Set Important Parameters section below.
# 3) You need to be online to run
# 
# INPUT Data Format
# This script expects the input data to be in one of two formats: wide or long 
#
# wide format are data
# with one column with the state abbreviation (eg CA for California) and/or state name
# and one or more columns named by the year for which the data in the rows are relevant
# any other columns are ignored
# eg:
# State,1916,1917,1818
# AK,1,3,7
# AL,2,3,6
#
# IMPORTANT: This script assumes that the wide format files have year columns in order.
#
# Long format data
# have a column for state name and/or abbreviation
# a column for year
# and one or more column of data values - of which only one is processed per run of this script
# eg:
# State,year,medincome,top10percent
# AK,1917,2000,4
# AL,1917,1500,5
# 

#------------------------------------------------------------------------------------



#--------------------------------------------------------------------------
# Set Important Parameters
# THESE SHOULD BE THE ONLY VALUES YOU NEED TO CHANGE to run this script
#--------------------------------------------------------------------------

# Identify the folder that contains the input and output files
my_data_directory <- "~/Documents/Dlab/consults/robin_e/rmapsByYear"

#
# INPUT DATA
#

# Available Sample data
# Source: http://www.shsu.edu/eco_mwf/inequality.html
#         http://www.shsu.edu/eco_mwf/Frank_WTID_2013.xls
# To use, download one of the following files and then and set the paramaters below
sample_data_long <- "https://raw.githubusercontent.com/dlab-geo/rmapsByYear/master/data/Frank_WTID_2013_top1_long.csv"
sample_data_wide <- "https://raw.githubusercontent.com/dlab-geo/rmapsByYear/master/data/Frank_WTID_2013_top1_wide.csv"

# Input data file type (csv or excel)
infile_type = "csv" # excel"

# The name of the input file
in_data_file <- sample_data_long
#in_data_file <- "Frank_WTID_2013_shares_deciles.xlsx" 
#in_data_file <- "top10returnsper10kpersons1916.xlsx"
#in_data_file <- "SOI data all Returns Map Input2.xlsx"
in_data_worksheet <- 1  # The excel worksheet with the data, default is the first sheet
                        # ignored if file type is csv

# Are the data in wide format and thus need to be transformed to long format?
indata_wide_format <- FALSE  #set to FALSE if data already long TRUE if wide
# If data are in long format you need to identify the names of the columns
# that contains the years and the data values
# Else leave these as none
indata_data_colname <- "Top1_adj" # or NULL
indata_year_colname <- "Year" #"year" # or NULL

# The name of the column in the input excel worksheet that contains:
state_colname <- "State"  # name of column with state name or abbreviation
state_type <- "name" # set to name if not abbreviation (default)

# Years to be mapped - currently ignored if data in long format already
start_year <- "1917"      # The first year of data
end_year <- "2013"        # The last year of data - can be same


# The name of the output file that will contain the map
out_map_file <- "map_top1.html"

# The title along the top of the map - THE YEAR WILL BE APPENDED
map_title <- "Top 1% Earners Share of Total Income, "

# When you click over the state you get a popup box
# that displays the state name, year, and data value.
# The data_label_in_popup is the name for the data value that will display in popup
data_label_in_popup <- "Percent Share: "

# The number of decimal places to display for the mapped data
#num_decimal_places = 0
num_decimal_places = 2

# Do you want your data binned by quintile (5 quantile bins) by EACH YEAR and then 
# have the quantile bin determine the color each mapped region (eg State)?
create_qbins_by_year <- FALSE

# The breakpoints for map colors
# SET TO NULL to let have the breaks determined automatically
# as quintiles over the entire range of the data
map_breaks <- NULL
map_ncuts <- 5 # This is used when map_breaks is NULL
# else it is calculated from the manual map_breaks you define below
# ALTERNATIVELY - Manually set map_breaks
# If you do not include the largest value in your data in the map_breaks range - it will automatically be calculated
#map_breaks <- c(1,50,75,90,110,125,150) # This could be used for percent above or below average
#map_breaks <- c(0,1,2,3,4) # This is good for quintiles
map_breaks <- c(0,5,10,15,20,25)

# Note: the first number in the map breaks is inclusive
# Set it to FALSE if you don't want this behavior
include_lowest_in_bin1 <- FALSE # If the lowest map_break is 1
# and this is set to FALSE then values <= 1 won't be in first bin
# Instead they will be mapped as no data values (zero is sometimes a no data value and sometimes a real data value)

# Color palettes
# To see the names of the different palettes you can try see: 
# https://github.com/dlab-geo/rmapsByYear/raw/master/data/r_color_palettes.pdf
#
useSequentialColors <- TRUE # set to FALSE to use divergent

# Divergent Color Palette - used to highlight deviations from mean
# UNCOMMENT TO USE THESE (remove preceding hash (#)) AND comment out any other color palette values
map_color_palette_divergent <-  #PuOr"   # The name of the color palette that will be used.
map_nodata_color_divergent <- "#794044" # dark red-brown, the color to use for no data values (values not included in the breaks, eg zero)
map_border_color_divergent <- "#989fa5" # Grey, outline color used with this divergent color palette
#
# Sequential Color Palette
map_color_palette_sequential <- "YlOrBr" #"Blues"  # The name of the color palette that will be used.
#map_nodata_color_sequential <- "#FFFFFF" # white, The color to use for no data values (values not included in the breaks, eg zero)
map_nodata_color_sequential <- "#989fa5" # Grey
#map_border_color_sequential <- "#FFFFFF" # White, outline color to use with sequential color palette
map_border_color_sequential <- "#989fa5" # Grey, outline color 

show_map_legend <- TRUE     # set to TRUE or FALSE - no quotes
show_map_labels <- TRUE    # set to TRUE or FALSE - no quotes
 
#--------------------------------------------------------------------------
# END OF THINGS THAT NEED TO CHANGE!!
#--------------------------------------------------------------------------

#----------------------------- 
# INSTALL REQUIRED R PACKAGES
#-----------------------------

# Install helper packages
required.pkg <- c('tidyr','plyr','dplyr','RColorBrewer','readxl','devtools','downloader')
pkgs.not.installed <- required.pkg[!sapply(required.pkg, function(p) require(p, character.only=T))]
if (length(pkgs.not.installed > 0)) {
  install.packages(pkgs.not.installed, dependencies=TRUE)
} else {
  print("Helper packages installed.")
}

# Install rMaps and rCharts packages for creating the map
if ("rMaps" %in% rownames(installed.packages()) == FALSE ) {
  require(devtools)
  install_github('ramnathv/rCharts@dev')
  install_github('ramnathv/rMaps')
} else {
  print("Rmaps installed.")
}

 
#--------------------
# Load Libraries
#--------------------
#library(reshape2)
#library(plyr)
library(tidyr)
library(plyr)
library(dplyr)
library(readxl)
library(RColorBrewer)
library(rCharts)
library(rMaps)


# Set working directory/folder 
# in which input and output files
# will be located
setwd(my_data_directory)

# Read in the code for the local, customized version of the rMap
# This version has my ichoropleth2 function
source('./R/Datamaps_customized.R')

# Read in the Excel file
# Note: you can also read in a CSV file with the command:
# read.csv(in_data_file, StringsAsFactors=FALSE)
if (infile_type == "excel") {
  indata <- read_excel(in_data_file)
} else {
  indata <- read.csv(in_data_file, stringsAsFactors = FALSE)
}
# We will take the data for each column and populate the following new columns foreach state & year combo:
year_colname <- 'year'    # the column with the year (and only the year) in YYYY format (e.g., 2016)
data_colname <- 'vals'    # the column that will contain the data values to be used to determine the map colors (data_colname)
popup_colname <- 'popup_data'   # the column with the data values to display in the map popup (can be the same as the data_colname)

# Make sure that the name of the column with the state abbreviation is State
# We use the state abbrevation to link the data to the map
if (state_colname != 'State') {
  names(indata)[names(indata)== state_colname] <- "State"
}

if (state_type == "name") {
  indata$state_name <- indata$State # save original input
  indata$State <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", indata$State, perl=TRUE) # standardize to init caps
  state_abbrev <- read.csv("data/state_abbrev.csv", stringsAsFactors = FALSE)
  indata <- merge(x = indata, y = state_abbrev[ , c("name", "code")], by.x="State", by.y = "name", all.x=TRUE, incomparables=NA)
  indata$State <- indata$code
}

# Remove any rows that do not contain state abbreviation
indata <- indata[!is.na(indata$State),]

if (indata_wide_format == TRUE) {
  print("creating long data frame")
  # Rearrange the data so that all the year become a value in a column named "year"
  # and all the data values for each year go into a column named "vals"
  # This makes the data table real long instead of wide
  # For this the columns are referenced by index number not name
  # IMPORTANT: This assumes that the excel file contains the years in order.
 
  if (start_year != end_year) {
    # there is more than one years worth of data
    firstcol <- which(colnames(indata)==start_year)  # The column with the first year
    lastcol <- which(colnames(indata)==end_year)     # The column with the last year
   
     byYear <- gather(indata,year,vals,firstcol:lastcol)  # Make the "wide" data "long" & put in a new data frame
 
  } else {
    # data already in long format by virtue of only one year
    # so relabel the year column with the name vals
    names(indata)[names(indata) == start_year] <- "vals"
    indata$year <- start_year
    
    byYear <- indata # Copy indata to byYear to indicate that the data are now organized by year
  }
  
} else {
  print("data in long format already")
  
  # relabel the data with the year
  if (!is.null(indata_year_colname)){
    names(indata)[names(indata) == indata_year_colname] <- year_colname
  } else {
    stop('The argument "indata_year_colname" cannot be NULL')
  }
  # relabel the data with the values to be mapped
  if (!is.null(indata_data_colname)) {
    names(indata)[names(indata) == indata_data_colname] <- data_colname
  } else {
    stop('The argument "indata_data_colname" cannot be NULL')
  }
  #save input data to new data frame
  byYear <- indata
  start_year <- min(byYear$year)      # The first year of data
  end_year <- max(byYear$year)       # The last year of data
  print(paste("Processing data for years ", start_year, "to", end_year))
} 

# Order the data by state abbreviation (State)
byYear <- arrange(byYear,State)

# Make sure the data values are decimals (not character strings)
# and remove any commas
byYear$vals <- as.numeric(gsub(',','',byYear$vals))

# Set the number of decimal places to num_decimal_places
byYear$vals <- round(byYear$vals, digits=num_decimal_places)

# Set the year values to integers (not character strings or factors)
byYear$year <- as.integer(as.character(byYear$year))

# Set the data to display in the map popup to be the same as the data values
byYear$popup_data <- byYear$vals
byYear$popup_data[is.na(thedata$popup_data)] <- "no data" #set no data popup value

if (create_qbins_by_year == TRUE) {
  # Create 5 quantile bins
  # To consistently classify the states across years
  # unique(quantile(fml$left, seq(0, 1, 1/ncuts))),
  print("Creating quantile bins")
  binned_data <- data.frame()
  for (this_year in start_year:end_year) {
    print(paste("Processing data for year:", this_year))
    tempdata <- subset(byYear, year == this_year)
    tempdata$vals <- ntile(tempdata[[data_colname]], 5)
    binned_data <- rbind(binned_data, tempdata)
  }
  # For the binned data the vals column contains the quintile bin 
  # and the popup_data column contains the data value that was binned
  byYear <- binned_data
}

# Select only the data columns that will be displayed on the map
thedata <- byYear[c('State','year','vals', 'popup_data')]


# Add the data label (for the map popup) to the data table
# not efficient but only way to pass the value to underlying map function
thedata$data_label <- data_label_in_popup

#
# Set up the map colors
#
if (useSequentialColors == TRUE) {
  map_color_palette <- map_color_palette_sequential # The name of the color palette that will be used.
  map_nodata_color <- map_nodata_color_sequential # dark red-brown, the color to use for no data values (values not included in the breaks, eg zero)
  map_border_color <- map_border_color_sequential  # Grey, outline color used with this divergent color palette

} else {
  map_color_palette <- map_color_palette_divergent  # The name of the color palette that will be used.
  map_nodata_color <- map_nodata_color_divergent   # dark red-brown, the color to use for no data values (values not included in the breaks, eg zero)
  map_border_color <- map_border_color_divergent   # Grey, outline color used with this divergent color palette
}


#
# Set the map breaks
#
if (! is.null(map_breaks)) {
  # Append the highest data value to the map breaks
  map_breaks <- c(map_breaks, max(na.omit(thedata$vals)))  
  map_breaks <- unique(map_breaks)
  # Number of bins into which the data will be groupped
  map_ncuts <- length(map_breaks) - 1  # this gives us the correct number of bins 
}

# Reset no data to zero or lower value
minval <- min(na.omit(thedata$vals))
if (minval > 0) {
  print("Setting no data value to 0")
  thedata$vals[is.na(thedata$vals)] <- 0
  # The label / value we use to indicate no data
  map_nodata_label <- "[0]"
} else {
  no_data_val <- minval -1
  print(paste("Setting no data value to ", no_data_val))
  thedata$vals[is.na(thedata$vals)] <- no_data_val
  # The label / value we use to indicate no data
  map_nodata_label <- paste0('[',no_data_val,']')
}
  
#
# Create the map
#
mymap <- ichoropleth2(vals~State, data = thedata,  pal=map_color_palette, nodata_color=map_nodata_color, nodata_label=map_nodata_label, my_breaks=map_breaks, ncuts=map_ncuts, 
                      animate="year", labels=show_map_labels, legend=show_map_legend, my_title=map_title, include_lowest=include_lowest_in_bin1, na.rm=TRUE)

# Below we are using a custom map that makes Washington D.C bigger (so you can click it)
# So we need to set the dataUrl
# See: https://github.com/markmarkoh/datamaps for the kinds of things you can set for map style
mymap$set(
  geographyConfig = list(
    dataUrl = "https://raw.githubusercontent.com/dlab-geo/rmapsByYear/master/data/usa_with_bigger_DC_topo.json",
    #scope = 'state_squares', # you need to define the scope if the name of the map in the json file is not "usa"
    borderColor= map_border_color,
    popupTemplate = "#! function(geo, data){
    return '<div class=\"hoverinfo\"><strong>'+ geo.properties.name + ' ' + data.year + '</strong>' + '<br>' + data.data_label + data.popup_data + '</div>';  } !#"
  )
)
 
# Save the map to an HTML file
# You can then view and animate in a web browser
mymap$save(out_map_file, cdn=TRUE)

print(paste0("You can now view your map by opening this file in your web browser: ", out_map_file))


 
## ---------------------------------------------------------------------------------------
## ichoropleth2 function customizations
## ---------------------------------------------------------------------------------------
## About the Custom dataUrl
## downloaded usa.json from datamaps github repo
## https://raw.githubusercontent.com/markmarkoh/datamaps/master/src/js/data/usa.json
## and update the coords and line number for Washington DC (move this feature below DE and MD) 
## Then converted to topojson using this command:
## topojson -o mystates4.json -p name usa.json 
## Finally, put online somewhere like github and access it via http (not https), eg
## "http://raw.githubusercontent.com/dlab-geo/rMaps/master/data/usa_with_bigger_DC_topo.json""
## Note, when the custom topojson is not from datamaps you need to make sure that the ID in the 
## topojson is the state appreviation. You can do this like so:
## topojson -o mystates4.json --id-property State usa_not_from_datamaps.json
## You also need to look inside the topojson file to see the name of the map and update the scope variable above.
## See: https://github.com/mbostock/topojson/wiki/Command-Line-Reference
## If you use a base json file other than those on Datamaps github site you need to set the scope variable
## to the name of the map as stated in the topojson file and set the setProjection properly 
## (see datamaps/rmaps documentation and the online article: http://www.r-bloggers.com/rmaps-mexico-map/ )

## NAs 
## By default NA values map with a black fill
## In order to change that we needed to update the rMaps code to set
## the fill key label and value to "[0]"
## We pass those values in as parameters to the ichoropleth2 function
## TODO - when values are not defined (eg ALaska missing rows for years without data rather than NA) things get funky.

## Helpful References
## http://statpond.com/blog/2015/10/06/weather-choropleth-map/
## http://www.r-bloggers.com/rmaps-mexico-map/


# ####################################################################################################
# GGPLOT MAPS
# after: https://rud.is/b/2014/09/26/overcoming-d3-cartographic-envy-with-r-ggplot/
# and: http://flowingdata.com/2016/09/26/the-spread-of-obesity/
# and: https://rpubs.com/walkerke/obesity_squares
# ####################################################################################################

library(rgeos)
library(rgdal) # needs gdal > 1.11.0
library(ggplot2)

# ggplot map theme
# After devtools::source_gist("https://gist.github.com/hrbrmstr/33baa3a79c5cfef0f6df")
theme_map <- function(base_size=9, base_family="") {
  require(grid)
  theme_bw(base_size=base_size, base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid=element_blank(),
          panel.margin=unit(0, "lines"),
          strip.background = element_blank(),
          plot.background=element_blank(),
          legend.justification = c(1,0), 
          legend.position = c(1,0)
    )
}

#map = readOGR("data/state_squares_from_kwalker.json", "usa") # using local squares map
#map_names <- data.frame(id=0:(length(map@data$id)-1), State=map$id )
map = readOGR(dsn="data/states_aea_akhi.geojson",layer="OGRGeoJSON") # using local squares map
map_names <- data.frame(id=0:(length(map@data$id)-1), State=map$STATE_ABBR )

map_df <- fortify(map)
map_df <- merge(map_df, map_names, by="id")
map_df2 <- merge(map_df, thedata, by="State")

# Map 1: simple map with labels
# find state centers for label points
centers <- data.frame(gCentroid(map, byid=TRUE))
centers$State <- map_names$State

gg <- ggplot()
gg <- gg + geom_map(data=map_df2, map=map_df2,
                    aes(map_id=id, x=long, y=lat, group=group),
                    color="#ffffff", fill="#bbbbbb", size=0.25)
gg <- gg + geom_text(data=centers, aes(label=State, x=x, y=y), size=3)
gg <- gg + coord_map()
gg <- gg + labs(x="", y="", title="State Squares")
gg <- gg + theme_map()
gg

# Map 2: choroplet micromap 


map_df2$colcuts <- cut(map_df2$vals,breaks = c(min(map_df2$vals),5,10,15,20,25,30,max(map_df2$vals)),right = FALSE)

gg <- ggplot()
gg <- gg + geom_map(data=map_df2[map_df2$year>2006,], map=map_df2[map_df2$year>2006,],
                    aes(map_id=id, x=long, y=lat, group=group, fill=colcuts), color="white", size=0.05)
#gg <- gg + scale_fill_continuous(low="#ccebc5", high="#084081")
#gg <- gg + scale_fill_distiller(palette = "Greens") + guides(fill = guide_legend(reverse = TRUE))
#gg <- gg + scale_fill_continuous(low="green", high="purple", guide="colorbar")
#gg <- gg + scale_fill_gradient(low = "white", high = "steelblue")
#gg <- gg + scale_fill_gradientn(low="green", high="purple", guide="colorbar",breaks=c(5,10,15,20,25,50))
##gg <- gg + scale_color_gradientn(low="green",high="purple",breaks=c(5,10,15,20,25,50),
##                    labels=c("0-5","5-10","10-15","15-20","20-25",">25"))
gg <- gg + scale_fill_brewer(palette="YlOrRd", name="Percent")
gg <- gg +facet_wrap( ~ year, ncol=4) 
#gg <- gg + geom_text(data=centers, aes(label=State, x=x, y=y), size=.5)
gg <- gg + labs(x="", y="", title="Top 1 Percent Income Shares")
gg <- gg + theme_map() 
#ggsave(gg, width=24, height=24, units="in", file="ratings2.pdf")
#ggsave(gg,file='top1.png', width=6, height=4)
ggsave(gg,file='top1b.png', width=8, height=4)

#--- add labels
gg <- gg + geom_text(data=centers, aes(label=State, x=x, y=y), size=2)
gg <- gg + labs(x="", y="", title="State Squares")
