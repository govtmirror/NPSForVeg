#' @title S4 Class Definition for NPSForveg
#' 
#' @description
#' |An S4 class that contains the data from forest monitoring from a set of forest montoirng plots. This will typcially be a single park, but it could be any group of plots, including part of a park or a group of parks. 
#' @slot ParkCode A short code to designate the park (or group of plots), typically an NPS 4 letter code. Stored as a length 1 character vector. 
#' @slot ShortName A short name for the park. Stored as a length 1 character vector. 
#' @slot LongName  A long, formal name for the park. Stored as a length 1 character vector. 
#' @slot Network The code for the I&M network the park belongs to. Stored as a length 1 character vector. 
#' @slot TPlotSize A length 2 numeric vector. The first element is the number of subplots where trees are monitored at each plot. The second element is the size of each of the subplots in meters squared.
#' @slot SapPlotSize A length 2 numeric vector. The first element is the number of microplots where saplings are monitored at each plot. The second element is the size of each of the microplots in meters squared.
#' @slot SeedPlotSize A length 2 numeric vector. The first element is the number of quadrats where seedlings are monitored at each plot. The second element is the size of each of the quadrats in meters squared.
#' @slot ShrubPlotSize  length 2 numeric vector. The first element is the number of microplots where shrbus are monitored at each plot. The second element is the size of each of the microplots in meters squared.
#' @slot ShSeedPlotSize A length 2 numeric vector. The first element is the number of quadrats where shrub seedlings are monitored at each plot. The second element is the size of each of the quadrats in meters squared.
#' @slot VPlotSize  A length 2 numeric vector. The first element is the number of subplots where vines on trees are monitored at each plot. The second element is the size of each of the subplots in meters squared.
#' @slot HPlotSize A length 2 numeric vector. The first element is the number of quadrats where herbacoius plants are monitored at each plot. The second element is the size of each of the quadrats in meters squared. Note - these need not be actually herbacious plants, but it is assumed that cover of each species is measured rather than the dbh or height.
#' @slot Plots a data.frame with information on the plots
#' @slot Events a data.frame with information on each sampling event
#' @slot Trees a data.frame with  tree data 
#' @slot Saplings a data.frame with sapling data
#' @slot Seedlings a data.frame with seedling data
#' @slot Shrubs a data.frame with shrub data
#' @slot ShSeedlings a data.frame with shrub seedling data
#' @slot Vines a data.frame with vine data
#' @slot Herbs a data.frame with herbacious plant data
#' @slot Commons a data.frame which links common names to Latin names
#' 
#' @exportClass NPSForVeg

setClass(Class="NPSForVeg",           ### Name of the Class
  slots=c(ParkCode="character",       ### NPS Park Code                     
          ShortName="character",      ### Useful short name for park
          LongName="character",       ### Formal long name for park
          
          Network="character",        ### Code for I&M Network
          
          TPlotSize="numeric",        ### Number and area of the plots where trees are sampled in m^2
          SapPlotSize="numeric",      ### Number and area of the plots where saplilngs are sampled in m^2
          SeedPlotSize="numeric",     ### Number and area of the plots where tree seedings are sampled in m^2
          ShrubPlotSize="numeric",    ### Number and area of the plots where shrubs are sampled in m^2
          ShSeedPlotSize="numeric",   ### Number and area of the plots where shub seedings are sampled in m^2
          VPlotSize="numeric",        ### Number and area of the plots where vines are sampled in m^2
          HPlotSize="numeric",        ### Number and area of the plots where herbs are sampled in m^2
          
          
          Plots="data.frame",         ### Data.frame with plot information
          Events="data.frame",        ### Data.farme with event information
          Trees="data.frame",         ### Data.frame with tree data
          Saplings="data.frame",      ### Data.frame with sapling data
          Seedlings="data.frame",     ### Data.frame with tree seedling data
          Shrubs="data.frame",        ### Data.frame with shrub data
          ShSeedlings="data.frame",   ### Data.frame with shrub seeling data
          Vines="data.frame",         ### Data.frame with vines on trees data
          Herbs="data.frame",         ### Data.frame with herbaceoous/ground cover data
          Commons="data.frame"        ### Data.frame with Latin and common names
  )
)
