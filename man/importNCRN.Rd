% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/importNCRN.R
\name{importNCRN}
\alias{importNCRN}
\title{importNCRN}
\usage{
importNCRN(Dir)
}
\arguments{
\item{Dir}{The directory where the data is found}
}
\value{
Returns 11 \code{NPSForVeg} objects, one for each park, named using the standard 4 letter park code (e.g. ANTI, CATO etc.) and a list, named NCRN, which contians all 11 objects.
}
\description{
This function imports data from the standard NCRN .csv files and saves it as \code{NPSForVeg} objects. The required .csv files are: Plots, Events, Trees, Saplings, Seedlings, Shrubs, Shrub_Seedlings, Vines, Herbs and CommonNames.
}

