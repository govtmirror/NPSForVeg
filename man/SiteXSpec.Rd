% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SiteXSpec.R
\name{SiteXSpec}
\alias{SiteXSpec}
\title{SiteXSpec}
\usage{
SiteXSpec(object, group, years = NA, cycles = NA, values = "count",
  output = "dataframe", species = NA, plots = NA, Total = TRUE, ...)
}
\arguments{
\item{object}{either an object of class \code{NPSForVeg} or a list of such objects}

\item{years}{Defaults to \code{NA}. A numeric vector indicating which years should be included. This is passed on to \code{\link{getPlants}}.}

\item{cycles}{Defaults to \code{NA}. A numeric vector indicating which cycles should be included. This is passed on to \code{\link{getPlants}}.}

\item{values}{Determines the data contained in the Site X Species matrix. Possible values are:
\describe{
\item{"count"}{The default. Each cell will include a count of the number of a given plant species in a given plot. For trees, saplings, seedlings, shrubs and shrub seedlings this is the number of plants. For vines, it is the number of trees a vine species grows on. For herbs it will be the number of quadrats the plant occurs in.}
\item{"size"}{For trees and saplings this is the total basal area. For tree seedlings and shrub seedlings it is the total height, and for herbs it is the total percent cover across all quadrats. For shrubs and vines there is no defined size and the function will terminate with an error.}
\item{"presab"}{Produces a presence/absence matrix. When a plant species is present in a given plot the corresponding cell value will 1, otherwise it is 0.}
}}

\item{output}{Either "dataframe" or "list". Determines the output type When \code{object} is a list. "dataframe", the default, indicates the output from all of \code{NSPForVeg} objects should be a single large \code{data.frame}, containing all sites and species from all \code{NPSForVeg} objects. "list" will return a \code{list} where each element of the list is a \code{data.frame} from a single \code{NPSForVeg} object, and each element is named based on that object's \code{ParkCode} slot.}

\item{species}{A character vector of names. Defaults to \code{NA}. When not \code{NA} only species included in \code{species} will be included in the matrix. If a name is in \code{species}, but is not present in the data, it will not appear in the output.}

\item{plots}{A character vector of plot names. Defaults to \code{NA}. When not \code{NA} only plots included in \code{plots} will be included in the matrix. If a plot name is in \code{plots}, but is not present in the data, it will not appear in the output.}

\item{Total}{Logical value. Determine if a "Total" column will be included in the output. Defaults to TRUE.}

\item{...}{Other arguments passed on to \code{\link{getPlants}}}

\item{groups}{A required character string indicating which group of plants should be selected. Options are: "trees", "saplings", "seedlings","shrubs", "shseedlings" (indicated shrub seedlings), "vines" or "herbs'.}
}
\description{
Produces a Site X Species matrix. Each cell can contain a count, a measure of size or 1/0 indicating presence/abscence
}
\details{
This function will first call \code{\link{getPlants}} to retrieve the plant data. Then a SiteXSpecies matrix will be created. Values in the cells are determined by the option selected with \code{values}. Each row corresponds to a different plot and each column to a different species. 

Note that \code{species} can be a vector of common or Latin names. If common names are used then \code{common=TRUE} must be included in the function call.
}

