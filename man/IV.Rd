% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IV.R
\name{IV}
\alias{IV}
\title{IV}
\usage{
IV(object, group, years = NA, output = "dataframe", ...)
}
\arguments{
\item{object}{Either an object of class \code{NPSForVeg} or a list of such objects}

\item{group}{A required character string indicating which group of plants should be selected. Options are: "trees", "saplings", "seedlings", "shseedlings"(indicated shrub seedlings), or "herbs'. Currently not implemented for "vines" or "shrubs".}

\item{years}{years Defaults to \code{NA}. A numeric vector indicating which years should be included. This is passed on to \code{\link{getPlants}}.}

\item{output}{Either "dataframe" or "list". Determines the output type When \code{object} is a list. "Dataframe",the default, indicates the output from all of \code{NSPForVeg} objects should be a data.frame combining allo data as if all objects were a single park. "List" will return a \code{list} where each element of the list is a \code{data.frame} from a single \code{NSPForVeg} object, and each element is named based on that object's \code{ParkCode} slot.}

\item{...}{Other arguments passed to \code{\link{SiteXSpec}}. If not used by that function the arguments will be passed to \code{\link{getPlants}}.}
}
\value{
A data.frame with the IV values for each species. Values are returned for the three separate components of IV (Density, Size and Distribution), as well as the Total. The values for each of the tree components will sum to one, so the the total will sum to three.
}
\description{
Calculates "importance values" from forest data
}
\details{
This function is used to calculate forestry importance values in conjcution with \code{\link{calcIV}}. One or more objects of class \code{NPSForVeg} are used. The arguments to the funciton are passed on to \code{\link{SiteXSpec}} and from there to \code{\link{getPlants}}, so any argument which is accepted by either of these fucntions (eg. \code{size.min} or \code{crown}) is valid. \code{getPlants} is used to acquire the relevent data.
   \code{SiteXSpec} is then used to create three site X spcies matrices - one which measures abundance per plot, one which is a measure of size per plot and one which is a presence/absence matrix. For trees, saplings and seedlings abundance is mearsured as the number of stems per plot, whereas for herb abundace is the number of quadrats a speices is found in. Size is measured as basal area for trees and saplings, height for seedlings and percent cover for herbs. 
   Once the matrices are created they are passed to \code{\link{calcIV}} which calcuates the IV values.
}

