#' @title getSoils
#' 
#' @description Retrieves soil chemistry data from an \code{NPSForVeg} object.
#' 
#' @param object Either an object of class \code{NPSForVeg} or a list of such objects
#' @param values  Defaults to \code{NA}. A required character string indicating which horizon should be selected. Options are "Ohor" or "Ahor".
#' @param cycles Defaults to \code{NA}. A numeric vector indicating which cycles should be included. This is determined by matching the appropriate \code{Cycle} field to \code{cycles}
#' @param years  Defaults to \code{NA}. A numeric vector indicating which years should be included. This is determined by matching the appropriate \code{Sample_Year} field to \code{years}
#' @param plots Defaults to \code{NA} A character vector indicating which plots should be included. This is determined by macthing the appropriate \code{Plot_Name} field to \code{plots}.
#' @param output Either "dataframe" or "list". Determines the output type When \code{object} is a list. "Dataframe", the default, indicates the output from all of \code{NSPForVeg} objects should be combined into a single \code{data.frame}. "List" will return a \code{list} where each element of the list is a \code{data.frame} from a single \code{NPSForVeg} object, and each element is named based on that objects \code{ParkCode} slot. 
#' 
#' @details This function extracts data on soil chemistry from an \code{NPSForVeg} object. This function is called by other analysis and graphing functions. 
#' 
#' @export



setGeneric(name="getSoils",function(object,values="Ohor",cycles=NA, years=NA, plots=NA, output="dataframe"){standardGeneric("getSoils")}, signature="object")

setMethod (f="getSoils", signature="list",
          function(object,values,cycles,years,plots,output) 
          {OutSoils<-lapply(X=object, FUN=getSoils, cycles=cycles, years=years, plots=plots, output=output)
           switch(output,
                  list=return(OutSoils),
                  dataframe=return(do.call("rbind",OutSoils))
           )
          })


setMethod(f="getSoils", signature=c(object="NPSForVeg"), 
          function(object,values,cycles,years,plots,output){
            switch(values,
                     all=XSoils<-object@Soils,
                     Ohor=XSoils<-object@Soils[object@Soils$Horiz=="O",],
                     Ahor=XSoils<-object@Soils[object@Soils$Horiz=="A",],
                     stop("Unknown Soil Type"))

 #           if(!sum(is.na(values))>0) XSoils<-XSoils[XSoils$Horiz %in% values,]
            
            if(!sum(is.na(cycles))>0) XSoils<-XSoils[XSoils$Cycle %in% cycles,]
            
            if(!sum(is.na(years))>0) XSoils<-XSoils[XSoils$Sample_Year %in% years,]
            
            if(!sum(is.na(plots))>0) XSoils<-XSoils[XSoils$Plot_Name %in% plots,]
            
            return(XSoils)
})






