#' @title getSoilChemNames
#' 
#' @description Retreives soil chemsitry variable names from an NPSForVeg object of a list of such objects.
#' 
#' @param object Either an NPSForVeg object or a list of such objects
#' @param chem.names Type of name to return. One of two options, in quotes.
#' \describe{
#' \item{"chemvar"}{ The default. Returnes the short name of the soil chemistry variable}
#' \item{"varlong"}{Returns the long, format name of the soil chemistry variable}
#' } 
#' @return A character vector with one or more soil chemistry names. 
#' 
#' @export

setGeneric(name="getSoilChemNames",function(object,chem.names="chemvar"){standardGeneric("getSoilChemNames")},signature=c("object") )


setMethod(f="getSoilChemNames", signature=c(object="list"),
  function(object,chem.names){
    sapply(object,FUN=getSoilChemNames, chem.names=ch.names) } )  
 


setMethod(f="getSoilChemNames", signature=c(object="NPSForVeg"),
          function(object,chem.names){
            switch(chem.name,
            chemvar = return(object@ChemVars$ChemVar),
            varlong = return(object@ChemVars$ChemistryVariable),
            stop("Unrecognized type in getSoilChemNames")
            )
})

