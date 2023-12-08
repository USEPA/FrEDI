###### get_sectorInfo ######
#' This function provides information about the sectors in FrEDI.
#'
#' @description
#' This helper function returns a character vector of sector names (default) *or* a data frame of sectors with related information (e.g., associated variants, impact types, etc.), which the user can supply to the [FrEDI::run_fredi()] `sectorList` argument.
#'
#' @param description = FALSE. Logical value indicating whether to include information about each sector. Returns a dataframe if `description=TRUE` and returns a character vector of sector names if `description=FALSE` (default).
#' @param gcmOnly = FALSE. Logical value indicating whether to return only sectors with climate impacts modeled using global climate model (GCM) results.
#' @param slrOnly = FALSE. Logical value indicating whether to return only sectors with climate impacts modeled using sea level rise (SLR) scenarios.
#'
#' @details
#' If `description=FALSE` (default), this helper function returns a character vector of sector names, which the user can supply to the `sectorList` argument to [FrEDI::run_fredi()]. If `description=TRUE`, `get_sectorInfo()` returns dataframe of sectors with related information returns a dataframe containing the sectors available for FrEDI along with additional information. Sector names are in the first column, with additional columns for the associated model type ("GCM" or "SLR"), variants, impact years, and impact types in the remaining columns. Variants, impact years, and impact types vary by sector.
#'
#' Users can specify whether to return only GCM sectors *or* SLR sectors by setting `gcmOnly=TRUE` or `slrOnly=TRUE`, respectively. [FrEDI::get_sectorInfo()] will return the sectors in the form specified by `description` (see above).
#'
#' @return
#'
#' * If `description=FALSE` (default), outputs a character vector containing the names of sectors available for FrEDI.
#' * If `description=TRUE`, `, outputs a dataframe containing the names of sectors available for FrEDI in one column, with information about the sector model type, variants, impact years, and impact types in the remaining columns.
#'
#' @examples
#'
#' ### Return a character vector with the names of all of the sectors in FrEDI:
#' get_sectorInfo()
#'
#' ### Return a dataframe of all of the sectors in FrEDI (sector names and additional information)
#' get_sectorInfo(description=T, gcmOnly=T)
#'
#' ### Return a character vector with only the names of the temperature-driven sectors:
#' get_sectorInfo(gcmOnly=T)
#'
#' ### Run FrEDI for only the temperature-driven sectors and view results:
#' df_x <- run_fredi(sectorList=get_sectorInfo(gcmOnly=T))
#'
#'
#' @references Environmental Protection Agency (EPA). 2021. Technical Documentation on The Framework for Evaluating Damages and Impacts (FrEDI). Technical Report EPA 430-R-21-004, EPA, Washington, DC. Available at <https://epa.gov/cira/FrEDI/>.
#'
#' @export
#' @md
#'
###
###
get_sectorInfo <- function(
  description=F,
  gcmOnly=F,
  slrOnly=F
  ){
  if(is.null(description)){description<-F}
  if(is.null(gcmOnly    )){gcmOnly    <-F}
  if(is.null(slrOnly    )){slrOnly    <-F}
  # co_sectorsRef$sector_label
  # assign("co_sectorsRef", rDataList[["co_sectors"]])
  co_sectorsRef  <- "co_sectorsRef" |> get_frediDataObj("frediData")

  co_sectorsRef <- co_sectorsRef |>
    select(-c("sector_id")) |>
    rename(sector     = sector_label) |>
    rename(model_type = modelType) |>
    mutate(model_type = model_type |> toupper())
  ### Sort
  co_sectorsRef <- co_sectorsRef |> arrange_at(.vars=c("sector"))
  ### GCM or SLR
  gcm_string <- "GCM"
  if(gcmOnly){
    co_sectorsRef <- co_sectorsRef |> filter(model_type==gcm_string)
  } else if(slrOnly){
    co_sectorsRef <- co_sectorsRef |> filter(model_type!=gcm_string)
  }

  ### If not description, return names only
  if(!description){
    return_obj <- co_sectorsRef$sector
  } else{
    return_obj <- co_sectorsRef |> as.data.frame()
  }

  return(return_obj)
}
