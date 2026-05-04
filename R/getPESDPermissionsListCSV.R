#' @title getPESDPermissionsListCSV
#' @description
#' Connects to the PESD Management Team SharePoint site and retrieves an exported csv version of "Staff_Permissions"
#' list, returning it as a data frame.  The original list lives at
#' https://086gc.sharepoint.com/sites/msteams_74c888-ManagementTeam/Lists/Staff_Permissions/AllItems.aspx but MS tools
#' don't allow the extraction of staff names, which exist in the csv. Power Automate is used to extract the list to csv
#' every day.
#'
#' This function fetches permission-related details for staff as maintained in the PESD SharePoint Online list. Columns
#' include employee identifier, organizational unit, mailing lists, employee roles, and permission confirmations. Only
#' the most relevant metadata columns are returned.
#' @param name  Defaults to \code{NULL}.This is a filter - text entered here will limit the results to only those records containing your
#' string in the name field (e.g entering "Mik", would return entries with names like "Mike).
#' @param section Defaults to \code{NULL}. This is a filter - valid values include \code{"ADMIN"}, \code{"ISAR"}, \code{"SALMON"}, \code{"DADSS"}, \code{"GPSS"},
#' or \code{"ALL"}.
#' @param unit  Defaults to \code{NULL}.  This is a filter - text entered here will limit the results to only those records containing your
#' string in the unit field (e.g. entering "TURT" would return the entries for the turtle unit)
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @returns A data frame
#' @seealso
#'   \code{\link{getPESDMailingLists}},
#'   \code{\link{getSharepointSiteMembers}}
#' @export
getPESDPermissionsListCSV <- function(name=NULL, section= NULL, unit=NULL){
  site <- suppressMessages( Microsoft365R::get_sharepoint_site(site_url="https://086gc.sharepoint.com/sites/msteams_74c888-ManagementTeam"))
  drive <- site$get_drive()
  tmp <- tempfile(fileext = ".csv")
  drive$download_file(
    src = "Management Team/automation/staff_permissions.csv",
    dest = tmp,
    overwrite = TRUE
  )
  clean_bool <- function(x) {
    x <- toupper(x)
    if (x %in% c("TRUE", "FALSE")) return(x)
    if (is.na(x) || x == "") return(FALSE)
    NA
  }

  extract_guest_unit_values <- function(x) {
    # Handle missing or empty values gracefully
    if(is.na(x) || x == "" || x == "[]") return(NA_character_)
    vals <- tryCatch(jsonlite::fromJSON(x)$Value, error=function(e) NA_character_)
    if(is.na(vals[1]) || length(vals) == 0) return(NA_character_)
    # vals <- sub("^[^:]*:", "", vals)       # Remove everything before (and including) the colon
    paste(vals, collapse="; ")             # Combine with semicolon
  }

  df <- utils::read.csv(tmp, stringsAsFactors = FALSE)
  df$DISPLAYNAME <- sapply(df$EmployeeName, function(x) jsonlite::fromJSON(x)$DisplayName)
  df$HOMEUNIT   <- sapply(df$HomeUnit, function(x) jsonlite::fromJSON(x)$Value)
  df$LOCATION <- toupper(sapply(df$Location, function(x) jsonlite::fromJSON(x)$Value))
  df$MAILLIST <- sapply(df$MailingList, function(x) jsonlite::fromJSON(x)$Value)
  df$EMAIL      <- tolower(sub("i:0#.f\\|membership\\|", "", df$EmployeeName.Claims))
  df$MAILLIST_CONFIRMED   <- sapply(df$MailingListConfirmed, clean_bool)
  df$SECTION     <- trimws(sub(":.*", "", df$HOMEUNIT))
  df$UNIT        <- trimws(sub(".*:", "", df$HOMEUNIT))
  df$IS_UNITLEAD   <- sapply(df$UnitLead, clean_bool)
  df$UNITLIBRARY_GRANTED   <- sapply(df$HomeLibraryGranted, clean_bool)
  df$IS_MANAGEMENT   <- sapply(df$Management, clean_bool)

  df$GUESTUNITS <- sapply(df$GuestUnit_x0028_s_x0029_, extract_guest_unit_values)
  df <- df[,c("DISPLAYNAME", "EMAIL", "IS_MANAGEMENT", "MAILLIST","MAILLIST_CONFIRMED","SECTION", "IS_UNITLEAD", "UNIT", "UNITLIBRARY_GRANTED","LOCATION","GUESTUNITS")]
  if (!is.null(name)) df <-df[grepl(name,df$DISPLAYNAME, ignore.case = T),]
  if (!is.null(section)) df <-df[df$SECTION == section,]
  if (!is.null(unit)) df <-df[grepl(unit, df$UNIT, ignore.case = T),]
  return(df)
}
