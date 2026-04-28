#' @title Automatically Generate Email Lists
#' @description
#' Generates collapsed, semicolon-separated email lists from PESD mailing lists,
#' permissions, and SharePoint site member data. Lists can be grouped by various
#' organizational categories.
#'
#' @param by character. The grouping category for the email list. Must be one of:
#'   \itemize{
#'     \item \code{"all"} - A single list containing all staff (default)
#'     \item \code{"location"} - Lists grouped by office location
#'     \item \code{"lists"} - Lists grouped by Outlook mailing list
#'     \item \code{"section"} - Lists grouped by section (note: may not be present for all staff)
#'     \item \code{"unit"} - Lists grouped by unit (note: may not be present for all staff)
#'     \item \code{"lead"} - A single list containing only unit leads (note:  may not be present for all staff)
#'   }
#' @param debug logical. If \code{TRUE}, the function will use pre-existing
#'   objects from the global environment (\code{PESDPermissionsListCSV},
#'   \code{SharepointSiteMembers}, \code{PESDMailingLists}) rather than
#'   fetching fresh data. Useful for development and testing. Default is
#'   \code{FALSE}.
#'
#' @return A named list where each element is a semicolon-separated string of
#'   email addresses. List names correspond to the grouping category (e.g.
#'   location names, section names, etc.). If a record has no value for the
#'   grouping category, it will be stored under the key \code{"NA"}.
#'
#' @details
#' Data is sourced from three places:
#' \itemize{
#'   \item Outlook mailing lists via \code{getPESDMailingLists()}
#'   \item A permissions CSV via \code{getPESDPermissionsListCSV()}
#'   \item SharePoint site members via \code{getSharepointSiteMembers()}
#' }
#' When grouping by \code{"section"}, \code{"unit"}, or \code{"lead"}, the
#' mailing list data is joined to the permissions data on email address. Staff
#' present in the mailing list but absent from the permissions data will be
#' grouped under \code{"NA"}.
#'
#' @note When grouping by \code{"lists"}, the mailing list
#'   \code{DFO.RMARSciencePED-ScienceDEPMARR.MPO@@dfo-mpo.gc.ca} includes all
#'   members of all other listed mailing lists in addition to its own members.
#'
#' @seealso
#'   \code{\link{getPESDMailingLists}},
#'   \code{\link{getPESDPermissionsListCSV}},
#'   \code{\link{getSharepointSiteMembers}}
#'
#' @examples
#' \dontrun{
#' # Get all staff in a single list
#' all_staff <- autoMakeEmailList()
#' cat(all_staff[["all"]])
#'
#' # Get lists grouped by location
#' by_location <- autoMakeEmailList(by = "location")
#' cat(by_location[["BIO"]])
#'
#' # Get lists grouped by section
#' by_section <- autoMakeEmailList(by = "section")
#'
#' # Use cached data for testing
#' by_unit <- autoMakeEmailList(by = "unit", debug = TRUE)
#' }
#'
#' @export
autoMakeEmailList <- function(by = "all", debug = F) {
  by <- tolower(by)
  by <- match.arg(by, choices = c("all", "location", "lists", "section", "unit", "lead"))

  if (!debug){
    message("extracting!|")
    permissions <- getPESDPermissionsListCSV()
    site_members <- getSharepointSiteMembers()
    mailing_lists <- getPESDMailingLists("all")
  }else{
    permissions <-.GlobalEnv$PESDPermissionsListCSV
    site_members <-.GlobalEnv$SharepointSiteMembers
    mailing_lists <-.GlobalEnv$PESDMailingLists
  }

  mailing_lists <- mailing_lists |>
    dplyr::rename_with(~ paste0("outlk_", .)) |>
    dplyr::select(outlk_mail, outlk_displayName, outlk_LOC, outlk_MAIL_LIST) |>
    dplyr::arrange(outlk_displayName)

  permissions <- permissions |>
    dplyr::rename_with(~ paste0("permiss_", .)) |>
    dplyr::select(permiss_displayName, permiss_email, permiss_UnitLead, permiss_SECTION, permiss_UNIT, permiss_LOC) |>
    dplyr::arrange(permiss_displayName)

    site_members <-  site_members |>
    dplyr::rename_with(~ paste0("sp_", .)) |>
    dplyr::select(sp_displayName, sp_mail, sp_LOC) |>
    dplyr::arrange(sp_displayName)

    res <- list()
  if (grepl("all",by,ignore.case = T)){
    res[["all"]] <- paste(mailing_lists$outlk_mail, collapse = ";")
  }else if (grepl("loc",by,ignore.case = T)){
    cats <- unique(mailing_lists$outlk_LOC)
    for (cat in cats) {
      subset_data <- mailing_lists[mailing_lists$outlk_LOC == cat, ]
      res[[cat]] <- paste(subset_data$outlk_mail, collapse = ";")
    }
  }else if (grepl("list",by,ignore.case = T)){
    cats <- unique(mailing_lists$outlk_MAIL_LIST)
    for (cat in cats) {
      subset_data <- mailing_lists[mailing_lists$outlk_MAIL_LIST == cat, ]
      res[[cat]] <- paste(subset_data$outlk_mail, collapse = ";")
    }
    cat("In addition to the listed staff, the mailing list 'DFO.RMARSciencePED-ScienceDEPMARR.MPO@dfo-mpo.gc.ca' also
includes all of the members of all of the other listed mailing lists\n")
  }else if (grepl("sect",by,ignore.case = T)){
    cat("Section info is stored separately from Outlook, and may not be present for all staff\n")
    joined <- dplyr::full_join(mailing_lists, permissions, by = c("outlk_mail" = "permiss_email"), keep = T)
    joined <- joined[!is.na(joined$outlk_mail),]
    cats <- unique(joined$permiss_SECTION)

    for (cat in cats) {
      if (is.na(cat)) {
        subset_data <- joined[is.na(joined$permiss_SECTION), ]
        res[["NA"]] <- paste(subset_data$outlk_mail, collapse = ";")
      } else {
        subset_data <- joined[!is.na(joined$permiss_SECTION) & joined$permiss_SECTION == cat, ]
        res[[cat]] <- paste(subset_data$outlk_mail, collapse = ";")
      }
    }

  }else if (grepl("unit",by,ignore.case = T)){
    cat("Unit info is stored separately from Outlook, and may not be present for all staff\n")
    joined <- dplyr::full_join(mailing_lists, permissions, by = c("outlk_mail" = "permiss_email"), keep = T)
    joined <- joined[!is.na(joined$outlk_mail),]
    cats <- unique(joined$permiss_UNIT)

    for (cat in cats) {
      if (is.na(cat)) {
        subset_data <- joined[is.na(joined$permiss_UNIT), ]
        res[["NA"]] <- paste(subset_data$outlk_mail, collapse = ";")
      } else {
        subset_data <- joined[!is.na(joined$permiss_UNIT) & joined$permiss_UNIT ==  cat, ]
        res[[cat]] <- paste(subset_data$outlk_mail, collapse = ";")
      }
    }
  }else if (grepl("lead",by,ignore.case = T)){
    cat("Unit lead info is stored separately from Outlook, and may not be present for all staff\n")
    joined <- dplyr::full_join(mailing_lists, permissions, by = c("outlk_mail" = "permiss_email"), keep = T)
    joined <- joined[!is.na(joined$outlk_mail),]
    joined <- joined[which(joined$permiss_UnitLead=="True"),]
    res[["leads"]] <- paste(joined[which(joined$permiss_UnitLead=="True"),"outlk_mail"], collapse = ";")
  }
  return(res)
  #QC
}
