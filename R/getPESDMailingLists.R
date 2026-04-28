#' @title getPESDMailingLists
#' @description
#' Retrieve Members of a DFO Maritimes Science PED Distribution Group
#'
#' Fetches the display names and email addresses of all members of one or more DFO Maritimes Science PED Exchange
#' distribution groups, using the Microsoft Graph API.
#'
#' Authentication is performed via a SharePoint site token obtained through Microsoft365R. The function supports
#' fetching a single group by short name, or all groups simultaneously via \code{group = "all"}.
#'
#' @param group Character string. One of \code{"ADMIN"}, \code{"ISAR"}, \code{"SALMON"}, \code{"DADSS"}, \code{"GPSS"},
#' or \code{"all"}.
#'  Defaults to \code{"all"}.
#'
#' @return A data frame with one row per group member, and columns:
#'   \describe{
#'     \item{displayName}{Display name of the user (e.g., "McMahon, Mike (DFO/MPO)")}
#'     \item{mail}{Email address of the user}
#'     \item{MAIL_LIST}{Full name of the distribution group the user belongs to}
#'     \item{section}{Short name of the distribution group the user belongs to
#'       (one of \code{"ADMIN"}, \code{"ISAR"}, \code{"SALMON"}, \code{"DADSS"},
#'       \code{"GPSS"})}
#'   }
#'   When \code{group = "all"}, rows from all five groups are concatenated.
#'   A user appearing in multiple groups will have one row per group.
#'
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @seealso
#'   \code{\link{getPESDPermissionsListCSV}},
#'   \code{\link{getSharepointSiteMembers}}
#' @export
#'
#' @examples
#' \dontrun{
#' # Get members of all distribution groups
#' all_members <- getPESDMailingLists()
#'
#' # Get members of a single group
#' isar_members <- getPESDMailingLists(group = "ISAR")
#'
#' # Preview results
#' head(all_members)
#' }
getPESDMailingLists <- function(group = "all") {
  group <- match.arg(group, choices = c("ADMIN", "ISAR", "SALMON", "DADSS", "GPSS", "all"))

  email_map <- list(
    ADMIN  = "DFO.RMARSciencePED-ScienceDEPMARR.MPO@dfo-mpo.gc.ca",
    ISAR   = "DFO.RMARSciencePEDISAR-ScienceIEPDEPCMARR.MPO@dfo-mpo.gc.ca",
    SALMON = "DFO.RMARSciencePEDSS-ScienceDEPSSMARR.MPO@dfo-mpo.gc.ca",
    DADSS  = "DFO.RMARSciencePEDSSAS-ScienceDEPSREESMARR.MPO@dfo-mpo.gc.ca",
    GPSS   = "DFO.RMARSciencePEDGPSS-ScienceDEPSPFPPCMARR.MPO@dfo-mpo.gc.ca"
  )

  group_emails <- tolower(unlist(email_map))
  groups_to_fetch <- if (group == "all") names(email_map) else group

  site <- suppressMessages(Microsoft365R::get_sharepoint_site(site_url = "https://086gc.sharepoint.com/sites/msteams_74c888-ManagementTeam"))
  token <- site$token$credentials$access_token

  fetch_group <- function(group_name) {
    group_email <- email_map[[group_name]]

    resp <- httr::GET(
      "https://graph.microsoft.com/v1.0/groups",
      httr::add_headers(Authorization = paste("Bearer", token)),
      httr::accept_json(),
      query = list(`$filter` = paste0("mail eq '", group_email, "'"))
    )
    httr::stop_for_status(resp)
    group_id <- httr::content(resp, as = "parsed", simplifyVector = TRUE)$value$id
    resp2 <- httr::GET(
      sprintf("https://graph.microsoft.com/v1.0/groups/%s/members?$select=displayName,mail,officeLocation", group_id), #
      httr::add_headers(Authorization = paste("Bearer", token)),
      httr::accept_json()
    )
    httr::stop_for_status(resp2)
    result <- httr::content(resp2, as = "parsed", simplifyVector = TRUE)$value
    result$`@odata.type` <- NULL
    result$LOC <- NA
    result$section <- group_name
    result$MAIL_LIST <- group_email
    result$mail <- tolower(result$mail)
    result <- result[!tolower(result$mail) %in% group_emails, ]
    result[grepl("BIO|Fish Lab|Fishlab|Strickland|Ellis|Bedford|Steenburg", result$officeLocation, ignore.case = T),"LOC"] <- "BIO"
    result[grepl("Andrews|George|Penhallow|SABS", result$officeLocation, ignore.case = T),"LOC"] <- "SABS"
    result[grepl("MACTAQUAC", result$officeLocation, ignore.case = T),"LOC"] <- "MACTAQUAC"
    result[grepl("COLDBROOK", result$officeLocation, ignore.case = T),"LOC"] <- "COLDBROOK"
    result[grepl("Yarmouth", result$officeLocation, ignore.case = T),"LOC"] <- "YARMOUTH"

    result[grepl("king",result$displayName, ignore.case = T)&grepl("king",result$displayName, ignore.case = T) ,"LOC"] <- "OTHER"
    result[grepl("glass",result$displayName, ignore.case = T)&grepl("amy",result$displayName, ignore.case = T),"LOC"] <- "BIO"
    result[grepl("broome",result$displayName, ignore.case = T)&grepl("jeremy",result$displayName, ignore.case = T),"LOC"] <- "BIO"
    result[grepl("wilson",result$displayName, ignore.case = T)&grepl("megan",result$displayName, ignore.case = T),"LOC"] <- "BIO"
    result[grepl("wilson",result$displayName, ignore.case = T)&grepl("tyler",result$displayName, ignore.case = T),"LOC"] <- "BIO"
    result[grepl("bowlby",result$displayName, ignore.case = T)&grepl("heather",result$displayName, ignore.case = T),"LOC"] <- "BIO"
    result[grepl("billard",result$displayName, ignore.case = T)&grepl("mark",result$displayName, ignore.case = T),"LOC"] <- "BIO"
    result[grepl("taylor",result$displayName, ignore.case = T)&grepl("andrew",result$displayName, ignore.case = T),"LOC"] <- "BIO"
    result[grepl("collier",result$displayName, ignore.case = T)&grepl("lynn",result$displayName, ignore.case = T),"LOC"] <- "SABS"
    result[grepl("li",result$displayName, ignore.case = T)&grepl("lingbo",result$displayName, ignore.case = T),"LOC"] <- "OTHER"
    result[grepl("finley",result$displayName, ignore.case = T)&grepl("monica",result$displayName, ignore.case = T),"LOC"] <- "SABS"
    result[is.na(result$LOC),"LOC"] <- "OTHER"
    return(result)
  }

  res <- dplyr::bind_rows(lapply(groups_to_fetch, fetch_group)) |> dplyr::arrange(displayName)
  return(res)
}
