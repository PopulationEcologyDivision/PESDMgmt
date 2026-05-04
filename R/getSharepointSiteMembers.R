#' @title getSharepointSiteMembers
#' @description
#' Retrieves details on all members of a specified SharePoint Online site (represented by a Microsoft 365 Group). For
#' each member, the function returns their user profile properties such as display name, email, user principal name,
#' account status, city, job title, and unique id.
#'
#' The `site` parameter is the short name or unique ID of the SharePoint site (e.g., `"msteams_74c888"`). The function
#' uses the Microsoft365R package to authenticate and connect, and assumes your R session is already set up to access
#' Microsoft 365 resources.
#'
#' @param site Defaults to \code{"msteams_74c888"}. Character string. The short name or unique ID of the SharePoint
#' Online site.
#' @return A data frame (tibble) with one row per site member, and columns:
#'   \describe{
#'     \item{accountEnabled}{Logical. Whether the user's account is enabled.}
#'     \item{displayName}{Character. Display name of the user.}
#'     \item{city}{Character. City (if available) of the user.}
#'     \item{mail}{Character. Email address.}
#'     \item{userPrincipalName}{Character. User principal name (UPN).}
#'     \item{id}{Character. Unique user id in Azure AD.}
#'     \item{jobTitle}{Character. Job title (if available).}
#'   }
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @seealso
#'   \code{\link{getPESDMailingLists}},
#'   \code{\link{getPESDPermissionsListCSV}}
#' @export
#'
#' @examples
#' \dontrun{
#' # Fetch all members of the default SharePoint site
#' members <- getSharepointSiteMembers()
#' # Examine the first few members
#' head(members)
#' }
getSharepointSiteMembers <- function(site = "msteams_74c888"){
  site <- match.arg(site, choices = c("msteams_74c888", "pesddadss", "pesdisar", "pesdgpss", "pesdsalmon", "msteams_74c888-ManagementTeam"))
  site <- suppressMessages(Microsoft365R::get_sharepoint_site(site_url=paste0("https://086gc.sharepoint.com/sites/",site)))
  group <- site$get_group()
  members <- group$list_members()
  wanted <- c("displayName", "mail", "city", "officeLocation", "streetAddress", "accountEnabled")
  member_df <- dplyr::bind_rows(lapply(members, function(x) {
    props <- jsonlite::fromJSON(jsonlite::toJSON(x$properties, auto_unbox = TRUE), flatten = TRUE)
    props <- props[names(props) %in% wanted]
    props <- lapply(props, function(z) {
      if (length(z) == 0) NA
      else paste(unlist(z), collapse = "; ")
    })
    as.data.frame(props, stringsAsFactors = FALSE)
  }))
  member_df[grepl("Dartmouth", member_df$city, ignore.case = T),"LOCATION"] <- "BIO"
  member_df[grepl("Andrews|George|Penhallow|SABS", member_df$city, ignore.case = T),"LOCATION"] <- "SABS"
  member_df[grepl("French Village", member_df$city, ignore.case = T),"LOCATION"] <- "MACTAQUAC"
  member_df[grepl("Coldbrook", member_df$city, ignore.case = T),"LOCATION"] <- "COLDBROOK"
  member_df[grepl("Yarmouth", member_df$city, ignore.case = T),"LOCATION"] <- "YARMOUTH"
  member_df[is.na(member_df$LOCATION),"LOCATION"] <- "OTHER"

  member_df <- member_df |>
    dplyr::rename(ACCOUNTENABLED = accountEnabled,
                  DISPLAYNAME = displayName,
                  EMAIL=mail) |>
    dplyr::mutate(EMAIL = tolower(EMAIL)) |>
    dplyr::select(DISPLAYNAME, EMAIL, LOCATION,ACCOUNTENABLED)

  return(member_df)
}
