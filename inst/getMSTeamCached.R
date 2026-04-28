#' @title getMSTeamCached
#' @description
#' Extract Cached Microsoft Teams User Information from SharePoint
#'
#' Retrieves cached user profile information from the "User Information List" of a specified SharePoint Online/MS Teams site.
#'
#' This SharePoint system list acts as a cache of users who have interacted with the site, and includes basic user metadata such as job title, office, email, SIP (Skype) address, and user name. Note: this will not necessarily include all staff in your organization—only those who have interacted with this SharePoint site or its associated MS Team.
#'
#' @param site Character string. The short name or unique ID of the SharePoint Online site (e.g., `"msteams_74c888"`). Defaults to `"msteams_74c888"`.
#' @return A data frame with one row per user, and columns:
#'   \describe{
#'     \item{id}{User's integer SharePoint ID within this site list}
#'     \item{ContentType}{Content Type string (should be "Person" for users)}
#'     \item{JobTitle}{Job title (if available)}
#'     \item{Office}{Office location (if available)}
#'     \item{UserName}{Username as stored on the site}
#'     \item{EMail}{Email address}
#'     \item{SipAddress}{Instant messaging (SIP) address, if set}
#'   }
#'
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get cached user information for the default site
#' cached_users <- getMSTeamCached()
#' # Preview a few records
#' head(cached_users)
#' }
getMSTeamCached <- function(site = "msteams_74c888"){
  site <- Microsoft365R::get_sharepoint_site(site_url=paste0("https://086gc.sharepoint.com/sites/",site))
  user_list <- site$get_list("User Information List")
  user_info_df <- user_list$list_items()
  # browser()
  # user_info_df <- user_info_df[,c("id", "ContentType", "JobTitle", "Office", "UserName", "EMail", "SipAddress")]
  return(user_info_df)
}
