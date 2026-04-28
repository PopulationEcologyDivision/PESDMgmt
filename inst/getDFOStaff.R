#' @title getDFOStaff
#' @description
#' Retrieves a data frame containing all users (staff members) present in the organization, using the Microsoft Graph
#' API. The function authenticates using a SharePoint site token (obtained from Microsoft365R via a site name or ID),
#' and then iterates through the paginated list of users, returning their IDs, names, emails, job titles, and office
#' locations.
#'
#' The \code{site} parameter is used only to authenticate via Microsoft365R, in order to obtain a Microsoft Graph
#' access token authorized for your tenant. The choice of site does not filter the staff being returned – all users
#' visible to the tenant are included.
#' @param site Defaults to \code{"msteams_74c888"}. Character string. The short name or unique ID of the SharePoint
#' Online site.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @returns A data frame with one row per user, and columns: \code{id}, \code{displayName}, \code{mail},
#' \code{jobTitle}, \code{officeLocation}.
#' @export
#'
#' @examples
#' \dontrun{
#' # Fetch all members of the default SharePoint site
#' staff <- getDFOStaff(site = "msteams_74c888")
#' # Examine the first few members
#' head(members)
#' }
getDFOStaff <- function(site = "msteams_74c888") {
  site <- Microsoft365R::get_sharepoint_site(site_url=paste0("https://086gc.sharepoint.com/sites/",site))
  token <- site$token$credentials$access_token
  base_url <- "https://graph.microsoft.com/v1.0/users?$top=999&$select=id,displayName,mail,jobTitle,officeLocation"
  next_url <- base_url
  all_users <- list()

  repeat {
    resp <- httr::GET(next_url, httr::add_headers(Authorization = paste("Bearer", token)))
    httr::stop_for_status(resp)
    resp_content <- httr::content(resp, as = "parsed", simplifyVector = TRUE)
    if (!is.null(resp_content$value) && length(resp_content$value) > 0) {
      all_users <- c(all_users, list(as.data.frame(resp_content$value, stringsAsFactors = FALSE)))
    }
    if (!is.null(resp_content$`@odata.nextLink`)) {
      next_url <- resp_content$`@odata.nextLink`
    } else {
      break
    }
  }
  users_df <- dplyr::bind_rows(all_users)
  return(users_df)
}
