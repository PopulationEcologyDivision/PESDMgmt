#' @title getPESDPermissionsList
#' @description
#' Connects to the PESD Management Team SharePoint site and retrieves the "Staff_Permissions" list, returning it as a
#' data frame.
#'
#' This function fetches permission-related details for staff as maintained in the PESD SharePoint Online list. Columns
#' include employee identifier, organizational unit, mailing lists, employee roles, and permission confirmations. Only
#' the most relevant metadata columns are returned.

#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @returns
#' @export
#'
#' @examples
getPESDPermissionsList <- function(){
  site <- Microsoft365R::get_sharepoint_site(site_url="https://086gc.sharepoint.com/sites/msteams_74c888-ManagementTeam")
  staff_permissions_list <- site$get_list("Staff_Permissions")
  staff_permissions_df <- staff_permissions_list$list_items(
    # select = c(
    #   "EmployeeNameLookupId",
    #   "HomeUnit",
    #   "HubSite",
    #   "MailingList",
    #   "MailingListConfirmed",
    #   "Management",
    #   "UnitLead",
    #   "HomeLibraryGranted",
    #   "Location"
    # )
  )
  return(staff_permissions_df)
}
