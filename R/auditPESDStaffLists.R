#' @title auditPESDStaffLists
#' @description
#' Audit Staff Consistency Across PESD Data Sources
#'
#' Compares staff email addresses across three data sources—the PESD Staff
#' Permissions list, the PESD mailing lists, and the SharePoint site
#' membership—and reports any discrepancies. For each email address found
#' across all sources, the function indicates which sources it appears in,
#' reporting only those emails not present in all three sources.
#'
#' Results are printed to the console as a formatted table and returned
#' invisibly as a data frame for further programmatic use.
#'
#' @param debug logical. If \code{TRUE}, the function will use pre-existing
#'   objects from the global environment (\code{PESDPermissionsListCSV},
#'   \code{SharepointSiteMembers}, \code{PESDMailingLists}) rather than
#'   fetching fresh data. Useful for development and testing. Default is
#'   \code{FALSE}.
#' @return Invisibly returns a data frame with columns:
#'   \describe{
#'     \item{email}{Email address}
#'     \item{permissions}{Logical. Whether the email appears in the permissions list}
#'     \item{mailing}{Logical. Whether the email appears in the mailing lists}
#'     \item{site}{Logical. Whether the email appears in the SharePoint site membership}
#'   }
#'   Only rows where the email is absent from at least one source are included.
#'
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#'
#' @examples
#' \dontrun{
# permissions   <- getPESDPermissionsListCSV()
#' mailing_lists <- getDistributionGroupMembers()
#' site_members  <- getSharepointSiteMembers()
#'
#' # Print discrepancy table to console
#' auditPESDStaffLists(permissions, mailing_lists, site_members)
#'
#' # Capture results for further use
#' audit <- auditPESDStaffLists(permissions, mailing_lists, site_members)
#' audit[!audit$permissions, ]  # emails missing from permissions
#' }
auditPESDStaffLists <- function(debug=F) {
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

  emails_permissions <- tolower(permissions$EMAIL)
  emails_mailing     <- tolower(mailing_lists$EMAIL)
  emails_site        <- tolower(site_members$EMAIL)

  all_emails <- unique(c(emails_permissions, emails_mailing, emails_site))

  # For each email, record which sources it appears in
  audit_df <- data.frame(
    email       = all_emails,
    permissions = all_emails %in% emails_permissions,
    mailing     = all_emails %in% emails_mailing,
    site        = all_emails %in% emails_site,
    stringsAsFactors = FALSE
  )

  # Only report emails that are not in all three sources
  audit_df <- audit_df[!(audit_df$permissions & audit_df$mailing & audit_df$site), ]
  audit_df <- audit_df[order(audit_df$email), ]

  cat(sprintf("%-45s %-20s %-20s %-20s\n", "Email", "Lib Permissions", "Mailing Lists", "Sharepoint Site"))
  cat(strrep("-", 108), "\n")
  for (i in seq_len(nrow(audit_df))) {
    cat(sprintf("%-45s %-20s %-20s %-20s\n",
                audit_df$email[i],
                ifelse(audit_df$permissions[i], "Y", "-"),
                ifelse(audit_df$mailing[i],     "Y", "-"),
                ifelse(audit_df$site[i],        "Y", "-")
    ))
  }

  invisible(audit_df)
}
