
PESDPermissionsListCSV <- getPESDPermissionsListCSV()
SharepointSiteMembers <- getSharepointSiteMembers()
PESDMailingLists <- getPESDMailingLists("all")

audit <- auditPESDStaffLists(
  permissions   = PESDPermissionsListCSV,
  mailing_lists = PESDMailingLists,
  site_members  = SharepointSiteMembers
)
