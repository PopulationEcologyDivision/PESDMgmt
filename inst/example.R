devtools::load_all()
PESDPermissionsListCSV <- getPESDPermissionsListCSV()

SharepointSiteMembers_PESD <- getSharepointSiteMembers("msteams_74c888")
SharepointSiteMembers_DADDS <- getSharepointSiteMembers("pesddadss")
SharepointSiteMembers_ISAR <- getSharepointSiteMembers("pesdisar")
SharepointSiteMembers_SALMON <- getSharepointSiteMembers("pesdsalmon")
SharepointSiteMembers_GPSS <- getSharepointSiteMembers("pesdgpss")

ml_ALL <- getPESDMailingLists("ALL")
ml_MGMT <- getPESDMailingLists("ADMIN")
ml_DADSS <- getPESDMailingLists("DADSS")
ml_ISAR <- getPESDMailingLists("ISAR")
ml_SALMON <- getPESDMailingLists("SALMON")
ml_GPSS <- getPESDMailingLists("GPSS")

permissions   = getPESDPermissionsListCSV(name="WILSON")
permissions   = getPESDPermissionsListCSV(section="ADMIN")
permissions   = getPESDPermissionsListCSV(unit="TURTLE")

audit_DADSS <- auditPESDStaffLists(maillist = "DADSS", sp_site = "pesddadss", perm_sect = "DADSS")
audit_ISAR <- auditPESDStaffLists(maillist = "ISAR", sp_site = "pesdisar", perm_sect = "ISAR")
audit_SALMON <- auditPESDStaffLists(maillist = "SALMON", sp_site = "pesdsalmon", perm_sect = "SALMON")
audit_GPSS <- auditPESDStaffLists(maillist = "GPSS", sp_site = "pesdgpss", perm_sect = "GPSS")

autoMakeEmailList("DADDS")

audit_pesd <- auditPESDStaffLists(maillist = "ALL", sp_site = "pesddadss")
audit_admin <- auditPESDStaffLists(maillist = "ADMIN", sp_site = "msteams_74c888", perm_sect="ADMIN")
audit_dadss <- auditPESDStaffLists(maillist = "DADSS", sp_site = "pesddadss", perm_sect="DADSS")
audit_isar <- auditPESDStaffLists(maillist = "ISAR", sp_site = "pesdisar", perm_sect="ISAR")
audit_salmon <- auditPESDStaffLists(maillist = "SALMON", sp_site = "pesdsalmon", perm_sect="SALMON")
audit_gpss <- auditPESDStaffLists(maillist = "GPSS", sp_site = "pesdgpss", perm_sect="GPSS")



