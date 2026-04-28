makeEmailList <- function(permissions, mailing_lists, site_members){

  colnames(permissions) <- paste("permiss_", colnames(permissions), sep = "")
  colnames(mailing_lists) <- paste("outlk_", colnames(mailing_lists), sep = "")
  colnames(site_members) <- paste("sp_", colnames(site_members), sep = "")

  mailing_lists <-mailing_lists[,c("outlk_mail", "outlk_displayName", "outlk_LOC", "outlk_MAIL_LIST")]
  permissions <- permissions[,c("permiss_displayName","permiss_email", "permiss_UnitLead", "permiss_SECTION", "permiss_UNIT", "permiss_LOC")]
  site_members <- site_members[,c("sp_displayName", "sp_mail", "sp_LOC")]

  tt<-merge(, , by.x = "outlk_mail", by.y="permiss_email", all=T)
  yy<-merge(tt, , by.x = "outlk_mail", by.y="sp_mail", all=T)

  # > tt[tt$LOC.x != toupper(tt$LOC.y),]
  #
}
