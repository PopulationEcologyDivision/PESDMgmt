#' @title makePushButtons
#' @description Generates 6 desktop batch files, one for each grouping category
#' of \code{autoMakeEmailList()}. Double-clicking a batch file will run the
#' function with the corresponding \code{by} parameter and copy the result to
#' the clipboard.
#' @param arch default is \code{NULL}. The R architecture to use - either 32 or
#'   64 are valid entries. If left as NULL, it will use your default
#'   architecture.
#' @family PESDMgmt
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
makePushButtons <- function(arch = NULL) {
  if (Sys.info()["sysname"] == "Linux") {
    stop("No batch files for Linux - Sorry!")
  }

  # Desktop path
  desktopPath <- file.path("C:", "Users", Sys.info()["user"], "Desktop")
  desktopPath <- path.expand(desktopPath)

  # R script location
  RSLoc <- file.path(R.home("bin"), "Rscript.exe")
  if (!is.null(arch)) {
    if (arch == 32) {
      RSLoc <- gsub(pattern = "x64", replacement = "i386", RSLoc)
    } else {
      RSLoc <- gsub(pattern = "i386", replacement = "x64", RSLoc)
    }
  }

  # Create the PESDMgmt folder if needed and write the shared R script
  scriptDir <- file.path("C:", "DFO-MPO", "PESDMgmt")
  dir.create(scriptDir, showWarnings = FALSE)
  runScript <- file.path(scriptDir, "batchRun.R")

  runner <- '
args <- commandArgs(TRUE)
by_val <- args[1]

result <- PESDMgmt::autoMakeEmailList(by = by_val)

# Format to match printed list style: $NAME \\n [1] "emails"
formatted <- paste(
  mapply(function(name, val) {
    paste0("$", name, "\\n[1] \\"", val, "\\"")
  }, names(result), result),
  collapse = "\\n\\n"
)
writeClipboard(formatted)
cat("Done! The", by_val, "email list has been copied to your clipboard, and the results are shown below:\\n")
cat(formatted)
cat("\\n")
'

writeLines(runner, con = runScript)
cat(paste0("R script written to ", runScript, "\n"))

# Write one .bat per grouping category
byValues <- c("all", "location", "lists", "section", "unit", "lead")

batHeader <- "REM PESDMgmt Email List Generator
REM Double-clicking this file will generate the email list and copy it to your clipboard
REM
REM You can regenerate these files via PESDMgmt::makePushButtons()
REM"

for (byVal in byValues) {
  filename <- paste0(byVal,"_emailList", ".bat")
  batFile  <- file(file.path(desktopPath, filename))
  scriptPath <- paste0('"', RSLoc, '" "', runScript, '" ', byVal)
  writeLines(c(batHeader, scriptPath, "PAUSE"), batFile)
  close(batFile)
  cat(paste0("Batch file written to ", file.path(desktopPath, filename), "\n"))
}
}
