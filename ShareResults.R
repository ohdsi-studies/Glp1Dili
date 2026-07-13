################################################################################
# INSTRUCTIONS: This script zips your local Strategus results and uploads them
# to a Google Cloud Storage bucket using a signed upload URL provided by the
# study coordinator.
# 
# PLEASE EMAIL EVELYN GOH AT E0983111@U.NUS.EDU TO REQUEST A SIGNED URL.
#
# You do NOT need a Google account, GCP project access, or any cloud
# credentials of your own to use this script. The study coordinator will
# email you a single, time-limited signed URL scoped only to your site's
# upload path - it cannot be used to view or access any other site's results.
#
# If you don't have a signed URL yet, email the study coordinator to request
# one before running this script.
################################################################################

##=========== START OF INPUTS ==========
outputLocation <- "[local path to your project, e.g. e:/testGlp1Dili]"
databaseName <- "[your site name, used as a folder name, e.g. JMDC]"

# The signed upload URL provided by the study coordinator via email.
# This is a single-use-path URL scoped to your site only.
gcsUploadUrl <- "[signed URL provided by the study coordinator]"
##=========== END OF INPUTS ==========

##################################
# DO NOT MODIFY BELOW THIS POINT
##################################
outputLocation <- file.path(outputLocation, "results", databaseName, "strategusOutput")
zipFile <- file.path(outputLocation, paste0(databaseName, ".zip"))

Strategus::zipResults(
  resultsFolder = outputLocation,
  zipFile = zipFile
)

# Upload the zipped results to the signed GCS URL ------------------------------
if (!requireNamespace("httr", quietly = TRUE)) {
  stop("The 'httr' package is required for this upload step. Install it with: install.packages('httr')")
}

message("Uploading ", zipFile, " to Google Cloud Storage...")

response <- httr::PUT(
  url = gcsUploadUrl,
  body = httr::upload_file(zipFile),
  httr::add_headers("Content-Type" = "application/zip")
)

if (httr::status_code(response) %in% c(200, 201)) {
  message("Upload succeeded. You're done - thank you for contributing your results!")
} else {
  message(
    "Upload may have failed. Status code: ", httr::status_code(response), "\n",
    "Response: ", httr::content(response, as = "text", encoding = "UTF-8"), "\n",
    "If this persists, email the study coordinator - your signed URL may have expired ",
    "(they are time-limited) and a new one may be needed."
  )
}
