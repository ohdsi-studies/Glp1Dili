
####String replacement from SemaglutideNaion to Strategus
#Load necessary library
# install.packages("stringr")
library(stringr)

#Specify the folder path to work on
folder_path <- getwd() #"your/folder/path"

#Get all files in the folder
file_list <- list.files(path = folder_path, full.names = TRUE, 
                        all.files = FALSE, 
                        recursive = TRUE)

#Perform string replacement for each file
for (file in file_list) {
  # Read the file content
  file_content <- readLines(file, encoding = "UTF-8")  # Adjust file encoding if necessary
  
  # Replace the specified string
  modified_content <- str_replace_all(file_content, "Strategus", "Strategus")
  
  # Check if the content has changed
  if (!identical(file_content, modified_content)) {
    # Save the modified content back to the file
    writeLines(modified_content, file, useBytes = TRUE)
    
    # Print the file name to log the change
    cat("Modified:", file, "\n")
  }
}
print("String replacement is complete in all files.")
