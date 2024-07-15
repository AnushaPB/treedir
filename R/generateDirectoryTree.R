generateDirectoryTree <- function(main_directory, output_file = NULL, exclude_pattern = character(0), include_pattern = character(0), fill_in = FALSE) {
  # Function to get description from the user
  getDescription <- function(file_name) {
    if (fill_in) {
      cat("Enter description for", file_name, "(leave empty to skip): ")
      desc <- readline()
      return(ifelse(nchar(desc) > 0, paste0("# ", desc), "#"))
    } else {
      return("#DESCRIPTION HERE")
    }
  }
 
  # Inner recursive function to traverse the directories
  recurseDirs <- function(path, indent = "", is_last = TRUE) {
    contents <- list.files(path, full.names = TRUE)
    is_dir <- file.info(contents)$isdir
    items <- character(0)
   
    for (i in seq_along(contents)) {
      item_name <- basename(contents[i])
     
      # Apply exclude pattern for directories and files
      if (length(exclude_pattern) > 0 && any(vapply(exclude_pattern, function(patt) grepl(patt, item_name), logical(1)))) {
        next
      }
     
      # Apply include pattern for files
      if (!is_dir[i] && length(include_pattern) > 0 && !any(vapply(include_pattern, function(patt) grepl(patt, item_name), logical(1)))) {
        next
      }
     
      # Formatting for files and directories
      prefix <- ifelse(is_dir[i], "[", "")
      suffix <- ifelse(is_dir[i], "]", "")
      branch <- ifelse(is_last && i == length(contents), "└───", "├───")
     
      # Add the formatted line
      items <- c(items, list(c("line" = paste0(indent, branch, prefix, item_name, suffix), "desc" = getDescription(item_name))))
     
      # Recurse into subdirectories
      if (is_dir[i]) {
        sub_indent <- paste0(indent, ifelse(i == length(contents), "    ", "│   "), "    ")
        items <- c(items, recurseDirs(contents[i], sub_indent, i == length(contents)))
      }
    }
    return(items)
  }
 
  # Generate directory tree from the main directory
  all_lines <- recurseDirs(main_directory)
 
  # Calculate padding for alignment
  max_name_length <- max(sapply(all_lines, function(x) nchar(x[1])))
 
  # Generate the aligned text with descriptions
  tree_structure <- sapply(all_lines, function(x) {
    padding <- max_name_length - nchar(x[1]) + 4 # 4 spaces before description
    paste0(x[1], paste(rep(" ", padding), collapse = ""), x[2])
  })
 
  # Write to file
  if(!is.null(output_file)) writeLines(tree_structure, con = output_file)
 
  # Print to terminal
  cat(tree_structure, sep = "\n")
 
  return(tree_structure)
}

# examples
res <- generateDirectoryTree(here("file"), "filetree.txt", include_pattern = c("*.py", "*.R"), exclude_pattern = "html")

res <- generateDirectoryTree(here("file"), include_pattern = c("*.R"), fill_in = TRUE)
