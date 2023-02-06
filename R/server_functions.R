#' Read csv
#'
#' Reads a csv file from a fileInput widget with the column separator specified in a 
#' radioButtons widget.
#'
#' @param file_path a valid path to a csv file
#' @param column_separator column separator character
#'
#' @return A lsit with 2 elements: 
#' dat = the dataframe read from the file and
#' sep = the character separator used to read the file
#'
#' @export
read_csv <- function(file_path, column_separator) {
  
  if (is.null(column_separator)) { # Unspecified file separator
    # Default to comma separator
    fsep <- ","
    df <- utils::read.csv(file_path, sep = fsep)
    
    if (ncol(df) == 1) { # Try tab
      fsep <- "\t"
      df <- utils::read.csv(file_path, sep = fsep)
    }
    if (ncol(df) == 1) { # Try semicolon
      fsep <- ";"
      df <- utils::read.csv(file_path, sep = fsep)
    } 
    if (ncol(df) == 1) { # Other character (choice of custom character to implement)
      df <- utils::read.csv(file_path, sep = fsep)
    }
  } else { # File separator is specified
    fsep <- column_separator
    df <- utils::read.csv(file_path, sep = column_separator)
  }
  
  # Warning
  if (ncol(df) == 1) {
    warning("Only one column detected: check file separator")
  }
  
  return(list(dat = df,
              sep = fsep))
}