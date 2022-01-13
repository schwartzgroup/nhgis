# nhgis.R - fast and simple access to NHGIS time series data

# TODO: maybe use multipart formulas in the future for variables that change
# across years, e.g. for the female population:
# * tables
#     * 2010: P12
#     * 2000: NP012B
#     * 1990: NP5
#     * 1980: NT10B
# * formula: female ~ P12xx | NP012Bxx | NP5xx

#library(Formula)

library(data.table)
library(progress)
library(rlang)

join_columns <- c("GISJOIN", "YEAR")
default_optional_columns <- c(
  "STATEA", "COUNTYA", "TRACTA", "PLACEA", "BLKGRPA", "BLK_GRPA", "BLOCKA", "ZCTA5A", "ZCTAA", "ZIPA"
)

# primitive functions -----------------------------------------------------

# given a string, return all words separated by variable-length whitespace
str_words <- function(string) {
  return(strsplit(string, "\\s+")[[1]])
}

# given a string, return the last n words
str_last_words <- function(string, n) {
  words <- str_words(string)
  if (nchar(words[1]) == 0) {
    words <- tail(words, -1)
  }
  return(paste(tail(words, n), collapse = " "))
}

# functions for data exploration ------------------------------------------

# given the path of an NHGIS time series data dictionary .txt file, return the
# contents. we could use the `ipumsr` library here but wouldn't be able to
# distinguish between different tables.
#
# format of an NHGIS data dictionary is as follows:
#
# Table {table_number}: ({table_name}) {table_title}
#         {variable_name}:       {variable_description}
#
# e.g.:
#
# Table 2: (AV1) Persons by Sex [2]
#         AV1AA:       Persons: Male
#         AV1AB:       Persons: Female
#read_nhgis_timeseries_codebook <- function(path) {
#  all_tables <- data.frame()
#  all_variables <- data.frame()
#  
#  current_table <- NA
#  encountered_tables <- c()
#  
#  connection <- file(path, "r")
#  
#  # skip forward to the data dictionary
#  while (TRUE) {
#    if (readLines(connection, 1) == "Data Dictionary") {
#      # skip forward one more line
#      readLines(connection, 1)
#      break
#    }
#  }
#  
#  # start parsing out the tables and variables
#  while (TRUE) {
#    line <- readLines(connection, 1)
#    
#    if (length(line) == 0 | startsWith(line, "----------")) {
#      break
#      
#    # table definition
#    } else if (startsWith(line, "Table")) {
#      words <- str_words(line)
#      
#      table_name <- gsub("[\\(\\)]", "", words[3]) # remove parentheses
#      table_description <- paste(words[c(4:length(words))], collapse = " ")
#      
#      # store table info
#      if (! table_name %in% encountered_tables) {
#        all_tables <- rbind(
#          all_tables, data.frame(
#            table = table_name,
#            description = table_description,
#            file = path
#          )
#        )
#      }
#      
#      encountered_tables <- c(encountered_tables, table_name)
#      current_table <- table_name
#      
#    # variable definition, following a table definition
#    } else if (startsWith(line, "       ") & !is.na(current_table)) {
#      words <- str_words(line)
#      
#      variable_name <- gsub(":", "", words[2]) # first word is ""; use the 2nd
#      variable_description <- paste(words[c(3:length(words))], collapse = " ")
#      
#      all_variables <- rbind(
#        all_variables,
#        data.frame(
#          table = current_table,
#          variable = variable_name,
#          description = variable_description
#        )
#      )
#    }
#    
#  }
#  
#  close(connection)
#  
#  return(list(
#    tables = all_tables[order(all_tables$table),],
#    variables = all_variables[order(all_variables$variable),]
#  ))
#}

# similar to `read_nhgis_timeseries_codebook`, but for single-year data sets
read_nhgis_dataset_codebook <- function(path) {
  all_tables <- data.frame()
  all_variables <- data.frame()
  
  connection <- file(path, "r")
  
  while (TRUE) {
    line <- readLines(connection, 1)
    
    if (length(line) == 0) {
      break
      
    # data set year
    } else if (startsWith(line, "Year:")) {
      year <- as.numeric(str_last_words(line, 1))
      
    # data set information
    } else if (startsWith(line, "Dataset:")) {
      ds_source <- str_last_words(line, -1)
      ds_code <- str_last_words(readLines(connection, 1), 1)
      ds_id <- str_last_words(readLines(connection, 1), 1)
      
    # encountered a new table definition
    } else if (grepl("Table [0-9]+", line)) {
      
      # first line: table description
      # example: "    Table 1:     Means of Transportation to Work"
      table_description <- str_last_words(line, -2)
      
      # second line: universe
      # example: "    Universe:    Workers 16 Years and Over"
      # could also be without leading whitespace: "Universe:    Persons"
      universe <- str_last_words(readLines(connection, 1), -1)
      
      # third line: Census table code
      # example: "    Source code: NP030A"
      census_code <- str_last_words(readLines(connection, 1), 1)
      
      # fourth line: NHGIS table code
      # example: "    NHGIS code:  GJ9"
      nhgis_code <- str_last_words(readLines(connection, 1), 1)
      
      # store table info
      all_tables <- rbind(
        all_tables,
        data.frame(
          year = year,
          census_table = census_code,
          nhgis_code = nhgis_code,
          universe = universe,
          description = table_description,
          source = ds_source,
          dataset_code = ds_code,
          dataset_id = ds_id,
          file = path
        )
      )
      
      # now, loop over variables
      # example: "        GKD001:      Less than 5 minutes"
      while (TRUE) {
        line <- readLines(connection, 1)
        
        if (grepl("^\\s*$", line)) {
          break
        }
        
        words <- str_words(line)
        
        first_word <- words[2]
        variable_name <- substr(first_word, 1, nchar(first_word)-1)
        
        variable_description <- paste(tail(words, -2), collapse = " ")
        
        # store variable info
        all_variables <- rbind(
          all_variables,
          data.frame(
            year = year,
            census_table = census_code,
            nhgis_code = nhgis_code,
            variable = variable_name,
            description = variable_description
          )
        )
          
      }
      
    }
    
  }
  
  close(connection)
  
  return(list(
    #tables = all_tables[order(all_tables$census_table),],
    #variables = all_variables[order(all_variables$variable),]
    tables = all_tables[
      with(all_tables, order(year, census_table)),
    ],
    variables = all_variables[
      with(all_variables, order(year, census_table, variable)),
    ]
  ))
}

# read all NHGIS codebooks in the given directory or subdirectories
read_all_nhgis_codebooks <- function(nhgis_dir, geography) {
  all_tables <- data.frame()
  all_variables <- data.frame()
  
  for (relative_path in list.files(nhgis_dir, recursive = TRUE)) {
    codebook_path <- file.path(nhgis_dir, relative_path)
    filename <- basename(codebook_path)
    if (grepl(geography, filename) & (endsWith(filename, ".txt"))) {
      
      if (grepl("ds", filename)) {
        results <- read_nhgis_dataset_codebook(codebook_path)
      } else if (grepl("ts", filename)) {
        #results <- read_nhgis_timeseries_codebook(codebook_path)
        print("time series not supported at this time")
      }
      
      all_tables <- rbind(all_tables, results$tables)
      all_variables <- rbind(all_variables, results$variables)
      
    }
  }
  
  return(list(
    #tables = all_tables[order(all_tables$census_table),],
    #variables = all_variables[order(all_variables$variable),]
    tables = all_tables[
      with(all_tables, order(year, census_table)),
    ],
    variables = all_variables[
      with(all_variables, order(year, census_table, variable)),
    ]
  ))
}

# list storing information about which files have which columns
# we can later query this to see which files contain a given column
list_available_columns <- function(nhgis_dir, geography, year = NA) {
  all_columns <- list()
  for (filename in list.files(nhgis_dir, recursive = TRUE)) {
    if (!endsWith(filename, ".csv") | !grepl(geography, filename)) next
    if (!is.na(year) & !grepl(as.character(year), filename)) next
    
    path = file.path(nhgis_dir, filename)
    
    for (column in names(read.csv(path, nrows = 1))) {
      if (column %in% names(all_columns)) {
        all_columns[[column]] <- c(all_columns[[column]], path)
      } else {
        all_columns[[column]] <- c(path)
      }
    }
  }
  return(all_columns)
}

# given a list of columns, return a list of all unique files containing any of
# the columns
# this allows us to set up a later subset + merge
find_files_with_columns <- function(..., nhgis_dir, geography, year = NA) {
  columns <- list(...)
  all_columns <- list_available_columns(nhgis_dir, geography, year)
  return(unique(c(unlist(sapply(
    columns,
    function(column) {
      all_columns[[column]]
    }
  )))))
}

# functions for data preprocessing ----------------------------------------

# functions for data retrieval --------------------------------------------
# some of these may depend on functions from the previous section in order to
# figure out what _can_ be retrieved.

# given a list of columns, extract those columns from the files containing them
# and return a merged data frame containing only `join_columns` and the selected
# columns
extract_nhgis_columns <- function(...,
                                  nhgis_dir,
                                  geography,
                                  year = NA,
                                  optional_columns = default_optional_columns) {
  columns <- as.character(list(...))
  
  # we always need to include `join_columns` and `optional_columns`
  columns_to_subset <- c(join_columns, optional_columns, columns)
  
  message("determining required files")
  files <- find_files_with_columns(
    ..., nhgis_dir = nhgis_dir, geography = geography, year = year
  )
  
  if (any(sapply(files, is.null))) {
    available_columns <- names(list_available_columns(nhgis_dir, geography))
    errors <- c()
    for (column in columns) {
      if (!column %in% available_columns) {
        errors <- c(errors, column)
      }
    }
    stop(sprintf(
      "columns <%s> do not exist in the raw data",
      paste(errors, collapse = ",")
    ))
  }
  
  message(sprintf("reading %d files", length(files)))
  bar <- progress_bar$new(total = length(files))
  parts <- lapply(
    files,
    function(path) {
      bar$tick()
      data <- fread(path, showProgress = FALSE)
      existing_columns_to_subset <- intersect(names(data), columns_to_subset)
      return(data[, ..existing_columns_to_subset])
    }
  )
  
  # to prevent duplicates in optional columns, we will also join on them
  #all_joinable_columns <- Reduce(
  #  intersect,
  #  lapply(parts, names)
  #)
  
  message(sprintf(
    "merging %d parts on %s",
    #length(parts), paste(all_joinable_columns, collapse = ", ")
    length(parts), paste(join_columns, collapse = ", ")
  ))
  bar <- progress_bar$new(total = length(parts) - 1)
  return(Reduce(
    function(left, right) {
      bar$tick()
      
      merged <- merge(
        # some column types will change randomly so we will convert them all to
        # character vectors to facilitate the joins
        #
        # TODO: current method could be improved by first checking if the
        # columns are already characters; may not be an issue due to the
        # relatively small size of the data but look into later
        left[,
             (join_columns) := lapply(.SD, as.character),
             .SDcols = join_columns],
        right[,
              (join_columns) := lapply(.SD, as.character),
              .SDcols = join_columns],
        by = join_columns,
        suffixes = c(".left", ".right"),
        all = TRUE
      )
      
      # coalesce duplicated columns
      duplicates_with_suffix <- Filter(
        function(column_name) {
          endsWith(column_name, ".left")
        },
        names(merged)
      )
      for (left_column in duplicates_with_suffix) {
        base_name <- gsub(".left$", "", left_column)
        right_column <- gsub(".left$", ".right", left_column)
        
        # coalesce
        merged[[base_name]] <- fcoalesce(
          merged[[left_column]], merged[[right_column]]
        )
        
        # remove constituents
        merged[,c(left_column, right_column) := NULL]
      }
      
      return(merged)
    },
    parts
  ))
}

# wrapper around `extract_nhgis_columns` that uses characters, names, and formulas to
# query raw data and compute new variables
query_nhgis <- function(...,
                        nhgis_dir,
                        geography,
                        year = NA,
                        optional_columns = default_optional_columns) {
  # parse columns - convert everything into either a name or a formula
  columns <- lapply(
    eval(substitute(alist(...))),
    function(column) {
      if (class(column) == "character") {
        return(as.name(column))
      } else if (class(column) == "name") {
        return(column)
      } else if (class(column) == "call") {
        return(eval(column)) # this is for formulas
      }
    }
  )
  
  # extract the column names:
  # * for a name: the name as a character
  # * for a formula: the left-hand-side
  column_names <- unlist(lapply(
    columns,
    function(column) {
      if (class(column) == "name") {
        return(as.character(column))
      } else {
        return(as.character(f_lhs(column)))
      }
    }
  ))
  
  # figure out what columns are needed:
  # * for a name: the name as a character
  # * for a formula: a character vector of variables in the right-hand side
  #     * will be collapsed at the end
  needed_columns <- unlist(lapply(
    columns,
    function(column) {
      if (class(column) == "name") {
        return(as.character(column))
      } else {
        return(all.vars(f_rhs(column)))
      }
    }
  ))
  
  # get the unprocessed data
  data <- do.call(
    extract_nhgis_columns,
    c(
      needed_columns,
      list(
        nhgis_dir = nhgis_dir,
        geography = geography,
        year = year,
        optional_columns = optional_columns
      )
    )
  )
  
  # see which of `optional_columns` actually existed in the data
  existing_optional_columns <- intersect(optional_columns, names(data))
  
  # compute columns
  message("computing columns")
  bar <- progress_bar$new(total = length(columns))
  computed_columns <- lapply(
    # also add the `join_columns` and `optional_columns`
    c(
      lapply(join_columns, as.name),
      lapply(existing_optional_columns, as.name),
      columns
    ),
    function(column) {
      if (class(column) == "name") {
        with(data, eval(column))
      } else {
        with(data, eval(f_rhs(column)))
      }
    }
  )
  
  # build the final data frame
  message("binding columns")
  names(computed_columns) <- c(join_columns, existing_optional_columns, column_names)
  full_data <- cbind.data.frame(computed_columns)
  
  # return subset of only columns without all NAs
  return(full_data[colSums(!is.na(full_data)) > 0])
}

