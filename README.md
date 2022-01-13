# nhgis

`nhgis.R` is a small R library to help with processing and working with data sets from NHGIS. When working with very large amounts of data, it can often be very cumbersome to work with NHGIS data due to the way that the codebooks and raw data are separated. This library aims to address this issue by abstracting away the main issues with data exploration, extraction, and transformation and providing an easy-to-use interface.

# Simple example

No additional setup is required beyond sourcing the file. However, we will want to define some variables that we will be using in every function call: where the data is stored, and what geographic level we are working with.

```r
source("nhgis.R")
nhgis_dir = "raw_data"
geography = "tract"
```

### Data exploration

To view what data is available, we can use the `read_all_nhgis_codebooks` function, which will scan a directory for all data set codebooks matching the specified geography:

```r
available_data <- read_all_nhgis_codebooks(nhgis_dir, geography)
```

This creates a list with two members: `tables`, describing what tables are available, and `variables`, describing what variables are available. Let's take a look at what we have:

```r
head(tibble::as_tibble(available_data$tables), 3)
```

```r
head(tibble::as_tibble(available_data$variables), 3)
```

Note that these two tables have the common columns `year` and `census_table`. It can often be easier to find what you're looking for by joining these tables and then passing the result into `View`, or exporting it to a CSV file where you can view the codebook in a spreadsheet program.

```r
View(merge(
  available_data$variables, available_data$tables,
  by = c("year", "census_table")
))
```

### Simple data extraction

The `extract_nhgis_columns` function allows for simple extraction of raw data. The script uses the same functions that `read_all_nhgis_codebooks` uses to automatically detect which files to read and then reads them.

Let's say I want to know the number of workers, age 16 or older, in the year 2000, who used public transit. From inspecting `available_data`, we see that this variable is `GJ9002`, so we can do the following:

```r
result <- extract_nhgis_columns(
  "GJ9002",
  nhgis_dir = nhgis_dir,
  geography = geography
)
tail(result, 3)
```

What if I also want to know how many people lived in the area at that time? That variable is `FL5001`, so we can just stick it into the function:

```r
result <- extract_nhgis_columns(
  "GJ9002", "FL5001",
  nhgis_dir = nhgis_dir,
  geography = geography
)
tail(result, 3)
```

### Complex data extraction and transformation

For more complicated data extractions, or data extractions mixed with transformations - i.e. renaming and calculations - the `query_nhgis` function exists. This is a wrapper on top of `extract_nhgis_columns` that provides a formula interface for column extraction and transformation. In `query_nhgis`, columns can be specified in 3 different ways:

* As names, e.g. `FL5001`,
* As characters, e.g. `"FL5001"`, and
* **As formulas**, e.g. `pop_2000 ~ FL5001` or `pct_transit ~ GJ9002 / FL5001`.

The formula interface works by evaluating the formula in the environment of the data frame, similar to how `dplyr`'s `mutate` function works.  This means that any expression that operates on vectors is valid. The formula interface is particularly useful for doing one-pass extractions and transformations while also supplying meaningful names for future reference.

Let's see this in an example:

```r
result <- query_nhgis(
  # variable name input
  FL5001,
  
  # string input
  "GJ9002",
  
  # formula input
  pop_2000 ~ FL5001,
  pct_transit ~ GJ9002 / FL5001,
  # percent of workers who were transit users in 2000
  pct_transit_2000 ~ GJ9002 / FL5001,
  
  # the sine of e^(percent of transit user) (???)
  sin_exp_pct_transit ~ sin(1 / exp(GJ9002 / FL5001)),
  
  # indicator variable for > 10 transit users
  if_gt_10_people ~ ifelse(GJ9002 > 10, 1, 0),
  
  # contained houses that did not use fuel for heating
  # i.e. if any houses that used "Electricity" or "No fuel used"
  no_heating_fumes ~ (CTT003 > 0) | (CTT008 > 0),
  
  nhgis_dir = nhgis_dir, geography = geography
)
tail(result, 3)
```

One caveat to be aware of is that this uses R's vanilla `with()` operator, so things like referencing external variables are not supported. This can be worked around by building custom functions, though for more complicated things it may be best to defer to processing the exported data with other packages:

```
add_pi <- function(x) {
  return(x + pi)
}

result <- query_nhgis(
  pop_2000_plus_pi ~ add_pi(FL5001),
  nhgis_dir = nhgis_dir,
  geography = geography
)
tail(result, 3)
```
