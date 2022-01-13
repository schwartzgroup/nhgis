# Extract data from NHGIS using `nhgis.R`

source("nhgis.R")

nhgis_dir = "raw_data"

export_wrapper <- function(geoid_functions, query_args) {
  lapply(
    names(geoid_functions),
    function(geography) {
      message(sprintf("========== %s", geography))
      
      data <- do.call(
        query_nhgis,
        c(
          list(
            geoid_functions[[geography]],
            geography = geography
          ),
          query_args
        )
      )
      
      output_file <- sprintf("output/%s_%s.csv.gz", query_args[["year"]], geography)
      message(sprintf("writing to %s", output_file))
      
      write.csv(data, gzfile(output_file), row.names = FALSE)
    }
  )
}

# dump codebooks ----------------------------------------------------------

for (geography in c("block", "blck_grp", "tract", "county", "zcta", "zip")) {
  codebook <- read_all_nhgis_codebooks(nhgis_dir, geography)
  base_name <- sprintf("codebook_tables/2021-04-02-%s-", geography)
  write.csv(codebook$tables, paste0(base_name, "tables.csv"))
  write.csv(codebook$variables, paste0(base_name, "variables.csv"))
}

# setup / data exploration ------------------------------------------------

geography = "zcta"
geography = "county"
geography = "tract"
geography = "blck_grp"
geography = "block"

codebooks <- read_all_nhgis_codebooks(nhgis_dir, geography)

if (FALSE) {
  View(subset(
    read_all_nhgis_codebooks(nhgis_dir, "blck_grp")$variable,
    year == 1990
  ))
  View(subset(
    read_all_nhgis_codebooks(nhgis_dir, "tract")$variable,
    year == 1990
  ))
  View(subset(
    read_all_nhgis_codebooks(nhgis_dir, "county")$variable,
    year == 1990
  ))
  View(subset(
    read_all_nhgis_codebooks(nhgis_dir, "zcta")$variable,
    year == 2000
  ))
  write.csv(
    merge(
      codebooks$tables,
      subset(codebooks$variables, year == 1980), # change the year
      by = c("nhgis_code"),
      all = TRUE
    ),
    "temp.csv"
  )
  View(merge(
    codebooks$tables,
    subset(codebooks$variables, year == 1990), # change the year
    by = c("nhgis_code"),
    all.y = TRUE
  ))
}

# 2010 (all) -------------------------------------------------------------
# reduced variable set for 2010; the rest is in ACS

export_wrapper(
  
  geoid_functions = list(
    zcta = GEOID ~ sprintf("%05d", as.numeric(ZCTA5A)),
    county = GEOID ~ sprintf("%02d%03d", as.numeric(STATEA), as.numeric(COUNTYA)),
    tract = GEOID ~ sprintf("%02d%03d%06d", as.numeric(STATEA), as.numeric(COUNTYA), as.numeric(TRACTA)),
    blck_grp = GEOID ~ sprintf("%02d%03d%06d%01d", as.numeric(STATEA), as.numeric(COUNTYA), as.numeric(TRACTA), as.numeric(BLKGRPA)),
    block = GEOID ~ sprintf("%02d%03d%06d%04d", as.numeric(STATEA), as.numeric(COUNTYA), as.numeric(TRACTA), as.numeric(BLOCKA))
  ),
  
  query_args = list(
    
    # Universe: Total population
    population ~ H76001,
    pct_female ~ H76026 / H76001,
    pct_age_under_5 ~ (H76003 + H76027) / H76001,
    pct_age_5_to_9 ~ (H76004 + H76028) / H76001,
    pct_age_10_to_14 ~ (H76005 + H76029) / H76001,
    pct_age_15_to_17 ~ (H76006 + H76030) / H76001,
    pct_age_18_to_19 ~ (H76007 + H76031) / H76001,
    pct_age_20 ~ (H76008 + H76032) / H76001,
    pct_age_21 ~ (H76009 + H76033) / H76001,
    pct_age_22_to_24 ~ (H76010 + H76034) / H76001,
    pct_age_25_to_29 ~ (H76011 + H76035) / H76001,
    pct_age_30_to_34 ~ (H76012 + H76036) / H76001,
    pct_age_35_to_39 ~ (H76013 + H76037) / H76001,
    pct_age_40_to_44 ~ (H76014 + H76038) / H76001,
    pct_age_45_to_49 ~ (H76015 + H76039) / H76001,
    pct_age_50_to_54 ~ (H76016 + H76040) / H76001,
    pct_age_55_to_59 ~ (H76017 + H76041) / H76001,
    pct_age_60_to_61 ~ (H76018 + H76042) / H76001,
    pct_age_62_to_64 ~ (H76019 + H76043) / H76001,
    pct_age_65_to_66 ~ (H76020 + H76044) / H76001,
    pct_age_67_to_69 ~ (H76021 + H76045) / H76001,
    pct_age_65_to_69 ~ (H76020 + H76044 + H76021 + H76045) / H76001,
    pct_age_70_to_74 ~ (H76022 + H76046) / H76001,
    pct_age_75_to_79 ~ (H76023 + H76047) / H76001,
    pct_age_80_to_84 ~ (H76024 + H76048) / H76001,
    pct_age_over_85 ~ (H76025 + H76049) / H76001,
    pct_white ~ H7X002 / H76001,
    pct_non_hispanic_white ~ H7Z003 / H76001,
    pct_hispanic_white ~ H7Z011 / H76001,
    pct_black ~ H7X003 / H76001,
    pct_non_hispanic_black ~ H7Z004 / H76001,
    pct_hispanic_black ~ H7Z012 / H76001,
    pct_native ~ H7X004 / H76001,
    pct_non_hispanic_native ~ H7Z005 / H76001,
    pct_hispanic_native ~ H7Z013 / H76001,
    pct_asian ~ H7X005 / H76001,
    pct_non_hispanic_asian ~ H7Z006 / H76001,
    pct_hispanic_asian ~ H7Z014 / H76001,
    pct_two_or_more_races ~ H7X008 / H76001,
    pct_non_hispanic_two_or_more_races ~ H7Z009 / H76001,
    pct_hispanic_two_or_more_races ~ H7Z017 / H76001,
    pct_asian_pacific_islander ~ (H7X005 + H7X006) / H76001,
    pct_non_hispanic_asian_pacific_islander ~ (H7Z006 + H7Z007) / H76001,
    pct_hispanic_asian_pacific_islander ~ (H7Z014 + H7Z015) / H76001,
    pct_hispanic ~ H7Z010 / H76001,
    
    # Universe: Households
    n_households ~ H8C001,
    pct_households_single_father ~ H8C005 / H8C001,
    pct_households_single_mother ~ H8C006 / H8C001,
    
    # Universe: Housing Units
    n_housing_units ~ IFC001,
    
    # Universe: Occupied Housing Units
    n_occupied_housing_units ~ IFF001,
    pct_renting ~ IFF004 / IFF001,
    
    nhgis_dir = nhgis_dir, year = 2010
  )
  
)

# 2000 (larger than block groups) ------------------------------------

# BLS CPI calculator January 2000 -> January 2010
inflation_adjustment_2000 <- 1.28369

export_wrapper(
  
  geoid_functions = list(
    zcta = GEOID ~ sprintf("%05d", as.numeric(ZCTAA)),
    county = GEOID ~ sprintf("%02d%03d", as.numeric(STATEA), as.numeric(COUNTYA)),
    tract = GEOID ~ sprintf("%02d%03d%06d", as.numeric(STATEA), as.numeric(COUNTYA), as.numeric(TRACTA))
  ),
  
  query_args = list(
    
    # Universe: Total population
    population ~ FL5001,
    pct_female ~ (
      FMZ024 + FMZ025 + FMZ026 + FMZ027 + FMZ028 + FMZ029 + FMZ030 + FMZ031 +
      FMZ032 + FMZ033 + FMZ034 + FMZ035 + FMZ036 + FMZ037 + FMZ038 + FMZ039 +
      FMZ040 + FMZ041 + FMZ042 + FMZ043 + FMZ044 + FMZ045 + FMZ046
    ) / FL5001,
    pct_age_under_5 ~ (FMZ001 + FMZ024) / FL5001,
    pct_age_5_to_9 ~ (FMZ002 + FMZ025) / FL5001,
    pct_age_10_to_14 ~ (FMZ003 + FMZ026) / FL5001,
    pct_age_15_to_17 ~ (FMZ004 + FMZ027) / FL5001,
    pct_age_18_to_19 ~ (FMZ005 + FMZ028) / FL5001,
    pct_age_20 ~ (FMZ006 + FMZ029) / FL5001,
    pct_age_21 ~ (FMZ007 + FMZ030) / FL5001,
    pct_age_22_to_24 ~ (FMZ008 + FMZ031) / FL5001,
    pct_age_25_to_29 ~ (FMZ009 + FMZ032) / FL5001,
    pct_age_30_to_34 ~ (FMZ010 + FMZ033) / FL5001,
    pct_age_35_to_39 ~ (FMZ011 + FMZ034) / FL5001,
    pct_age_40_to_44 ~ (FMZ012 + FMZ035) / FL5001,
    pct_age_45_to_49 ~ (FMZ013 + FMZ036) / FL5001,
    pct_age_50_to_54 ~ (FMZ014 + FMZ037) / FL5001,
    pct_age_55_to_59 ~ (FMZ015 + FMZ038) / FL5001,
    pct_age_60_to_61 ~ (FMZ016 + FMZ039) / FL5001,
    pct_age_62_to_64 ~ (FMZ017 + FMZ040) / FL5001,
    pct_age_65_to_66 ~ (FMZ018 + FMZ041) / FL5001,
    pct_age_67_to_69 ~ (FMZ019 + FMZ042) / FL5001,
    pct_age_65_to_69 ~ (FMZ018 + FMZ041 + FMZ019 + FMZ042) / FL5001,
    pct_age_70_to_74 ~ (FMZ020 + FMZ043) / FL5001,
    pct_age_75_to_79 ~ (FMZ021 + FMZ044) / FL5001,
    pct_age_80_to_84 ~ (FMZ022 + FMZ045) / FL5001,
    pct_age_over_85 ~ (FMZ023 + FMZ046) / FL5001,
    pct_white ~ FMR001 / FL5001,
    pct_non_hispanic_white ~ FMS001 / FL5001,
    pct_hispanic_white ~ FMS008 / FL5001,
    pct_black ~ FMR002 / FL5001,
    pct_non_hispanic_black ~ FMS002 / FL5001,
    pct_hispanic_black ~ FMS009 / FL5001,
    pct_native ~ FMR003 / FL5001,
    pct_non_hispanic_native ~ FMS004 / FL5001,
    pct_hispanic_native ~ FMS011 / FL5001,
    pct_asian ~ FMR004 / FL5001,
    pct_non_hispanic_asian ~ FMS003 / FL5001,
    pct_hispanic_asian ~ FMS010 / FL5001,
    pct_two_or_more_races ~ FMR007 / FL5001,
    pct_non_hispanic_two_or_more_races ~ FMS007 / FL5001,
    pct_hispanic_two_or_more_races ~ FMS014 / FL5001,
    pct_asian_pacific_islander ~ (FMR004 + FMR005) / FL5001,
    pct_non_hispanic_asian_pacific_islander ~ (FMS004 + FMS005) / FL5001,
    pct_hispanic_asian_pacific_islander ~ (FMS011 + FMS012) / FL5001,
    pct_hispanic ~ FMS008 / FL5001,
   
    # Universe: Workers 16 Years and Over
    pct_transport_auto ~ GJ9001 / (
      GJ9001 + GJ9002 + GJ9003 + GJ9004 + GJ9005 + GJ9006 + GJ9007
    ),
    pct_transport_public_transit ~ GJ9002 / (
      GJ9001 + GJ9002 + GJ9003 + GJ9004 + GJ9005 + GJ9006 + GJ9007
    ),
    pct_transport_motorcycle ~ GJ9003 / (
      GJ9001 + GJ9002 + GJ9003 + GJ9004 + GJ9005 + GJ9006 + GJ9007
    ),
    pct_transport_bicycle ~ GJ9004 / (
      GJ9001 + GJ9002 + GJ9003 + GJ9004 + GJ9005 + GJ9006 + GJ9007
    ),
    pct_transport_walk ~ GJ9005 / (
      GJ9001 + GJ9002 + GJ9003 + GJ9004 + GJ9005 + GJ9006 + GJ9007
    ),
    pct_transport_other ~ GJ9006 / (
      GJ9001 + GJ9002 + GJ9003 + GJ9004 + GJ9005 + GJ9006 + GJ9007
    ),
    pct_transport_wfh ~ GJ9007 / (
      GJ9001 + GJ9002 + GJ9003 + GJ9004 + GJ9005 + GJ9006 + GJ9007
    ),
   
    # Universe: Workers 16 Years and Over Who Did Not Work at Home
    pct_travel_lt_5_min ~ GKD001 / (
      GKD001 + GKD002 + GKD003 + GKD004 + GKD005 + GKD006 + GKD007 + GKD008 +
      GKD009 + GKD010 + GKD011 + GKD012
    ),
    pct_travel_5_to_9_min ~ GKD002 / (
      GKD001 + GKD002 + GKD003 + GKD004 + GKD005 + GKD006 + GKD007 + GKD008 +
      GKD009 + GKD010 + GKD011 + GKD012
    ),
    pct_travel_10_to_14_min ~ GKD003 / (
      GKD001 + GKD002 + GKD003 + GKD004 + GKD005 + GKD006 + GKD007 + GKD008 +
      GKD009 + GKD010 + GKD011 + GKD012
    ),
    pct_travel_15_to_19_min ~ GKD004 / (
      GKD001 + GKD002 + GKD003 + GKD004 + GKD005 + GKD006 + GKD007 + GKD008 +
      GKD009 + GKD010 + GKD011 + GKD012
    ),
    pct_travel_20_to_24_min ~ GKD005 / (
      GKD001 + GKD002 + GKD003 + GKD004 + GKD005 + GKD006 + GKD007 + GKD008 +
      GKD009 + GKD010 + GKD011 + GKD012
    ),
    pct_travel_25_to_29_min ~ GKD006 / (
      GKD001 + GKD002 + GKD003 + GKD004 + GKD005 + GKD006 + GKD007 + GKD008 +
      GKD009 + GKD010 + GKD011 + GKD012
    ),
    pct_travel_30_to_34_min ~ GKD007 / (
      GKD001 + GKD002 + GKD003 + GKD004 + GKD005 + GKD006 + GKD007 + GKD008 +
      GKD009 + GKD010 + GKD011 + GKD012
    ),
    pct_travel_35_to_39_min ~ GKD008 / (
      GKD001 + GKD002 + GKD003 + GKD004 + GKD005 + GKD006 + GKD007 + GKD008 +
      GKD009 + GKD010 + GKD011 + GKD012
    ),
    pct_travel_40_to_44_min ~ GKD009 / (
      GKD001 + GKD002 + GKD003 + GKD004 + GKD005 + GKD006 + GKD007 + GKD008 +
      GKD009 + GKD010 + GKD011 + GKD012
    ),
    pct_travel_45_to_59_min ~ GKD010 / (
      GKD001 + GKD002 + GKD003 + GKD004 + GKD005 + GKD006 + GKD007 + GKD008 +
      GKD009 + GKD010 + GKD011 + GKD012
    ),
    pct_travel_60_to_89_min ~ GKD011 / (
      GKD001 + GKD002 + GKD003 + GKD004 + GKD005 + GKD006 + GKD007 + GKD008 +
      GKD009 + GKD010 + GKD011 + GKD012
    ),
    pct_travel_gt_90_min ~ GKD012 / (
      GKD001 + GKD002 + GKD003 + GKD004 + GKD005 + GKD006 + GKD007 + GKD008 +
      GKD009 + GKD010 + GKD011 + GKD012
    ),
    
    # Universe: Persons 18 Years and Over
    pct_edu_lt_9th_grade ~ (
      GW5001 + GW5008 + GW5015 + GW5022 + GW5029 + GW5036 + GW5043 + GW5050 +
      GW5057 + GW5064
    ) / (
      GW5001 + GW5002 + GW5003 + GW5004 + GW5005 + GW5006 + GW5007 + GW5008 +
      GW5009 + GW5010 + GW5011 + GW5012 + GW5013 + GW5014 + GW5015 + GW5016 +
      GW5017 + GW5018 + GW5019 + GW5020 + GW5021 + GW5022 + GW5023 + GW5024 +
      GW5025 + GW5026 + GW5027 + GW5028 + GW5029 + GW5030 + GW5031 + GW5032 +
      GW5033 + GW5034 + GW5035 + GW5036 + GW5037 + GW5038 + GW5039 + GW5040 +
      GW5041 + GW5042 + GW5043 + GW5044 + GW5045 + GW5046 + GW5047 + GW5048 +
      GW5049 + GW5050 + GW5051 + GW5052 + GW5053 + GW5054 + GW5055 + GW5056 +
      GW5057 + GW5058 + GW5059 + GW5060 + GW5061 + GW5062 + GW5063 + GW5064 +
      GW5065 + GW5066 + GW5067 + GW5068 + GW5069 + GW5070
    ),
    pct_edu_9th_to_12th_grade ~ (
      GW5002 + GW5009 + GW5016 + GW5023 + GW5030 + GW5037 + GW5044 + GW5051 +
      GW5058 + GW5065
    ) / (
      GW5001 + GW5002 + GW5003 + GW5004 + GW5005 + GW5006 + GW5007 + GW5008 +
      GW5009 + GW5010 + GW5011 + GW5012 + GW5013 + GW5014 + GW5015 + GW5016 +
      GW5017 + GW5018 + GW5019 + GW5020 + GW5021 + GW5022 + GW5023 + GW5024 +
      GW5025 + GW5026 + GW5027 + GW5028 + GW5029 + GW5030 + GW5031 + GW5032 +
      GW5033 + GW5034 + GW5035 + GW5036 + GW5037 + GW5038 + GW5039 + GW5040 +
      GW5041 + GW5042 + GW5043 + GW5044 + GW5045 + GW5046 + GW5047 + GW5048 +
      GW5049 + GW5050 + GW5051 + GW5052 + GW5053 + GW5054 + GW5055 + GW5056 +
      GW5057 + GW5058 + GW5059 + GW5060 + GW5061 + GW5062 + GW5063 + GW5064 +
      GW5065 + GW5066 + GW5067 + GW5068 + GW5069 + GW5070
    ),
    pct_edu_high_school ~ (
      GW5003 + GW5010 + GW5017 + GW5024 + GW5031 + GW5038 + GW5045 + GW5052 +
      GW5059 + GW5066
    ) / (
      GW5001 + GW5002 + GW5003 + GW5004 + GW5005 + GW5006 + GW5007 + GW5008 +
      GW5009 + GW5010 + GW5011 + GW5012 + GW5013 + GW5014 + GW5015 + GW5016 +
      GW5017 + GW5018 + GW5019 + GW5020 + GW5021 + GW5022 + GW5023 + GW5024 +
      GW5025 + GW5026 + GW5027 + GW5028 + GW5029 + GW5030 + GW5031 + GW5032 +
      GW5033 + GW5034 + GW5035 + GW5036 + GW5037 + GW5038 + GW5039 + GW5040 +
      GW5041 + GW5042 + GW5043 + GW5044 + GW5045 + GW5046 + GW5047 + GW5048 +
      GW5049 + GW5050 + GW5051 + GW5052 + GW5053 + GW5054 + GW5055 + GW5056 +
      GW5057 + GW5058 + GW5059 + GW5060 + GW5061 + GW5062 + GW5063 + GW5064 +
      GW5065 + GW5066 + GW5067 + GW5068 + GW5069 + GW5070
    ),
    pct_edu_some_college ~ (
      GW5004 + GW5011 + GW5018 + GW5025 + GW5032 + GW5039 + GW5046 + GW5053 +
      GW5060 + GW5067
    ) / (
      GW5001 + GW5002 + GW5003 + GW5004 + GW5005 + GW5006 + GW5007 + GW5008 +
      GW5009 + GW5010 + GW5011 + GW5012 + GW5013 + GW5014 + GW5015 + GW5016 +
      GW5017 + GW5018 + GW5019 + GW5020 + GW5021 + GW5022 + GW5023 + GW5024 +
      GW5025 + GW5026 + GW5027 + GW5028 + GW5029 + GW5030 + GW5031 + GW5032 +
      GW5033 + GW5034 + GW5035 + GW5036 + GW5037 + GW5038 + GW5039 + GW5040 +
      GW5041 + GW5042 + GW5043 + GW5044 + GW5045 + GW5046 + GW5047 + GW5048 +
      GW5049 + GW5050 + GW5051 + GW5052 + GW5053 + GW5054 + GW5055 + GW5056 +
      GW5057 + GW5058 + GW5059 + GW5060 + GW5061 + GW5062 + GW5063 + GW5064 +
      GW5065 + GW5066 + GW5067 + GW5068 + GW5069 + GW5070
    ),
    pct_edu_associate ~ (
      GW5005 + GW5012 + GW5019 + GW5026 + GW5033 + GW5040 + GW5047 + GW5054 +
      GW5061 + GW5068
    ) / (
      GW5001 + GW5002 + GW5003 + GW5004 + GW5005 + GW5006 + GW5007 + GW5008 +
      GW5009 + GW5010 + GW5011 + GW5012 + GW5013 + GW5014 + GW5015 + GW5016 +
      GW5017 + GW5018 + GW5019 + GW5020 + GW5021 + GW5022 + GW5023 + GW5024 +
      GW5025 + GW5026 + GW5027 + GW5028 + GW5029 + GW5030 + GW5031 + GW5032 +
      GW5033 + GW5034 + GW5035 + GW5036 + GW5037 + GW5038 + GW5039 + GW5040 +
      GW5041 + GW5042 + GW5043 + GW5044 + GW5045 + GW5046 + GW5047 + GW5048 +
      GW5049 + GW5050 + GW5051 + GW5052 + GW5053 + GW5054 + GW5055 + GW5056 +
      GW5057 + GW5058 + GW5059 + GW5060 + GW5061 + GW5062 + GW5063 + GW5064 +
      GW5065 + GW5066 + GW5067 + GW5068 + GW5069 + GW5070
    ),
    pct_edu_bachelors ~ (
      GW5006 + GW5013 + GW5020 + GW5027 + GW5034 + GW5041 + GW5048 + GW5055 +
      GW5062 + GW5069
    ) / (
      GW5001 + GW5002 + GW5003 + GW5004 + GW5005 + GW5006 + GW5007 + GW5008 +
      GW5009 + GW5010 + GW5011 + GW5012 + GW5013 + GW5014 + GW5015 + GW5016 +
      GW5017 + GW5018 + GW5019 + GW5020 + GW5021 + GW5022 + GW5023 + GW5024 +
      GW5025 + GW5026 + GW5027 + GW5028 + GW5029 + GW5030 + GW5031 + GW5032 +
      GW5033 + GW5034 + GW5035 + GW5036 + GW5037 + GW5038 + GW5039 + GW5040 +
      GW5041 + GW5042 + GW5043 + GW5044 + GW5045 + GW5046 + GW5047 + GW5048 +
      GW5049 + GW5050 + GW5051 + GW5052 + GW5053 + GW5054 + GW5055 + GW5056 +
      GW5057 + GW5058 + GW5059 + GW5060 + GW5061 + GW5062 + GW5063 + GW5064 +
      GW5065 + GW5066 + GW5067 + GW5068 + GW5069 + GW5070
    ),
    pct_edu_graduate_or_professional ~ (
      GW5007 + GW5014 + GW5021 + GW5028 + GW5035 + GW5042 + GW5049 + GW5056 +
      GW5063 + GW5070
    ) / (
      GW5001 + GW5002 + GW5003 + GW5004 + GW5005 + GW5006 + GW5007 + GW5008 +
      GW5009 + GW5010 + GW5011 + GW5012 + GW5013 + GW5014 + GW5015 + GW5016 +
      GW5017 + GW5018 + GW5019 + GW5020 + GW5021 + GW5022 + GW5023 + GW5024 +
      GW5025 + GW5026 + GW5027 + GW5028 + GW5029 + GW5030 + GW5031 + GW5032 +
      GW5033 + GW5034 + GW5035 + GW5036 + GW5037 + GW5038 + GW5039 + GW5040 +
      GW5041 + GW5042 + GW5043 + GW5044 + GW5045 + GW5046 + GW5047 + GW5048 +
      GW5049 + GW5050 + GW5051 + GW5052 + GW5053 + GW5054 + GW5055 + GW5056 +
      GW5057 + GW5058 + GW5059 + GW5060 + GW5061 + GW5062 + GW5063 + GW5064 +
      GW5065 + GW5066 + GW5067 + GW5068 + GW5069 + GW5070
    ),
    
    # Universe: Persons for Whom Poverty Status Is Determined
    pct_poverty ~ GN6001 / (GN6001 + GN6002),
    
    # Universe: Households
    n_households ~ FNH001,
    mean_household_size ~ FNP001,
    pct_income_lt_10k ~ GMX001 / FNH001,
    pct_income_10k_to_15k ~ GMX002 / FNH001,
    pct_income_15k_to_20k ~ GMX003 / FNH001,
    pct_income_20k_to_25k ~ GMX004 / FNH001,
    pct_income_25k_to_30k ~ GMX005 / FNH001,
    pct_income_30k_to_35k ~ GMX006 / FNH001,
    pct_income_35k_to_40k ~ GMX007 / FNH001,
    pct_income_40k_to_45k ~ GMX008 / FNH001,
    pct_income_45k_to_50k ~ GMX009 / FNH001,
    pct_income_50k_to_60k ~ GMX010 / FNH001,
    pct_income_60k_to_75k ~ GMX011 / FNH001,
    pct_income_75k_to_100k ~ GMX012 / FNH001,
    pct_income_100k_to_125k ~ GMX013 / FNH001,
    pct_income_125k_to_150k ~ GMX014 / FNH001,
    pct_income_150k_to_200k ~ GMX015 / FNH001,
    pct_income_gt_150k ~ (GMX015 + GMX016) / FNH001,
    pct_income_gt_200k ~ GMX016 / FNH001,
    pct_public_assistance ~ GNB001 / FNH001,
    
    # Universe: Civilian Persons 16 Years and Over in Labor Force
    pct_eligible_unemployed ~ (GLR002 + GLR004) / (GLR001 + GLR002 + GLR003 + GLR004),
    
    # Universe: Housing Units
    n_housing_units ~ FKI001,
    pct_housing_standalone ~ GAF001 / FKI001,
    pct_housing_1_unit ~ (GAF001 + GAF002) / FKI001,
    pct_housing_2_units ~ GAF003 / FKI001,
    pct_housing_3_to_4_units ~ GAF004 / FKI001,
    pct_housing_5_to_9_units ~ GAF005 / FKI001,
    pct_housing_10_to_19_units ~ GAF006 / FKI001,
    pct_housing_20_to_49_units ~ GAF007 / FKI001,
    pct_housing_gt_50_units ~ GAF008 / FKI001,
    pct_housing_mobile_home ~ GAF009 / FKI001,
    pct_housing_vehicle ~ GAF010 / FKI001,
    
    # Universe: Occupied Housing Units
    n_occupied_housing_units ~ FKN001 + FKN002,
    pct_renting ~ FKN002 / (FKN001 + FKN002),
    pct_heating_utility_gas ~ GAR001 / (FKN001 + FKN002),
    pct_heating_bottled_gas ~ GAR002 / (FKN001 + FKN002),
    pct_heating_electricity ~ GAR003 / (FKN001 + FKN002),
    pct_heating_oil ~ GAR004 / (FKN001 + FKN002),
    pct_heating_coal ~ GAR005 / (FKN001 + FKN002),
    pct_heating_wood ~ GAR006 / (FKN001 + FKN002),
    pct_heating_solar ~ GAR007 / (FKN001 + FKN002),
    pct_heating_other ~ GAR008 / (FKN001 + FKN002),
    pct_heating_none ~ GAR009 / (FKN001 + FKN002),
    
    # Money
    med_household_income ~ GMY001,
    med_household_income_2010_dollars ~ GMY001 * inflation_adjustment_2000,
    med_housing_value ~ GB7001,
    med_housing_value_2010_dollars ~ GB7001 * inflation_adjustment_2000,
    
    nhgis_dir = nhgis_dir, year = 2000
  )
  
)

# 2000 (block groups) -----------------------------------------------------
  
# TODO: THESE VARIABLES ARENT THE SAME
export_wrapper(
  
  geoid_functions = list(
    blck_grp = GEOID ~ sprintf("%02d%03d%06d%01d", as.numeric(STATEA), as.numeric(COUNTYA), as.numeric(TRACTA), as.numeric(BLCK_GRPA))
  ),
  
  query_args = list(
    
    # Universe: Total population
    population ~ FXS001,
    pct_female ~ (
      FYM024 + FYM025 + FYM026 + FYM027 + FYM028 + FYM029 + FYM030 + FYM031 +
      FYM032 + FYM033 + FYM034 + FYM035 + FYM036 + FYM037 + FYM038 + FYM039 +
      FYM040 + FYM041 + FYM042 + FYM043 + FYM044 + FYM045 + FYM046
    ) / FXS001,
    pct_age_under_5 ~ (FYM001 + FYM024) / FXS001,
    pct_age_5_to_9 ~ (FYM002 + FYM025) / FXS001,
    pct_age_10_to_14 ~ (FYM003 + FYM026) / FXS001,
    pct_age_15_to_17 ~ (FYM004 + FYM027) / FXS001,
    pct_age_18_to_19 ~ (FYM005 + FYM028) / FXS001,
    pct_age_20 ~ (FYM006 + FYM029) / FXS001,
    pct_age_21 ~ (FYM007 + FYM030) / FXS001,
    pct_age_22_to_24 ~ (FYM008 + FYM031) / FXS001,
    pct_age_25_to_29 ~ (FYM009 + FYM032) / FXS001,
    pct_age_30_to_34 ~ (FYM010 + FYM033) / FXS001,
    pct_age_35_to_39 ~ (FYM011 + FYM034) / FXS001,
    pct_age_40_to_44 ~ (FYM012 + FYM035) / FXS001,
    pct_age_45_to_49 ~ (FYM013 + FYM036) / FXS001,
    pct_age_50_to_54 ~ (FYM014 + FYM037) / FXS001,
    pct_age_55_to_59 ~ (FYM015 + FYM038) / FXS001,
    pct_age_60_to_61 ~ (FYM016 + FYM039) / FXS001,
    pct_age_62_to_64 ~ (FYM017 + FYM040) / FXS001,
    pct_age_65_to_66 ~ (FYM018 + FYM041) / FXS001,
    pct_age_67_to_69 ~ (FYM019 + FYM042) / FXS001,
    pct_age_65_to_69 ~ (FYM018 + FYM041 + FYM019 + FYM042) / FXS001,
    pct_age_70_to_74 ~ (FYM020 + FYM043) / FXS001,
    pct_age_75_to_79 ~ (FYM021 + FYM044) / FXS001,
    pct_age_80_to_84 ~ (FYM022 + FYM045) / FXS001,
    pct_age_over_85 ~ (FYM023 + FYM046) / FXS001,
    pct_white ~ FYE001 / FXS001,
    pct_non_hispanic_white ~ FYF001 / FXS001,
    pct_hispanic_white ~ FYF008 / FXS001,
    pct_black ~ FYE002 / FXS001,
    pct_non_hispanic_black ~ FYF002 / FXS001,
    pct_hispanic_black ~ FYF009 / FXS001,
    pct_native ~ FYE003 / FXS001,
    pct_non_hispanic_native ~ FYF003 / FXS001,
    pct_hispanic_native ~ FYF010 / FXS001,
    pct_asian ~ FYE004 / FXS001,
    pct_non_hispanic_asian ~ FYF004 / FXS001,
    pct_hispanic_asian ~ FYF011 / FXS001,
    pct_asian_pacific_islander ~ (FYE004 + FYE005) / FXS001,
    pct_non_hispanic_asian_pacific_islander ~ (FYF004 + FYF005) / FXS001,
    pct_hispanic_asian_pacific_islander ~ (FYF011 + FYF012) / FXS001,
    pct_two_or_more_races ~ FYE007 / FXS001,
    pct_non_hispanic_two_or_more_races ~ FYF007 / FXS001,
    pct_hispanic_two_or_more_races ~ FYF014 / FXS001,
    pct_hispanic ~ FYF008 / FXS001,
   
    # Universe: Workers 16 Years and Over
    pct_transport_auto ~ HDH001 / (
      HDH001 + HDH002 + HDH003 + HDH004 + HDH005 + HDH006 + HDH007
    ),
    pct_transport_public_transit ~ HDH002 / (
      HDH001 + HDH002 + HDH003 + HDH004 + HDH005 + HDH006 + HDH007
    ),
    pct_transport_motorcycle ~ HDH003 / (
      HDH001 + HDH002 + HDH003 + HDH004 + HDH005 + HDH006 + HDH007
    ),
    pct_transport_bicycle ~ HDH004 / (
      HDH001 + HDH002 + HDH003 + HDH004 + HDH005 + HDH006 + HDH007
    ),
    pct_transport_walk ~ HDH005 / (
      HDH001 + HDH002 + HDH003 + HDH004 + HDH005 + HDH006 + HDH007
    ),
    pct_transport_other ~ HDH006 / (
      HDH001 + HDH002 + HDH003 + HDH004 + HDH005 + HDH006 + HDH007
    ),
    pct_transport_wfh ~ HDH007 / (
      HDH001 + HDH002 + HDH003 + HDH004 + HDH005 + HDH006 + HDH007
    ),
   
    # Universe: Workers 16 Years and Over Who Did Not Work at Home
    pct_travel_lt_5_min ~ HDL001 / (
      HDL001 + HDL002 + HDL003 + HDL004 + HDL005 + HDL006 + HDL007 + HDL008 +
      HDL009 + HDL010 + HDL011 + HDL012
    ),
    pct_travel_5_to_9_min ~ HDL002 / (
      HDL001 + HDL002 + HDL003 + HDL004 + HDL005 + HDL006 + HDL007 + HDL008 +
      HDL009 + HDL010 + HDL011 + HDL012
    ),
    pct_travel_10_to_14_min ~ HDL003 / (
      HDL001 + HDL002 + HDL003 + HDL004 + HDL005 + HDL006 + HDL007 + HDL008 +
      HDL009 + HDL010 + HDL011 + HDL012
    ),
    pct_travel_15_to_19_min ~ HDL004 / (
      HDL001 + HDL002 + HDL003 + HDL004 + HDL005 + HDL006 + HDL007 + HDL008 +
      HDL009 + HDL010 + HDL011 + HDL012
    ),
    pct_travel_20_to_24_min ~ HDL005 / (
      HDL001 + HDL002 + HDL003 + HDL004 + HDL005 + HDL006 + HDL007 + HDL008 +
      HDL009 + HDL010 + HDL011 + HDL012
    ),
    pct_travel_25_to_29_min ~ HDL006 / (
      HDL001 + HDL002 + HDL003 + HDL004 + HDL005 + HDL006 + HDL007 + HDL008 +
      HDL009 + HDL010 + HDL011 + HDL012
    ),
    pct_travel_30_to_34_min ~ HDL007 / (
      HDL001 + HDL002 + HDL003 + HDL004 + HDL005 + HDL006 + HDL007 + HDL008 +
      HDL009 + HDL010 + HDL011 + HDL012
    ),
    pct_travel_35_to_39_min ~ HDL008 / (
      HDL001 + HDL002 + HDL003 + HDL004 + HDL005 + HDL006 + HDL007 + HDL008 +
      HDL009 + HDL010 + HDL011 + HDL012
    ),
    pct_travel_40_to_44_min ~ HDL009 / (
      HDL001 + HDL002 + HDL003 + HDL004 + HDL005 + HDL006 + HDL007 + HDL008 +
      HDL009 + HDL010 + HDL011 + HDL012
    ),
    pct_travel_45_to_59_min ~ HDL010 / (
      HDL001 + HDL002 + HDL003 + HDL004 + HDL005 + HDL006 + HDL007 + HDL008 +
      HDL009 + HDL010 + HDL011 + HDL012
    ),
    pct_travel_60_to_89_min ~ HDL011 / (
      HDL001 + HDL002 + HDL003 + HDL004 + HDL005 + HDL006 + HDL007 + HDL008 +
      HDL009 + HDL010 + HDL011 + HDL012
    ),
    pct_travel_gt_90_min ~ HDL012 / (
      HDL001 + HDL002 + HDL003 + HDL004 + HDL005 + HDL006 + HDL007 + HDL008 +
      HDL009 + HDL010 + HDL011 + HDL012
    ),
    
    # Universe: Persons 18 Years and Over
    pct_edu_lt_9th_grade ~ (
      HD1001 + HD1002 + HD1003 + HD1004 + HD1017 + HD1018 + HD1019 + HD1020
    ) / (
      HD1001 + HD1002 + HD1003 + HD1004 + HD1005 + HD1006 + HD1007 + HD1008 +
      HD1009 + HD1010 + HD1011 + HD1012 + HD1013 + HD1014 + HD1015 + HD1016 +
      HD1017 + HD1018 + HD1019 + HD1020 + HD1021 + HD1022 + HD1023 + HD1024 +
      HD1025 + HD1026 + HD1027 + HD1028 + HD1029 + HD1030 + HD1031 + HD1032
    ),
    pct_edu_9th_to_12th_grade ~ (
      HD1005 + HD1006 + HD1007 + HD1008 + HD1021 + HD1022 + HD1023 + HD1024
    ) / (
      HD1001 + HD1002 + HD1003 + HD1004 + HD1005 + HD1006 + HD1007 + HD1008 +
      HD1009 + HD1010 + HD1011 + HD1012 + HD1013 + HD1014 + HD1015 + HD1016 +
      HD1017 + HD1018 + HD1019 + HD1020 + HD1021 + HD1022 + HD1023 + HD1024 +
      HD1025 + HD1026 + HD1027 + HD1028 + HD1029 + HD1030 + HD1031 + HD1032
    ),
    pct_edu_high_school ~ (HD1009 + HD1025) / (
      HD1001 + HD1002 + HD1003 + HD1004 + HD1005 + HD1006 + HD1007 + HD1008 +
      HD1009 + HD1010 + HD1011 + HD1012 + HD1013 + HD1014 + HD1015 + HD1016 +
      HD1017 + HD1018 + HD1019 + HD1020 + HD1021 + HD1022 + HD1023 + HD1024 +
      HD1025 + HD1026 + HD1027 + HD1028 + HD1029 + HD1030 + HD1031 + HD1032
    ),
    pct_edu_some_college ~ (HD1010 + HD1011 + HD1026 + HD1027) / (
      HD1001 + HD1002 + HD1003 + HD1004 + HD1005 + HD1006 + HD1007 + HD1008 +
      HD1009 + HD1010 + HD1011 + HD1012 + HD1013 + HD1014 + HD1015 + HD1016 +
      HD1017 + HD1018 + HD1019 + HD1020 + HD1021 + HD1022 + HD1023 + HD1024 +
      HD1025 + HD1026 + HD1027 + HD1028 + HD1029 + HD1030 + HD1031 + HD1032
    ),
    pct_edu_associate ~ (HD1012 + HD1028) / (
      HD1001 + HD1002 + HD1003 + HD1004 + HD1005 + HD1006 + HD1007 + HD1008 +
      HD1009 + HD1010 + HD1011 + HD1012 + HD1013 + HD1014 + HD1015 + HD1016 +
      HD1017 + HD1018 + HD1019 + HD1020 + HD1021 + HD1022 + HD1023 + HD1024 +
      HD1025 + HD1026 + HD1027 + HD1028 + HD1029 + HD1030 + HD1031 + HD1032
    ),
    pct_edu_bachelors ~ (HD1013 + HD1029) / (
      HD1001 + HD1002 + HD1003 + HD1004 + HD1005 + HD1006 + HD1007 + HD1008 +
      HD1009 + HD1010 + HD1011 + HD1012 + HD1013 + HD1014 + HD1015 + HD1016 +
      HD1017 + HD1018 + HD1019 + HD1020 + HD1021 + HD1022 + HD1023 + HD1024 +
      HD1025 + HD1026 + HD1027 + HD1028 + HD1029 + HD1030 + HD1031 + HD1032
    ),
    pct_edu_graduate_or_professional ~ (
      HD1014 + HD1015 + HD1016 + HD1030 + HD1031 + HD1032
    ) / (
      HD1001 + HD1002 + HD1003 + HD1004 + HD1005 + HD1006 + HD1007 + HD1008 +
      HD1009 + HD1010 + HD1011 + HD1012 + HD1013 + HD1014 + HD1015 + HD1016 +
      HD1017 + HD1018 + HD1019 + HD1020 + HD1021 + HD1022 + HD1023 + HD1024 +
      HD1025 + HD1026 + HD1027 + HD1028 + HD1029 + HD1030 + HD1031 + HD1032
    ),
    
    # Universe: Persons for Whom Poverty Status Is Determined
    pct_poverty ~ HHE001 / (HHE001 + HHE002),
    
    # Universe: Households
    n_households ~ FY4001,
    mean_household_size ~ FZC001,
    pct_income_lt_10k ~ HF5001 / FY4001,
    pct_income_10k_to_15k ~ HF5002 / FY4001,
    pct_income_15k_to_20k ~ HF5003 / FY4001,
    pct_income_20k_to_25k ~ HF5004 / FY4001,
    pct_income_25k_to_30k ~ HF5005 / FY4001,
    pct_income_30k_to_35k ~ HF5006 / FY4001,
    pct_income_35k_to_40k ~ HF5007 / FY4001,
    pct_income_40k_to_45k ~ HF5008 / FY4001,
    pct_income_45k_to_50k ~ HF5009 / FY4001,
    pct_income_50k_to_60k ~ HF5010 / FY4001,
    pct_income_60k_to_75k ~ HF5011 / FY4001,
    pct_income_75k_to_100k ~ HF5012 / FY4001,
    pct_income_100k_to_125k ~ HF5013 / FY4001,
    pct_income_125k_to_150k ~ HF5014 / FY4001,
    pct_income_150k_to_200k ~ HF5015 / FY4001,
    pct_income_gt_150k ~ (HF5015 + HF5016) / FY4001,
    pct_income_gt_200k ~ HF5016 / FY4001,
    pct_public_assistance ~ HGJ001 / FY4001,
    
    # Universe: Civilian Persons 16 Years and Over in Labor Force
    pct_eligible_unemployed ~ (HEZ002 + HEZ004) / (HEZ001 + HEZ002 + HEZ003 + HEZ004),
    
    # Universe: Housing Units
    n_housing_units ~ FV5001,
    pct_housing_standalone ~ G63001 / FV5001,
    pct_housing_1_unit ~ (G63001 + G63002) / FV5001,
    pct_housing_2_units ~ G63003 / FV5001,
    pct_housing_3_to_4_units ~ G63004 / FV5001,
    pct_housing_5_to_9_units ~ G63005 / FV5001,
    pct_housing_10_to_19_units ~ G63006/ FV5001,
    pct_housing_20_to_49_units ~ G63007/ FV5001,
    pct_housing_gt_50_units ~ G63008 / FV5001,
    pct_housing_mobile_home ~ G63009 / FV5001,
    pct_housing_vehicle ~ G63010 / FV5001,
    
    # Universe: Occupied Housing Units
    n_occupied_housing_units ~ FWA001 + FWA002,
    pct_renting ~ FWA002 / (FWA001 + FWA002),
    pct_heating_utility_gas ~ G7F001 / (FWA001 + FWA002),
    pct_heating_bottled_gas ~ G7F002 / (FWA001 + FWA002),
    pct_heating_electricity ~ G7F003 / (FWA001 + FWA002),
    pct_heating_oil ~ G7F004 / (FWA001 + FWA002),
    pct_heating_coal ~ G7F005 / (FWA001 + FWA002),
    pct_heating_wood ~ G7F006 / (FWA001 + FWA002),
    pct_heating_solar ~ G7F007 / (FWA001 + FWA002),
    pct_heating_other ~ G7F008 / (FWA001 + FWA002),
    pct_heating_none ~ G7F009 / (FWA001 + FWA002),
    
    # Money
    med_household_income ~ HF6001,
    med_household_income_2010_dollars ~ HF6001 * inflation_adjustment_2000,
    med_housing_value ~ G8V001,
    med_housing_value_2010_dollars ~ G8V001 * inflation_adjustment_2000,
    
    nhgis_dir = nhgis_dir, year = 2000
  )

)

# 2000 (blocks) -----------------------------------------------------------

export_wrapper(
  
  geoid_functions = list(
    block = GEOID ~ sprintf("%02d%03d%06d%01d", as.numeric(STATEA), as.numeric(COUNTYA), as.numeric(TRACTA), as.numeric(BLOCKA))
  ),
  
  query_args = list(
    
    # Universe: Total population
    population ~ FXS001,
    pct_female ~ (
      FYM024 + FYM025 + FYM026 + FYM027 + FYM028 + FYM029 + FYM030 + FYM031 +
      FYM032 + FYM033 + FYM034 + FYM035 + FYM036 + FYM037 + FYM038 + FYM039 +
      FYM040 + FYM041 + FYM042 + FYM043 + FYM044 + FYM045 + FYM046
    ) / FXS001,
    pct_age_under_5 ~ (FYM001 + FYM024) / FXS001,
    pct_age_5_to_9 ~ (FYM002 + FYM025) / FXS001,
    pct_age_10_to_14 ~ (FYM003 + FYM026) / FXS001,
    pct_age_15_to_17 ~ (FYM004 + FYM027) / FXS001,
    pct_age_18_to_19 ~ (FYM005 + FYM028) / FXS001,
    pct_age_20 ~ (FYM006 + FYM029) / FXS001,
    pct_age_21 ~ (FYM007 + FYM030) / FXS001,
    pct_age_22_to_24 ~ (FYM008 + FYM031) / FXS001,
    pct_age_25_to_29 ~ (FYM009 + FYM032) / FXS001,
    pct_age_30_to_34 ~ (FYM010 + FYM033) / FXS001,
    pct_age_35_to_39 ~ (FYM011 + FYM034) / FXS001,
    pct_age_40_to_44 ~ (FYM012 + FYM035) / FXS001,
    pct_age_45_to_49 ~ (FYM013 + FYM036) / FXS001,
    pct_age_50_to_54 ~ (FYM014 + FYM037) / FXS001,
    pct_age_55_to_59 ~ (FYM015 + FYM038) / FXS001,
    pct_age_60_to_61 ~ (FYM016 + FYM039) / FXS001,
    pct_age_62_to_64 ~ (FYM017 + FYM040) / FXS001,
    pct_age_65_to_66 ~ (FYM018 + FYM041) / FXS001,
    pct_age_67_to_69 ~ (FYM019 + FYM042) / FXS001,
    pct_age_65_to_69 ~ (FYM018 + FYM041 + FYM019 + FYM042) / FXS001,
    pct_age_70_to_74 ~ (FYM020 + FYM043) / FXS001,
    pct_age_75_to_79 ~ (FYM021 + FYM044) / FXS001,
    pct_age_80_to_84 ~ (FYM022 + FYM045) / FXS001,
    pct_age_over_85 ~ (FYM023 + FYM046) / FXS001,
    pct_white ~ FYE001 / FXS001,
    pct_non_hispanic_white ~ FYF001 / FXS001,
    pct_hispanic_white ~ FYF008 / FXS001,
    pct_black ~ FYE002 / FXS001,
    pct_non_hispanic_black ~ FYF002 / FXS001,
    pct_hispanic_black ~ FYF009 / FXS001,
    pct_native ~ FYE003 / FXS001,
    pct_non_hispanic_native ~ FYF003 / FXS001,
    pct_hispanic_native ~ FYF010 / FXS001,
    pct_asian ~ FYE004 / FXS001,
    pct_non_hispanic_asian ~ FYF004 / FXS001,
    pct_hispanic_asian ~ FYF011 / FXS001,
    pct_two_or_more_races ~ FYE007 / FXS001,
    pct_non_hispanic_two_or_more_races ~ FYF007 / FXS001,
    pct_hispanic_two_or_more_races ~ FYF014 / FXS001,
    pct_asian_pacific_islander ~ (FYE004 + FYE005) / FXS001,
    pct_non_hispanic_asian_pacific_islander ~ (FYF004 + FYF005) / FXS001,
    pct_hispanic_asian_pacific_islander ~ (FYF011 + FYF012) / FXS001,
    pct_hispanic ~ FYF008 / FXS001,
   
    # Universe: Workers 16 Years and Over
    # pct_transport_* not available for blocks
    
    # Universe: Workers 16 Years and Over Who Did Not Work at Home
    # pct_travel_* not available for blocks
    
    # Universe: Persons 18 Years and Over
    # pct_edu_* not available for blocks
    
    # Universe: Persons for Whom Poverty Status Is Determined
    # pct_poverty not available for blocks
    
    # Universe: Households
    n_households ~ FY4001,
    mean_household_size ~ FZC001,
    # pct_income_* not available for blocks
    
    # Universe: Housing Units
    n_housing_units ~ FV5001,
    
    # Universe: Occupied Housing Units
    n_occupied_housing_units ~ FWA001 + FWA002,
    pct_renting ~ FWA002 / (FWA001 + FWA002),
    
    # pct_housing_* not available for blocks
    
    # pct_heating_* not available for blocks
    
    # Money
    # med_household_income* not available for blocks
    # med_housing_value* not available for blocks
    
    nhgis_dir = nhgis_dir, year = 2000
  )
  
)

# 1990 (larger than blocks) -----------------------------------------------

# BLS CPI calculator January 1990 -> January 2010
inflation_adjustment_1990 <- 1.70084

export_wrapper(
  
  geoid_functions = list(
    # ZCTA unavailable
    county = GEOID ~ sprintf("%02d%03d", as.numeric(STATEA), as.numeric(COUNTYA)),
    tract = GEOID ~ sprintf("%02d%03d%06d", as.numeric(STATEA), as.numeric(COUNTYA), as.numeric(TRACTA)),
    blck_grp = GEOID ~ sprintf("%02d%03d%06d%01d", as.numeric(STATEA), as.numeric(COUNTYA), as.numeric(TRACTA), as.numeric(BLCK_GRPA))
  ),
  
  query_args = list(
    
    # Universe: Total population
    population ~ ET1001,
    pct_female ~ EUX002 / ET1001,
    pct_age_under_5 ~ (ET3001 + ET3002 + ET3003) / ET1001,
    pct_age_5_to_9 ~ (ET3004 + ET3005 + ET3006) / ET1001,
    pct_age_10_to_14 ~ (ET3007 + ET3008 + ET3009) / ET1001,
    pct_age_15_to_17 ~ (ET3010 + ET3011 + ET3012) / ET1001,
    pct_age_18_to_19 ~ (ET3013 + ET3014) / ET1001,
    pct_age_20 ~ ET3015 / ET1001,
    pct_age_21 ~ ET3016 / ET1001,
    pct_age_22_to_24 ~ ET3017 / ET1001,
    pct_age_25_to_29 ~ ET3018 / ET1001,
    pct_age_30_to_34 ~ ET3019 / ET1001,
    pct_age_35_to_39 ~ ET3020 / ET1001,
    pct_age_40_to_44 ~ ET3021 / ET1001,
    pct_age_45_to_49 ~ ET3022 / ET1001,
    pct_age_50_to_54 ~ ET3023 / ET1001,
    pct_age_55_to_59 ~ ET3024 / ET1001,
    pct_age_60_to_61 ~ ET3025 / ET1001,
    pct_age_62_to_64 ~ ET3026 / ET1001,
    # pct_age_65_to_66 ~ , # Unavailable - only 65_to_69 ET3027
    # pct_age_67_to_69 ~ , # Unavailable - only 65_to_69 ET3027
    pct_age_65_to_69 ~ ET3027 / ET1001,
    pct_age_70_to_74 ~ ET3028 / ET1001,
    pct_age_75_to_79 ~ ET3029 / ET1001,
    pct_age_80_to_84 ~ ET3030 / ET1001,
    pct_age_over_85 ~ ET3031 / ET1001,
    pct_white ~ EUY001 / ET1001,
    pct_non_hispanic_white ~ ET2001 / ET1001,
    pct_hispanic_white ~ ET2006 / ET1001,
    pct_black ~ EUY002/ ET1001,
    pct_non_hispanic_black ~ ET2002 / ET1001,
    pct_hispanic_black ~ ET2007 / ET1001,
    # pct_asian ~, # Unavailable - only Asian or Pacific Islander
    # pct_non_hispanic_asian ~, # Unavailable - only Asian or Pacific Islander
    # pct_hispanic_asian ~, # Unavailable - only Asian or Pacific Islander
    pct_native ~ EUY003/ ET1001,
    pct_non_hispanic_native ~ ET2003 / ET1001,
    pct_hispanic_native ~ ET2008 / ET1001,
    pct_asian_pacific_islander ~ EUY004 / ET1001,
    pct_non_hispanic_asian_pacific_islander ~ ET2004 / ET1001,
    pct_hispanic_asian_pacific_islander ~ ET2009 / ET1001,
    pct_hispanic ~ (ET2006 + ET2007 + ET2008 + ET2009 + ET2010) / ET1001,
   
    # Universe: Workers 16 Years and Over
    pct_transport_auto ~ (E3U001 + E3U002) / (
      E3U001 + E3U002 + E3U003 + E3U004 + E3U005 + E3U006 + E3U007 + E3U008 +
      E3U009 + E3U010 + E3U011 + E3U012 + E3U013
    ),
    pct_transport_public_transit ~ (
      E3U003 + E3U004 + E3U005 + E3U006 + E3U007 + E3U008
    ) / (
      E3U001 + E3U002 + E3U003 + E3U004 + E3U005 + E3U006 + E3U007 + E3U008 +
      E3U009 + E3U010 + E3U011 + E3U012 + E3U013
    ),
    pct_transport_motorcycle ~ E3U009 / (
      E3U001 + E3U002 + E3U003 + E3U004 + E3U005 + E3U006 + E3U007 + E3U008 +
      E3U009 + E3U010 + E3U011 + E3U012 + E3U013
    ),
    pct_transport_bicycle ~ E3U010 / (
      E3U001 + E3U002 + E3U003 + E3U004 + E3U005 + E3U006 + E3U007 + E3U008 +
      E3U009 + E3U010 + E3U011 + E3U012 + E3U013
    ),
    pct_transport_walk ~ E3U011 / (
      E3U001 + E3U002 + E3U003 + E3U004 + E3U005 + E3U006 + E3U007 + E3U008 +
      E3U009 + E3U010 + E3U011 + E3U012 + E3U013
    ),
    pct_transport_other ~ E3U012 / (
      E3U001 + E3U002 + E3U003 + E3U004 + E3U005 + E3U006 + E3U007 + E3U008 +
      E3U009 + E3U010 + E3U011 + E3U012 + E3U013
    ),
    pct_transport_wfh ~ E3U013 / (
      E3U001 + E3U002 + E3U003 + E3U004 + E3U005 + E3U006 + E3U007 + E3U008 +
      E3U009 + E3U010 + E3U011 + E3U012 + E3U013
    ),
   
    # Universe: Workers 16 Years and Over Who Did Not Work at Home
    pct_travel_lt_5_min ~ E3W001 / (
      E3W001 + E3W002 + E3W003 + E3W004 + E3W005 + E3W006 + E3W007 + E3W008 +
        E3W009 + E3W010 + E3W011 + E3W012
    ),
    pct_travel_5_to_9_min ~ E3W002 / (
      E3W001 + E3W002 + E3W003 + E3W004 + E3W005 + E3W006 + E3W007 + E3W008 +
        E3W009 + E3W010 + E3W011 + E3W012
    ),
    pct_travel_10_to_14_min ~ E3W003 / (
      E3W001 + E3W002 + E3W003 + E3W004 + E3W005 + E3W006 + E3W007 + E3W008 +
        E3W009 + E3W010 + E3W011 + E3W012
    ),
    pct_travel_15_to_19_min ~ E3W004 / (
      E3W001 + E3W002 + E3W003 + E3W004 + E3W005 + E3W006 + E3W007 + E3W008 +
        E3W009 + E3W010 + E3W011 + E3W012
    ),
    pct_travel_20_to_24_min ~ E3W005 / (
      E3W001 + E3W002 + E3W003 + E3W004 + E3W005 + E3W006 + E3W007 + E3W008 +
        E3W009 + E3W010 + E3W011 + E3W012
    ),
    pct_travel_25_to_29_min ~ E3W006 / (
      E3W001 + E3W002 + E3W003 + E3W004 + E3W005 + E3W006 + E3W007 + E3W008 +
        E3W009 + E3W010 + E3W011 + E3W012
    ),
    pct_travel_30_to_34_min ~ E3W007 / (
      E3W001 + E3W002 + E3W003 + E3W004 + E3W005 + E3W006 + E3W007 + E3W008 +
        E3W009 + E3W010 + E3W011 + E3W012
    ),
    pct_travel_35_to_39_min ~ E3W008 / (
      E3W001 + E3W002 + E3W003 + E3W004 + E3W005 + E3W006 + E3W007 + E3W008 +
        E3W009 + E3W010 + E3W011 + E3W012
    ),
    pct_travel_40_to_44_min ~ E3W009 / (
      E3W001 + E3W002 + E3W003 + E3W004 + E3W005 + E3W006 + E3W007 + E3W008 +
        E3W009 + E3W010 + E3W011 + E3W012
    ),
    pct_travel_45_to_59_min ~ E3W010 / (
      E3W001 + E3W002 + E3W003 + E3W004 + E3W005 + E3W006 + E3W007 + E3W008 +
        E3W009 + E3W010 + E3W011 + E3W012
    ),
    pct_travel_60_to_89_min ~ E3W011 / (
      E3W001 + E3W002 + E3W003 + E3W004 + E3W005 + E3W006 + E3W007 + E3W008 +
        E3W009 + E3W010 + E3W011 + E3W012
    ),
    pct_travel_gt_90_min ~ E3W012 / (
      E3W001 + E3W002 + E3W003 + E3W004 + E3W005 + E3W006 + E3W007 + E3W008 +
        E3W009 + E3W010 + E3W011 + E3W012
    ),
    
    # Universe: Persons 18 Years and Over
    pct_edu_lt_9th_grade ~ E37001 / (
      E37001 + E37002 + E37003 + E37004 + E37005 + E37006 + E37007
    ),
    pct_edu_9th_to_12th_grade ~ E37002 / (
      E37001 + E37002 + E37003 + E37004 + E37005 + E37006 + E37007
    ),
    pct_edu_high_school ~ E37003 / (
      E37001 + E37002 + E37003 + E37004 + E37005 + E37006 + E37007
    ),
    pct_edu_some_college ~ E37004 / (
      E37001 + E37002 + E37003 + E37004 + E37005 + E37006 + E37007
    ),
    pct_edu_associate ~ E37005 / (
      E37001 + E37002 + E37003 + E37004 + E37005 + E37006 + E37007
    ),
    pct_edu_bachelors ~ E37006 / (
      E37001 + E37002 + E37003 + E37004 + E37005 + E37006 + E37007
    ),
    pct_edu_graduate_or_professional ~ E37007 / (
      E37001 + E37002 + E37003 + E37004 + E37005 + E37006 + E37007
    ),
    
    # Universe: Persons for Whom Poverty Status Is Determined
    pct_poverty ~ (
      E07013 + E07014 + E07015 + E07016 + E07017 + E07018 + E07019 + E07020 +
      E07021 + E07022 + E07023 + E07024
    ) / (
      E07001 + E07002 + E07003 + E07004 + E07005 + E07006 + E07007 + E07008 +
      E07009 + E07010 + E07011 + E07012 + E07013 + E07014 + E07015 + E07016 +
      E07017 + E07018 + E07019 + E07020 + E07021 + E07022 + E07023 + E07024
    ),
    
    # Universe: Households
    n_households ~ EUO001,
    #pct_households_single_father ~ (ET8005 + ET8006) / EUO001,
    #pct_households_single_mother ~ (ET8007 + ET8008) / EUO001,
    pct_income_lt_10k ~ (E4T001 + E4T002) / EUO001,
    pct_income_10k_to_15k ~ (E4T003 + E4T004) / EUO001,
    pct_income_15k_to_20k ~ (E4T005 + E4T006) / EUO001,
    pct_income_20k_to_25k ~ (E4T007 + E4T008) / EUO001,
    pct_income_25k_to_30k ~ (E4T009 + E4T010) / EUO001,
    pct_income_30k_to_35k ~ (E4T011 + E4T012) / EUO001,
    pct_income_35k_to_40k ~ (E4T013 + E4T014) / EUO001,
    pct_income_40k_to_45k ~ (E4T015 + E4T016) / EUO001,
    pct_income_45k_to_50k ~ (E4T017 + E4T018) / EUO001,
    pct_income_50k_to_60k ~ (E4T019 + E4T020) / EUO001,
    pct_income_60k_to_75k ~ E4T021 / EUO001,
    pct_income_75k_to_100k ~ E4T022 / EUO001,
    pct_income_100k_to_125k ~ E4T023 / EUO001,
    pct_income_125k_to_150k ~ E4T024 / EUO001,
    # pct_income_150k_to_200k ~, # Unavailable - only gt_150k
    # pct_income_gt_200k ~, # Unavailable - only gt_150k
    pct_income_gt_150k ~ E4T025 / EUO001,
    pct_public_assistance ~ E5A001 / EUO001,
    
    pct_eligible_unemployed ~ (E4I003 + E4I007) / ET1001,
    
    # Universe: Housing Units
    n_housing_units ~ ESA001,
    pct_housing_standalone ~ ETH001 / ESA001,
    pct_housing_1_unit ~ (ETH001 + ETH002) / ESA001,
    pct_housing_2_units ~ ETH003 / ESA001,
    pct_housing_3_to_4_units ~ ETH004 / ESA001,
    pct_housing_5_to_9_units ~ ETH005 / ESA001,
    pct_housing_10_to_19_units ~ ETH006 / ESA001,
    pct_housing_20_to_49_units ~ ETH007 / ESA001,
    pct_housing_gt_50_units ~ ETH008 / ESA001,
    pct_housing_mobile_home ~ ETH009 / ESA001,
    # pct_housing_vehicle ~, # Unavailable
    pct_housing_other ~ ETH010 / ESA001,
    
    # Universe: Occupied Housing Units
    n_occupied_housing_units ~ ES1001 + ES1002,
    pct_renting ~ ES1002 / (ES1001 + ES1002),
    pct_heating_utility_gas ~ EYE001 / (ES1001 + ES1002),
    pct_heating_bottled_gas ~ EYE002 / (ES1001 + ES1002),
    pct_heating_electricity ~ EYE003 /(ES1001 + ES1002),
    pct_heating_oil ~ EYE004 / (ES1001 + ES1002),
    pct_heating_coal ~ EYE005 / (ES1001 + ES1002),
    pct_heating_wood ~ EYE006 / (ES1001 + ES1002),
    pct_heating_solar ~ EYE007 / (ES1001 + ES1002),
    pct_heating_other ~ EYE008 / (ES1001 + ES1002),
    pct_heating_none ~ EYE009 / (ES1001 + ES1002),
    
    # Money
    med_household_income ~ E4U001,
    med_household_income_2010_dollars ~ E4U001 * inflation_adjustment_1990,
    med_housing_value ~ EST001,
    med_housing_value_2010_dollars ~ EST001 * inflation_adjustment_1990,
    
    nhgis_dir = nhgis_dir, year = 1990
  )
  
)

# 1990 (blocks) -----------------------------------------------------------

export_wrapper(
  
  geoid_functions = list(
    block = GEOID ~ sprintf("%02d%03d%06d%01d", as.numeric(STATEA), as.numeric(COUNTYA), as.numeric(TRACTA), as.numeric(BLOCKA))
  ),
  
  query_args = list(
    
    # Universe: Total population
    population ~ ET1001,
    pct_female ~ EUX002 / ET1001,
    pct_age_under_5 ~ (ET3001 + ET3002 + ET3003) / ET1001,
    pct_age_5_to_9 ~ (ET3004 + ET3005 + ET3006) / ET1001,
    pct_age_10_to_14 ~ (ET3007 + ET3008 + ET3009) / ET1001,
    pct_age_15_to_17 ~ (ET3010 + ET3011 + ET3012) / ET1001,
    pct_age_18_to_19 ~ (ET3013 + ET3014) / ET1001,
    pct_age_20 ~ ET3015 / ET1001,
    pct_age_21 ~ ET3016 / ET1001,
    pct_age_22_to_24 ~ ET3017 / ET1001,
    pct_age_25_to_29 ~ ET3018 / ET1001,
    pct_age_30_to_34 ~ ET3019 / ET1001,
    pct_age_35_to_39 ~ ET3020 / ET1001,
    pct_age_40_to_44 ~ ET3021 / ET1001,
    pct_age_45_to_49 ~ ET3022 / ET1001,
    pct_age_50_to_54 ~ ET3023 / ET1001,
    pct_age_55_to_59 ~ ET3024 / ET1001,
    pct_age_60_to_61 ~ ET3025 / ET1001,
    pct_age_62_to_64 ~ ET3026 / ET1001,
    pct_age_65_to_69 ~ ET3027 / ET1001,
    # pct_age_65_to_66 ~ , # Unavailable - only 65_to_69 ET3027
    # pct_age_67_to_69 ~ , # Unavailable - only 65_to_69 ET3027
    pct_age_70_to_74 ~ ET3028 / ET1001,
    pct_age_75_to_79 ~ ET3029 / ET1001,
    pct_age_80_to_84 ~ ET3030 / ET1001,
    pct_age_over_85 ~ ET3031 / ET1001,
    pct_white ~ EUY001 / ET1001,
    pct_non_hispanic_white ~ ET2001 / ET1001,
    pct_hispanic_white ~ ET2006 / ET1001,
    pct_black ~ EUY002/ ET1001,
    pct_non_hispanic_black ~ ET2002 / ET1001,
    pct_hispanic_black ~ ET2007 / ET1001,
    # pct_asian ~, # Unavailable - only Asian or Pacific Islander
    # pct_non_hispanic_asian ~, # Unavailable - only Asian or Pacific Islander
    # pct_hispanic_asian ~, # Unavailable - only Asian or Pacific Islander
    pct_native ~ EUY003/ ET1001,
    pct_non_hispanic_native ~ ET2003 / ET1001,
    pct_hispanic_native ~ ET2008 / ET1001,
    pct_asian_pacific_islander ~ EUY004 / ET1001,
    pct_non_hispanic_asian_pacific_islander ~ ET2004 / ET1001,
    pct_hispanic_asian_pacific_islander ~ ET2009 / ET1001,
    pct_hispanic ~ (ET2006 + ET2007 + ET2008 + ET2009 + ET2010) / ET1001,
   
    # Universe: Workers 16 Years and Over
    # pct_transport_* not available for blocks
    
    # Universe: Workers 16 Years and Over Who Did Not Work at Home
    # pct_travel_* not available for blocks
    
    # Universe: Persons 18 Years and Over
    # pct_edu_* not available for blocks
    
    # Universe: Persons for Whom Poverty Status Is Determined
    # pct_poverty not available for blocks
    
    # Universe: Households
    n_households ~ EUO001,
    pct_households_single_father ~ (ET8005 + ET8006) / EUO001,
    pct_households_single_mother ~ (ET8007 + ET8008) / EUO001,
    # pct_income_* not available for blocks
    
    # pct_public_assistance not available for blocks
    
    # pct_eligible_unemployed not available for blocks
    
    # Universe: Housing Units
    n_housing_units ~ ESA001,
    pct_housing_standalone ~ ETH001 / ESA001,
    pct_housing_1_unit ~ (ETH001 + ETH002) / ESA001,
    pct_housing_2_units ~ ETH003 / ESA001,
    pct_housing_3_to_4_units ~ ETH004 / ESA001,
    pct_housing_5_to_9_units ~ ETH005 / ESA001,
    pct_housing_10_to_19_units ~ ETH006 / ESA001,
    pct_housing_20_to_49_units ~ ETH007 / ESA001,
    pct_housing_gt_50_units ~ ETH008 / ESA001,
    pct_housing_mobile_home ~ ETH009 / ESA001,
    # pct_housing_vehicle ~, # Unavailable
    pct_housing_other ~ ETH010 / ESA001,
    
    # Universe: Occupied Housing Units
    n_occupied_housing_units ~ ES1001 + ES1002,
    pct_renting ~ ES1002 / (ES1001 + ES1002),
    # pct_heating_* not available for blocks
    
    # Money
    # med_household_income* not available for blocks
    med_housing_value ~ EST001,
    med_housing_value_2010_dollars ~ EST001 * inflation_adjustment_1990,
    
    nhgis_dir = nhgis_dir, year = 1990
  )
  
)
