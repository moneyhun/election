source("header.R")

# IMPORT ------------------------------------------------------------------
# Read 2000-2016 county level results
county_pres_raw <- fread("data/countypres_2000-2016.csv") %>% tibble()

# Get county demographics
county_demo_raw<- list(
  "2012" = read_csv("data/acs5/dp05_2012.csv", skip = 1) %>% mutate(year = 2012),
  "2016" = read_csv("data/acs5/dp05_2016.csv", skip = 1) %>% mutate(year = 2016),
  "2020" = read_csv("data/acs5/dp05_2018.csv", skip = 1) %>% mutate(year = 2020)
)

# Key for US counties data
county_key <- read.xlsx("data/county_key.xlsx") %>% tibble() %>%
  filter(str_sub(Item_Id, 1, 3) %in% c("INC", "LND"))

# Land area, var of interest is LND110210D
county_land_area_raw <- read.xlsx("data/county_landarea.xlsx") %>% tibble()

# Income
county_income_raw <- read.xlsx("data/county_income.xlsx") %>% tibble()

# RESHAPE AND CLEAN --------------------------------------------------------
# Presidential vote (county-year)
# Exclude Alaska bc votes are reported by state house distict
county_pres <- county_pres_raw %>%
  select(-office, -version, -candidate) %>%
  filter(party %in% c("democrat", "republican")) %>%
  pivot_wider(names_from = party, 
              values_from = candidatevotes, 
              names_prefix = "votes_") %>%
  mutate(pct_dem = votes_democrat/totalvotes, 
         pct_rep = votes_republican/totalvotes,
         totalvotes_2p = votes_democrat + votes_republican,
         pct_2p_dem = votes_democrat/totalvotes_2p, 
         pct_2p_rep = votes_republican/totalvotes_2p, 
         n2p = totalvotes - totalvotes_2p, 
         pct_n2p = n2p/totalvotes) %>%
  rename(fips = FIPS) %>%
  filter(year >= 2012, 
         state_po != "AK", 
         !is.na(fips))

# Land area (county)
county_land_area <- county_land_area_raw %>%
  transmute(fips = as.integer(STCOU),
            area = LND110210D)

# Income (county, as of 2010)
county_income <- county_income_raw %>%
  transmute(fips = as.integer(STCOU),
            median_inc = INC110209D, 
            mean_inc = INC120209D)

# Demographics (county-year)
county_demo_list <- list()
for (i in names(county_demo_raw)) {
  county_demo_list[[i]] <- county_demo_raw[[i]] %>%
    select(id, starts_with("Estimate!!HISPANIC OR LATINO AND RACE!!")) %>%
    mutate(year = i)
  
  names(county_demo_list[[i]]) <- names(county_demo_list[[i]]) %>%
    str_replace_all("Estimate!!HISPANIC OR LATINO AND RACE!!", "") %>%
    janitor::make_clean_names() %>%
    str_replace_all(., "total_population_", "")
}

county_demo <- bind_rows(county_demo_list) %>%
  mutate(id = str_replace_all(id, "0500000US", "")) %>%
  mutate_all(as.character) %>%
  pivot_longer(cols = -c(id, year)) %>%
  rename(category = name, 
         n = value) %>%
  mutate(n = as.numeric(n), 
         category = category %>% 
           str_replace_all(., "hispanic_or_latino", "hl") %>%
           str_remove_all(., "_of_any_race") %>%
           str_remove_all(., "_alone") %>%
           str_replace_all(., "some_other_race", "oth") %>%
           str_remove_all(., "_or_african_american") %>%
           str_replace_all(., "american_indian_and_alaska_native", "ind") %>%
           str_replace_all(., "native_hawaiian_and_other_pacific_islander", "pac") %>%
           str_replace_all(., "two_or_more_races", "two")) %>%
  filter(!str_detect(category, "races")) %>%
  pivot_wider(names_from = category, values_from = n) %>%
  mutate(fips = as.numeric(id), 
         year = as.numeric(year)) %>%
  select(-id)

# JOIN --------------------------------------------------------------------
county_year <- county_pres %>%
  left_join(county_income, by = "fips") %>%
  left_join(county_land_area, by = "fips") %>%
  left_join(county_demo, by = c("year", "fips"))

# votes by year, demographics as of 2016
county <- county_year %>%
  select(year, 
         state, 
         state_po, 
         county, 
         fips, 
         median_inc, 
         mean_inc, 
         area, 
         total_population, 
         contains("hl")) %>%
  filter(year == 2016) %>%
  select(-year) %>%
  left_join(county_pres %>%
              filter(year == 2012) %>%
              select(fips, pct_2p_dem_2012 = pct_2p_dem), 
            by = "fips") %>%
  left_join(county_pres %>%
              filter(year == 2016) %>%
              select(fips, pct_2p_dem_2016 = pct_2p_dem), 
            by = "fips")