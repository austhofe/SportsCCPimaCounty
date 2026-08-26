# Packages
library(readxl)
library(dplyr)
library(stringr)
library(purrr)
library(broom)
library(writexl)

# -------------------------------------------------------------------------
# 1. Import data
# -------------------------------------------------------------------------

file_in <- "C:/Users/barrette/Documents/Work/Sports and CC/Climate+Change+and+Sport+-+Pima+County_March+10,+2026_07.56.xlsx"

dat <- read_excel(
  file_in,
  sheet = "Sheet0"
)

# -------------------------------------------------------------------------
# 2. Identify variables used for the missingness calculation
# -------------------------------------------------------------------------

missing_vars <- names(dat)[
  match("Q25", names(dat)):match("otherimpacts_text", names(dat))
]

dat <- dat %>%
  mutate(
    missing = rowSums(is.na(across(all_of(missing_vars)))),
    missing_percent = missing / 103 * 100
  ) %>%
  filter(
    Q25 != "-99",
    missing_percent < 100
  ) %>%
  rename(consent = Q25)

# -------------------------------------------------------------------------
# 3. Create sport_role
# -------------------------------------------------------------------------

dat <- dat %>%
  mutate(
    sport_role = case_when(
      role_3 == "1" ~ 1,
      role_2 == "1" ~ 2,
      role_1 == "1" ~ 3,
      role_4 == "1" | role_5 == "1" | role_6 == "1" ~ 4,
      TRUE ~ NA_real_
    ),
    
    sport_role = factor(
      sport_role,
      levels = 1:4,
      labels = c(
        "Administrator",
        "Coach, Athletic Trainer, or Fitness Professional",
        "Athlete",
        "Other (Sports Official, Medical Staff)"
      )
    )
  )

# -------------------------------------------------------------------------
# 4. Sport and sport-level variables
# -------------------------------------------------------------------------

dat <- dat %>%
  mutate(
    sport = factor(
      as.numeric(as.character(sport)),
      levels = 1:7,
      labels = c(
        "Running/track and field",
        "Cycling",
        "Baseball",
        "Soccer",
        "Swimming",
        "Football",
        "Other"
      )
    ),
    sport_level = factor(
      as.numeric(as.character(sport_level)),
      levels = 1:4,
      labels = c(
        "Youth",
        "Adult sports/rec",
        "Elite or collegiate",
        "Other"
      )
    ),
    
    sport_start = as.numeric(as.character(sport_start)),
    sport_end   = as.numeric(as.character(sport_end))
  )

dat <- dat %>%
  mutate(
    sport_text = str_trim(as.character(sport_7_TEXT)),
    
    sports = case_when(
      sport_text %in% c("Golf") ~ "Golf",
      sport_text %in% c("Mountain Biking") ~ "Mountain Biking",
      sport_text %in% c("Pickleball") ~ "Pickleball",
      str_to_lower(sport_text) == "triathlon" ~ "Triathlon",
      str_to_lower(sport_text) == "ultimate frisbee" ~ "Ultimate frisbee",
      
      str_detect(sport_text, "Basketball") ~ "Golf",
      str_detect(sport_text, "Adaptive") ~ "Adaptive",
      
      sport == "Running/track and field" ~ "Running/track and field",
      sport == "Cycling" ~ "Cycling",
      sport == "Baseball" ~ "Baseball",
      sport == "Soccer" ~ "Soccer",
      sport == "Swimming" ~ "Swimming",
      sport == "Football" ~ "Football",
      
      sport == "Other" ~ "Other",
      
      TRUE ~ NA_character_
    )
  )

# -------------------------------------------------------------------------
# 9. Create season indicators
# -------------------------------------------------------------------------

dat <- dat %>%
  mutate(
    winter = if_else(
      sport_start == 1 | sport_end == 1,
      1, NA_real_
    ),
    
    spring = if_else(
      sport_start == 2 | sport_end == 2,
      1, NA_real_
    ),
    
    summer = if_else(
      sport_start == 3 | sport_end == 3,
      1, NA_real_
    ),
    
    fall = if_else(
      sport_start == 4 | sport_end == 4,
      1, NA_real_
    ),
    
    year_round = if_else(
      sport_start == 5,
      1, NA_real_
    )
  )


#----------------------------------------
# Identify likelihood and severity variables
#----------------------------------------


likelihood_vars <- names(dat) %>%
  str_subset("likelihood")

severity_vars <- names(dat) %>%
  str_subset("severity")

# Create a lookup table of the actual variable names
likelihood_lookup <- tibble(
  variable = likelihood_vars,
  item = str_remove(likelihood_vars, "_likelihood.*")
)

severity_lookup <- tibble(
  variable = severity_vars,
  item = str_remove(severity_vars, "_severity.*")
)

# Combine actual variable names
test_vars <- bind_rows(
  likelihood_lookup %>%
    mutate(outcome = "Likelihood"),
  
  severity_lookup %>%
    mutate(outcome = "Severity")
)


# Convert likelihood and severity variables to numeric
dat <- dat %>%
  mutate(
    across(
      all_of(c(likelihood_vars, severity_vars)),
      ~ as.numeric(.x)
    )
  )

# Likelihood:
  # -99 = missing
  #  7  = NA
  #  6  = 5 ("Already Experiencing" treated as "Highly Likely")
dat <- dat %>%
  mutate(
    across(
      all_of(likelihood_vars),
      ~ case_when(
        .x %in% c(-99, 7) ~ NA_real_,
        .x == 6 ~ 5,
        TRUE ~ .x
      )
    )
  )

# Severity:
# -99 = missing
dat <- dat %>%
  mutate(
    across(
      all_of(severity_vars),
      ~ na_if(.x, -99)
    )
  )

# -------------------------------------------------------------------------
# QC: response distribution for every likelihood and severity variable
# -------------------------------------------------------------------------

qc_results <- dat %>%
  select(all_of(c(likelihood_vars, severity_vars))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "response"
  ) %>%
  count(
    variable,
    response,
    name = "n"
  ) %>%
  complete(
    variable = c(likelihood_vars, severity_vars),
    response = 1:5,
    fill = list(n = 0)
  ) %>%
  arrange(variable, response)

qc_out_of_range <- dat %>%
  select(all_of(c(likelihood_vars, severity_vars))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "response"
  ) %>%
  filter(
    !is.na(response),
    response < 1 | response > 5
  ) %>%
  count(
    variable,
    response,
    name = "n"
  )

qc_out_of_range
# no observations is a good thing.

# keep as a new dataframe now that cleaning is done
sport_data <- dat


qc_summary <- sport_data %>%
  summarise(
    across(
      all_of(c(likelihood_vars, severity_vars)),
      list(
        n = ~ sum(!is.na(.x)),
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("variable", ".value"),
    names_pattern = "(.+)_(n|min|max)$"
  )

qc_summary

write.csv(sport_data, file = "sport data 8-25-2026.csv")
