# Load packages
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

# Import Excel file
sport_data <- read_excel("sport data 7-23-2026.xls")
sport_data <- sport_data %>% drop_na(StartDate)

sport_data <- sport_data %>%
  mutate(
    sport_role = factor(
      sport_role,
      levels = c(1, 2, 3, 4),
      labels = c(
        "Administrator",
        "Coach, Athletic Trainer,\nor Fitness Professional",
        "Athlete",
        "Other\n(Sports Official,\nMedical Staff)"
      )
    ),
    
    sport_level = factor(
      sport_level,
      levels = c(1, 2, 3, 4),
      labels = c(
        "Youth",
        "Adult sports/rec",
        "Elite or collegiate",
        "Other"
      )
    )
  )

#----------------------------------------
# Identify likelihood and severity variables
#----------------------------------------

likelihood_vars <- names(sport_data) %>%
  str_subset("likelihood")

severity_vars <- names(sport_data) %>%
  str_subset("severity")

# Remove suffixes to obtain the base variable names
likelihood_base <- str_remove(likelihood_vars, "_likelihood.*")
severity_base   <- str_remove(severity_vars, "_severity.*")

items <- intersect(likelihood_base, severity_base)
out <- symdiff(likelihood_base, severity_base)
## remove climate variables
filter_items <- items[!items %in% c("risingtemps", "extremeheat", "monsoon", "airq", "drought")]

# likelihood_vars <- paste0(filter_items, "_likelihood")
# severity_vars   <- paste0(filter_items, "_severity")

#----------------------------------------
# Convert to long format
#----------------------------------------


long_df <-
  sport_data %>%
  pivot_longer(
    cols = c(all_of(likelihood_vars), all_of(severity_vars)),
    names_to = c("question", ".value"),
    names_pattern = "(.*)_(likelihood|severity)"
  ) %>%
  mutate(
    risk_score = likelihood * severity
  )

library(dplyr)

long_df <- long_df %>%
  mutate(
    domain = case_when(
      question %in% c(
        "risingtemps", "extremeheat", "monsoon", "airq", "drought"
      ) ~ "Climate",
      
      question %in% c(
        "adjusttime", "changelocation", "adjustseason",
        "planning", "hiring"
      ) ~ "Scheduling & Management",
      
      question %in% c(
        "facilitychange", "indoors", "equipment",
        "lighting", "flooddamage", "badturf", "wildfire"
      ) ~ "Infrastructure",
      
      question %in% c(
        "cancel", "insurance", "summer",
        "repair", "participate"
      ) ~ "Finances",
      
      question %in% c(
        "frustrated", "lowparticipation", "comfort",
        "people_6", "social", "tradition"
      ) ~ "Participants & Spectators",
      
      question %in% c(
        "health_athletes_10",
        "health_athletes_11",
        "health_athletes_12",
        "health_athletes_14",
        "health_athletes_15",
        "health_athletes_heatinjury",
        "health_athletes_vulnerable",
        "health_athletes_17",
        "health_athletes_18"
      ) ~ "Athlete Health & Safety",
      
      question %in% c(
        "health_spectator_climate",
        "health_spectators-vulnerable",
        "health_spectators_med_staff"
      ) ~ "Spectator Health & Safety",
      
      TRUE ~ NA_character_
    )
  )

long_df_cc <-
  long_df %>%
  filter(domain == "Climate")

long_df_nocc <-
  long_df %>%
  filter(domain != "Climate")

sport_level_summary_cc <-
  long_df_cc %>%
  group_by(sport_level, question) %>%
  summarise(
    likelihood = mean(likelihood, na.rm = TRUE),
    severity = mean(severity, na.rm = TRUE),
    risk_score = mean(risk_score, na.rm = TRUE),
    .groups = "drop"
  )

role_summary_cc <-
  long_df_cc %>%
  group_by(sport_role, question) %>%
  summarise(
    likelihood = mean(likelihood, na.rm = TRUE),
    severity = mean(severity, na.rm = TRUE),
    risk_score = mean(risk_score, na.rm = TRUE),
    .groups = "drop"
  )

sport_level_summary_nocc <-
  long_df_nocc %>%
  group_by(sport_level, domain) %>%
  summarise(
    likelihood = mean(likelihood, na.rm = TRUE),
    severity = mean(severity, na.rm = TRUE),
    risk_score = mean(risk_score, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

role_summary_nocc <-
  long_df_nocc %>%
  group_by(sport_role, domain) %>%
  summarise(
    likelihood = mean(likelihood, na.rm = TRUE),
    severity = mean(severity, na.rm = TRUE),
    risk_score = mean(risk_score, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )


# 
# 
# #----------------------------------------
# # Summary by sport level
# #----------------------------------------
# 
# sport_level_summary <-
#   long_df_cc %>%
#   group_by(sport_level, question) %>%
#   summarise(
#     likelihood = mean(likelihood, na.rm = TRUE),
#     severity   = mean(severity, na.rm = TRUE),
#     risk_score = mean(risk_score, na.rm = TRUE),
#     n = sum(!is.na(risk_score)),
#     .groups = "drop"
#   )
# 
# #----------------------------------------
# # Summary by role
# #----------------------------------------
# 
# role_summary <-
#   long_df_cc %>%
#   group_by(sport_role, question) %>%
#   summarise(
#     likelihood = mean(likelihood, na.rm = TRUE),
#     severity   = mean(severity, na.rm = TRUE),
#     risk_score = mean(risk_score, na.rm = TRUE),
#     n = sum(!is.na(risk_score)),
#     .groups = "drop"
#   )
# 
# # View results
# sport_level_summary
# role_summary



#### sport and roles for just the climate change variables #### 
library(ggplot2)
library(forcats)
library(RColorBrewer)

sport_level_summary_cc %>%
  mutate(
    question = fct_reorder(question, risk_score, .fun = mean)
  ) %>%
  ggplot(aes(
    x = sport_level,
    y = question,
    fill = risk_score
  )) +
  geom_tile(color = "white") +
  scale_fill_distiller(
    palette = "YlOrRd",
    direction = 1,
    limits = c(0, 25),
    breaks = seq(0, 25, by = 5),
    labels = seq(0, 25, by = 5),
    name = "Risk\nScore",
    oob = scales::squish
  ) +
  labs(
    x = "Sport level",
    y = NULL,
    title = "Climate change risk score by sport level"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.y = element_text(size = 8)
  )

role_summary_cc %>%
  mutate(
    question = fct_reorder(question, risk_score, .fun = mean)
  ) %>%
  ggplot(aes(
    x = sport_role,
    y = question,
    fill = risk_score
  )) +
  geom_tile(color = "white") +
  scale_fill_distiller(
    palette = "YlOrRd",
    direction = 1,
    limits = c(0, 25),
    breaks = seq(0, 25, by = 5),
    labels = seq(0, 25, by = 5),
    name = "Risk\nScore",
    oob = scales::squish
  ) +
  labs(
    x = "Role",
    y = NULL,
    title = "Climate change risk score by role"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.y = element_text(size = 8)
  )



sport_level_summary_nocc <-
  long_df_nocc %>%
  group_by(sport_level, domain) %>%
  summarise(
    likelihood = mean(likelihood, na.rm = TRUE),
    severity   = mean(severity, na.rm = TRUE),
    risk_score = mean(risk_score, na.rm = TRUE),
    n = sum(!is.na(risk_score)),
    .groups = "drop"
  )

#----------------------------------------
# Summary by role
#----------------------------------------

role_summary_nocc <-
  long_df_nocc %>%
  group_by(sport_role, domain) %>%
  summarise(
    likelihood = mean(likelihood, na.rm = TRUE),
    severity   = mean(severity, na.rm = TRUE),
    risk_score = mean(risk_score, na.rm = TRUE),
    n = sum(!is.na(risk_score)),
    .groups = "drop"
  )



#### sport and roles for everything else #### 
sport_level_summary_nocc %>%
  mutate(
    domain = fct_reorder(domain, risk_score, .fun = mean)
  ) %>%
  ggplot(aes(
    x = sport_level,
    y = domain,
    fill = risk_score
  )) +
  geom_tile(color = "white") +
  scale_fill_distiller(
    palette = "YlOrRd",
    direction = 1,
    limits = c(0, 25),
    breaks = seq(0, 25, by = 5),
    labels = seq(0, 25, by = 5),
    name = "Risk\nScore",
    oob = scales::squish
  ) +
  labs(
    x = "Sport level",
    y = NULL,
    title = "Domain risk score by sport level"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.y = element_text(size = 8)
  )





role_summary_nocc %>%
  mutate(
    domain = fct_reorder(domain, risk_score, .fun = mean)
  ) %>%
  ggplot(aes(
    x = sport_role,
    y = domain,
    fill = risk_score
  )) +
  geom_tile(color = "white") +
  scale_fill_distiller(
    palette = "YlOrRd",
    direction = 1,
    limits = c(0, 25),
    breaks = seq(0, 25, by = 5),
    labels = seq(0, 25, by = 5),
    name = "Risk\nScore",
    oob = scales::squish
  ) +
  labs(
    x = "Role",
    y = NULL,
    title = "Domain risk score by role"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.y = element_text(size = 8)
  )


