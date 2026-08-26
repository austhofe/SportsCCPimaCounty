# Load packages
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(forcats)
library(RColorBrewer)

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


# ============================================================
# 1. CLIMATE CHANGE VARIABLES BY SPORT LEVEL
# ============================================================

sport_level_summary_cc %>%
  filter(!is.na(sport_level)) %>%
  mutate(
    question = fct_reorder(question, risk_score, .fun = mean)
  ) %>%
  ggplot(aes(
    x = sport_level,
    y = question,
    fill = risk_score
  )) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = ifelse(is.na(risk_score), "", round(risk_score, 1))),
    color = "black",
    size = 3
  ) +
  scale_fill_distiller(
    palette = "YlOrRd",
    direction = 1,
    limits = c(0, 25),
    breaks = seq(0, 25, by = 5),
    labels = seq(0, 25, by = 5),
    name = "Risk\nScore",
    oob = scales::squish
  ) +
  scale_y_discrete(labels = c(
    "extremeheat" = "Extreme Heat",
    "risingtemps" = "Rising Temperatures",
    "monsoon" = "Monsoon",
    "airq" = "Air Quality",
    "drought" = "Drought"
  )) +
  labs(
    x = "Sport Level",
    y = NULL,
    title = "Climate Change Risk Score by Sport Level"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.y = element_text(size = 9)
  )


# ============================================================
# 2. CLIMATE CHANGE VARIABLES BY SPORT ROLE
# ============================================================

role_summary_cc %>%
  filter(!is.na(sport_role)) %>%
  filter(sport_role != "Other (Sports Official, Medical Staff)") %>%
  mutate(
    question = fct_reorder(question, risk_score, .fun = mean)
  ) %>%
  ggplot(aes(
    x = sport_role,
    y = question,
    fill = risk_score
  )) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = ifelse(is.na(risk_score), "", round(risk_score, 1))),
    color = "black",
    size = 3
  ) +
  scale_fill_distiller(
    palette = "YlOrRd",
    direction = 1,
    limits = c(0, 25),
    breaks = seq(0, 25, by = 5),
    labels = seq(0, 25, by = 5),
    name = "Risk\nScore",
    oob = scales::squish
  ) +
  scale_y_discrete(labels = c(
    "extremeheat" = "Extreme Heat",
    "risingtemps" = "Rising Temperatures",
    "monsoon" = "Monsoon",
    "airq" = "Air Quality",
    "drought" = "Drought"
  )) +
  scale_x_discrete(labels = c(
    "Coach, Athletic Trainer, or Fitness Professional" = "Coach",
    "Other (Sports Official, Medical Staff)" = "Other"
  )) +
  labs(
    x = "Role",
    y = NULL,
    title = "Climate Change Risk Score by Role"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.y = element_text(size = 9)
  )


# ============================================================
# 3. ALL OTHER DOMAINS BY SPORT LEVEL
# ============================================================

sport_level_summary_nocc %>%
  filter(!is.na(sport_level)) %>%
  mutate(
    domain = fct_reorder(domain, risk_score, .fun = mean)
  ) %>%
  ggplot(aes(
    x = sport_level,
    y = domain,
    fill = risk_score
  )) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = ifelse(is.na(risk_score), "", round(risk_score, 1))),
    color = "black",
    size = 3
  ) +
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
    x = "Sport Level",
    y = NULL,
    title = "Domain Risk Score by Sport Level"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.y = element_text(size = 9)
  )


# ============================================================
# 4. ALL OTHER DOMAINS BY SPORT ROLE
# ============================================================

role_summary_nocc %>%
  filter(!is.na(sport_role)) %>%
  filter(sport_role != "Other (Sports Official, Medical Staff)") %>%
  mutate(
    domain = fct_reorder(domain, risk_score, .fun = mean)
  ) %>%
  ggplot(aes(
    x = sport_role,
    y = domain,
    fill = risk_score
  )) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = ifelse(is.na(risk_score), "", round(risk_score, 1))),
    color = "black",
    size = 3
  ) +
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
    title = "Domain Risk Score by Role"
  ) +
  scale_x_discrete(labels = c(
    "Coach, Athletic Trainer, or Fitness Professional" = "Coach",
    "Other (Sports Official, Medical Staff)" = "Other"
  )) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.y = element_text(size = 9)
  )
