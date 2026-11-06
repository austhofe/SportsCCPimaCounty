library(dplyr)
library(purrr)
library(tidyr)
library(broom)
library(table1)

sport_data <- sport_data %>%
  mutate(
    sport_level = setLabel(sport_level, "Sport Level"),
    sport_role = setLabel(sport_role, "Sport Role"),
    sport = setLabel(sport, "Sport"),
    winter = setLabel(winter, "Winter"),
    summer = setLabel(summer, "Summer"),
    spring = setLabel(spring, "Spring"),
    fall = setLabel(fall, "Fall"),
    year_round = setLabel(year_round, "Year Round")
  )

render_n <- function(x, ...) sum(!is.na(x))
render_missing <- function(x, ...) c("Missing" = sum(is.na(x)))

#Table 1 including descriptive statistics
table1(~ sport + sport_level + sport_role2 + winter + spring + summer+ fall + year_round, sport_data,
       render.continuous = "N",
       render.missing = render_missing)

#Table 2 showing sports by time of year
table1(~ winter + spring + summer+ fall + year_round | sport, sport_data,
       render = render_n,
       transpose = FALSE)

#General stats to see if there are significant differences by sport level or sport role. Only for a few.
kw_results <- map_dfr(
  seq_len(nrow(test_vars)),
  function(i) {
    
    var <- test_vars$variable[i]
    outcome <- test_vars$outcome[i]
    
    # Sport level
    dat_level <- sport_data %>%
      select(sport_level, all_of(var)) %>%
      filter(
        !is.na(sport_level),
        !is.na(.data[[var]])
      )
    
    test_level <- kruskal.test(
      reformulate("sport_level", response = var),
      data = dat_level
    )
    
    # Sport role
    dat_role <- sport_data %>%
      select(sport_role, all_of(var)) %>%
      filter(
        !is.na(sport_role),
        !is.na(.data[[var]])
      )
    
    test_role <- kruskal.test(
      reformulate("sport_role", response = var),
      data = dat_role
    )
    
    bind_rows(
      tibble(
        group = "Sport level",
        outcome = outcome,
        item = test_vars$item[i],
        variable = var,
        n = nrow(dat_level),
        statistic = unname(test_level$statistic),
        df = unname(test_level$parameter),
        p_value = test_level$p.value
      ),
      
      tibble(
        group = "Sport role",
        outcome = outcome,
        item = test_vars$item[i],
        variable = var,
        n = nrow(dat_role),
        statistic = unname(test_role$statistic),
        df = unname(test_role$parameter),
        p_value = test_role$p.value
      )
    )
  }
) %>%
  group_by(group, outcome) %>%
  mutate(
    p_adjusted = p.adjust(p_value, method = "BH")
  ) %>%
  ungroup() %>%
  arrange(group, outcome, p_adjusted)

# Add FDR-adjusted p-values since we're doing so many tests
kw_results <- kw_results %>%
  group_by(group, outcome) %>%
  mutate(
    p_adjusted = p.adjust(p_value, method = "BH")
  ) %>%
  ungroup() %>%
  arrange(group, outcome, p_adjusted)

# filter to only statistically sig results
kw_results_sig <- kw_results %>%
  filter(p_value < 0.05)


