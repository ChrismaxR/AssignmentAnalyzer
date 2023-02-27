library(tidyverse)

# Get data ----------------------------------------------------------------

latest_compiled_parsed_set <- fs::dir_ls(here::here("data"), regexp = "_compiled_parsed_output.rds$") |> 
  enframe() |> 
  arrange(desc(name)) |> 
  slice(1) |> 
  pull(value)

# all the data (insigthly & email data)compiled into one file after gpt throughput and manual cleaning 
compiled_parsed_output <- read_rds(latest_compiled_parsed_set) |> 
  tidylog::mutate(
    broker = map(
    .x = extracted_broker, 
    .f = \(x) pluck(x, "result")
    ), 
    broker = coalesce(as.character(broker), broker_in_organisation)
  ) |> 
  select(-extracted_email, -extracted_broker, -broker_in_organisation)


# Output share file -------------------------------------------------------

share_file <- compiled_parsed_output |> 
  transmute(
    id, 
    date,
    body, 
    result,
    job_title,
    job_title_derived,
    organisation, 
    organisation_derived,
    location,
    location_derived,
    experience = required_years_of_experience,
    experience_derived,
    eduction = required_education_level,
    education_derived,
    required_skills,
    certification = required_certification,
    certification_derived,
    hours = working_hours,
    hours_derived,
    job_duration,
    hourly_rate,
    hourly_rate_derived, 
    broker
  ) 

# writexl::write_xlsx(share_file, here::here("output", str_c(BAutils::dater(Sys.Date()), "_compiled_parsed_data.xlsx")))
# write_csv(share_file, here::here("output", str_c(BAutils::dater(Sys.Date()), "_compiled_parsed_data.csv")))


stats <- list(
  aanvragen = length(unique(share_file$id)), 
  brokers = length(unique(share_file$broker)),
  jobs_titles = length(unique(share_file$job_title)),
  organisations = length(unique(share_file$organisation)),
  organisations = length(unique(share_file$organisation_derived))
  
)


# Graphing ----------------------------------------------------------------


simple_labels <- c(
  broker = "Broker", 
  certification_derived = "Certificaten",
  education_derived     = "Opleidingsniveau",
  experience_derived    = "Ervaring",
  hours_derived         = "Uren per week", 
  job_title_derived     = "Functietitel", 
  location_derived      = "Locatie", 
  organisation_derived  = "Industrie"
)

# Univariate analysis: check out distributions of all derived columns
compiled_parsed_output |> 
  select(
    id,
    job_title_derived, 
    organisation_derived,
    location_derived,
    education_derived, 
    experience_derived,
    hours_derived, 
    certification_derived, 
    broker
  ) |>
  tidylog::mutate(
    location_derived = fct_lump(location_derived, n = 15, other_level = "Anders"),
    broker = fct_lump(broker, n = 15, other_level = "Anders"),
    hours_derived = fct_lump(hours_derived, n = 6, other_level = "Anders")
  ) |> 
  pivot_longer(cols = 2:9) |> 
  count(name, value) |> 
  #na.omit() |> 
  ggplot(aes(y = tidytext::reorder_within(x = value, by = n, within = name), x = n)) +
  geom_col(fill = "#4579D6") +
  tidytext::scale_y_reordered() +
  facet_wrap(
    ~ name, 
    scales = "free", 
    labeller = labeller(name = simple_labels)
  ) +
  BAutils::gg_theme_ba1() +
  labs(
    title = "Univariate analyse", 
    subtitle = "",
    x = "# Aanvragen", 
    y = NULL
  )


# Univariate analysis: distribution of Hourly rates

share_file |> 
  select(
    hourly_rate_derived
  ) |> 
  filter(str_detect(hourly_rate_derived, "[:digit:]")) |> 
  transmute(rate = as.numeric(hourly_rate_derived)) |>
  filter(between(rate, 25, 250)) |> 
  ggplot(aes(x = rate)) +
  geom_histogram(bins = 25, fill = "#4579D6") +
  scale_x_continuous(labels = scales::dollar_format(accuracy = 1L, prefix = "€ ", big.mark = ".", decimal.mark = ",")) +
  BAutils::gg_theme_ba1() +
  labs(
    title = "Verdeling Hourly rates", 
    subtitle = "",
    x = "Hourly rate in Euro's", 
    y = "Aantal aanvragen"
  )
  

# Univariate analysis: distribution of skills

skills_count <- compiled_parsed_output |> 
  mutate(
    required_skills = map(
      .x = str_split(pattern = "\\,\\s", string = required_skills), 
      .f = enframe
    )
  ) |>
  unnest_longer(col = required_skills) |> 
  transmute(
    id,
    job_title_derived,
    organisation_derived,
    skills = str_to_lower(required_skills$value), 
    skills_derived = case_when(
      str_detect(skills, "communicat|coachen|begeleiden|facilitation|faciliteren") == T ~ "Faciliteren",
      str_detect(skills, "analytical|analytisch|analyse|analysis|analyzing") == T ~ "Analytisch denken",
      str_detect(skills, "business") == T ~ "Business Analyse",
      str_detect(skills, "scrum|agile|safe|features|stories|epics") == T ~ "Agile/Scrum",
      str_detect(skills, "collaboration|samenwerken|teamwork|bruggen") == T ~ "Samenwerken",
      str_detect(skills, "data|science|^bi|gegevens") == T ~ "Data",
      str_detect(skills, "behoeften|requirements|eisen") == T ~ "Requirements management",
      str_detect(skills, "c\\+\\+|c\\#|.net|python|java|script|kafka|sql|git|^r$") == T ~ "Programmeren",
      str_detect(skills, "creative|creatief|creëren") == T ~ "Creativiteit",
      str_detect(skills, "overheid|bestuurlijk|sensitiviteit") == T ~ "Overheid",
      str_detect(skills, "process|ontwerp|architect|enterprise") == T ~ "Architectuur/Processmanagement",
      str_detect(skills, "functioneel|functional|functionele") == T ~ "Function",
      str_detect(skills, "verander|change") == T ~ "Verandermanagement",
      str_detect(skills, "test|unit testing") == T ~ "Testen",
      str_detect(skills, "advies|consultancy|strategy|strategie|strategisch|strategic") == T ~ "Consultancy",
      str_detect(skills, "lean|belt|six sigma") == T ~ "Lean",
      str_detect(skills, "problem|probleem") == T ~ "Probleemananlyse",
      str_detect(skills, "product|owner") == T ~ "Productmanagement/owner",
      str_detect(skills, "project") == T ~ "Projectmanagement",
      str_detect(skills, "stakeholer") == T ~ "Stakeholdermanagement",
      T ~ skills
    )
  ) 

top_15_skills <- skills_count |> 
  count(skills_derived, sort = T) |> 
  head(15) |> 
  pull(skills_derived)

skills_count |> 
  tidylog::filter(skills_derived %in% top_15_skills) |> 
  group_by(id) |> 
  count(skills_derived) |> 
  ungroup() |> 
  group_by(skills_derived) |> 
  summarise(
    skill_count = sum(n)
  ) |> 
  ungroup() |> 
  mutate(
    perc = skill_count/nrow(compiled_parsed_output)
  ) |> 
  ggplot(aes(y = fct_reorder(skills_derived, perc), x = perc)) +
  geom_col(fill = "#4579D6") +
  geom_text(aes(label = scales::percent(perc, accuracy = .1)), colour = "white", nudge_x = -.03, size = 3.5) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1L)) +
  BAutils::gg_theme_ba1() +
  labs(
    title = "Gevraagde skills - Meest voorkomende skills",
    x = "% Aanvragen",
    y = NULL
  )

# Multivariate analysis: cross referencing job_title, organisation with skills 

multiple_cats_multivariate <- compiled_parsed_output |> 
  transmute(
    id,
    job_title_derived, 
    organisation_derived,
    broker,
    required_skills = map(
      .x = str_split(pattern = "\\,\\s", string = required_skills), 
      .f = enframe
    )
  ) |> 
  unnest_longer(col = required_skills) |> 
  mutate(
    skills = str_to_sentence(required_skills$value)
  ) 

# Need to complete this graph, not yet done...
multiple_cats_wrangle |> 
  ggplot(aes(y = value, x = n)) +
  geom_col() +
  facet_wrap(~ var, scales = 'free')


# Impute hourly rates -----------------------------------------------------

# D.d. 2023-02-17 I find the following breakdown in hourly rates:

share_file |> 
  transmute(
    hourly_rate_original = str_to_lower(hourly_rate),
    market_conformity = if_else(str_detect(str_to_lower(hourly_rate), "market|markt|conform|mc") == T, T, F),
    buckets = case_when(
      str_detect(str_to_lower(hourly_rate), "[:digit:]{2,}") == T ~ "Prijs bekend",
      str_detect(str_to_lower(hourly_rate), "market|markt|conform|mc") == T ~ "Markt Conform",
      str_detect(str_to_lower(hourly_rate), "negotiable|overleg|t.b.d.|n.t.b.|n.o.t.k.|negotiation") == T ~ "Onderhandelbaar",
      T ~ "Onbekend"
    )
  ) |> 
  count(buckets) |>
  mutate(
    perc = n/sum(n)
  ) |> 
  ggplot(aes(y = fct_reorder(buckets, perc), x = perc)) +
  geom_col(fill = "#4579D6") +
  geom_text(aes(label = scales::percent(perc, accuracy = .1)), colour = "white", nudge_x = -.03, size = 3.5) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1L)) +
  BAutils::gg_theme_ba1() +
  labs(
    title = "Uurtarieven in aanvragen",
    x = "% Aanvragen",
    y = NULL
  )


# Can I impute in some way what missing hourly rates could be?

# I think that education, years of experience, industry sector and possibly
# job title influence the market conformity price.

impute_rates <- share_file |> 
  tidylog::mutate(
    hourly_rate_stripped = as.numeric(na_if(str_remove_all(hourly_rate_derived, "[:alpha:]"), ""))
  ) 

impute_rates_labeller <- c(
  education_derived     = "Opleidingsniveau",
  experience_derived    = "Ervaring",
  job_title_derived     = "Functietitel", 
  organisation_derived  = "Industrie"
)

# On the basis of this dist
impute_rates |> 
  select(
    education_derived, 
    experience_derived, 
    job_title_derived, 
    organisation_derived, 
    hourly_rate_stripped
  ) |> 
  tidylog::filter(between(hourly_rate_stripped, 45, 200)) |> 
  pivot_longer(cols = 1:4) |> 
  ggplot(aes(y = value, x = hourly_rate_stripped)) +
  geom_boxplot() +
  geom_jitter(colour = "#4579D6", alpha = 0.7) +
  BAutils::gg_theme_ba1() +
  facet_wrap(~ name, scales = "free", labeller = labeller(name = impute_rates_labeller)) +
  labs(
    title = "Distributie Uurtarieven over 4 verschillende kenmerken",
    x = NULL,
    y = NULL
  )
  
