### R in Production 
### Tyler Sanders | ty.sanders1997@gmail.com | https://www.linkedin.com/in/tyler-sanders-8275b5150/
### https://github.com/ty-sanders/political_ds_au_2022


#Tests
print("Hello World")
print(Sys.getenv("message"))

# Load Libraries
library(tidyverse)
library(aws.s3)
library(kableExtra)
library(janitor)
library(slackr)
library(googlesheets4)

# Connect to AWS
access_key_id      <- config::get(config = "redoak-retriever", value =     "access_key_id")
secret_access_key  <- config::get(config = "redoak-retriever", value = "secret_access_key")
aws_default_region <- config::get(config = "redoak-retriever", value =    "default_region")

Sys.setenv("AWS_ACCESS_KEY_ID"     =     access_key_id)
Sys.setenv("AWS_SECRET_ACCESS_KEY" = secret_access_key)
Sys.setenv("AWS_DEFAULT_REGION"    = aws_default_region)



#Connect to Slack
slackr_setup(channel              = "@tyler", 
             username             = config::get(config = "slackr_tyler_bot", value =  "username"),
             token                = config::get(config = "slackr_tyler_bot", value =  "token"),
             incoming_webhook_url = config::get(config = "slackr_tyler_bot", value =  "incoming_webhook_url"))

# Connect to Google Sheets
#url to project crosswalk
gs4_auth(path = rawToChar(aws.s3::get_object("s3://ros-analytics-secrets/ROS-Data-Team-Google-Bot.json")))

google_sheet_path  <- "https://docs.google.com/spreadsheets/d/1IN5LTDoweEBBcHRUFI64_eslLy2gBxNFBL2Q_kKB4GE"

user_input <- googlesheets4::read_sheet(google_sheet_path) %>% 
  clean_names()
 

penguins_raw <- palmerpenguins::penguins

penguins_clean <- penguins_raw %>% 
  rename_with(str_to_lower) %>% 
  janitor::clean_names() %>% 
  #na.omit() %>% 
  filter(! is.na(species)) %>%
  mutate(sex = case_when(is.na(sex) ~ "unknown",
                         TRUE       ~ as.character(sex))) %>%
  mutate(bill_length_cat = case_when(bill_length_mm > 50 ~ "Long",
                                     bill_length_mm > 40 ~ "Medium",
                                     TRUE                ~ "Short"))

# Glimpse is incredibly useful and can be used on arrow and dbplyr objects too!
glimpse(penguins_clean)

# Tabyl is the easiest eda function around
penguins_clean %>% 
  janitor::tabyl(year)



penguins_summary <- penguins_clean %>% 
  group_by(species, island, sex) %>% 
  summarise(bill_length    = round_half_up(mean(bill_length_mm,    na.rm = TRUE), digits = 2),
            bill_depth     = round_half_up(mean(bill_depth_mm,     na.rm = TRUE), digits = 2),
            flipper_length = round_half_up(mean(flipper_length_mm, na.rm = TRUE), digits = 2)) %>% 
  mutate(sex = str_to_title(sex))

penguins_summary %>% 
  select(-species) %>% 
  kableExtra::kbl(co.names = c("Species", "Island", "Sex", "Bill length", "Bill Depth", "Flipper Length")) %>% 
  kable_paper()


index <- penguins_summary %>% 
  group_by(species) %>% 
  count() %>% 
  deframe()


penguins_summary %>%
  ungroup() %>%
  select(-species) %>%
  arrange(desc(bill_length)) %>%
  kableExtra::kbl(col.names = c("Island", "Sex", "Bill length", "Bill Depth", "Flipper Length")) %>%
  kable_paper() %>%
  group_rows(group_label = "Species", index = index) %>%
  row_spec(0, bold = TRUE) %>%
  column_spec(4, background = spec_color(penguins_summary$bill_length, end = 0.7), color = "white", bold = TRUE) %>%
  add_header_above(c("Groupings" = 2, "Average Feature Values" = 3)) %>%
  add_header_above(c("Palmer's Penguins" = 5), bold = TRUE) %>%
  footnote(general = "Values in MM")

# For more on kableExtra: https://haozhu233.github.io/kableExtra/awesome_table_in_pdf.pdf

get_param <- function(selected_parameter){
  user_input %>% filter(paramaters %in% selected_parameter) %>% pull(values)
}

slackr_msg(paste0("*Palmer's Penguins: ", get_param("penguin_type"), " Penguins with Bill Length > ", get_param("min_bill_length"), "*"))

penguins_summary %>% 
  filter(species %in% get_param("penguin_type")) %>% 
  filter(bill_length > get_param("min_bill_length")) %>% 
  arrange(desc(bill_length)) %>% 
  pander::pandoc.table.return(., style = "multiline") %>%
  slackr_msg(txt = .)








## People you should follow on the internet (twitter or whatever comes after twitter)
# Hadley 
# Julia Silge
# Jenny Bryan
# Thomas Mock
# Thomas Lin Pederson
# Cara Thompson
# Emily Riederer
# Kyle Ciulla
# Matt Worthington
# David Keyes
# Asmae Toumi
# Kyle Walker
# David Robinson 

