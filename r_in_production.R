#install.packages("palmerpenguins") #alternative r test data to iris

library(tidyverse)
library(palmerpenguins) # For More on Palmer Penguines: https://allisonhorst.github.io/palmerpenguins/
library(aws.s3)
library(janitor)


survey_raw <- palmerpenguins::penguins


# AWS 
access_key_id      <- config::get(config = "redoak-retriever", value =     "access_key_id")
secret_access_key  <- config::get(config = "redoak-retriever", value = "secret_access_key")
aws_default_region <- config::get(config = "redoak-retriever", value =    "default_region")

Sys.setenv("AWS_ACCESS_KEY_ID"     =     access_key_id)
Sys.setenv("AWS_SECRET_ACCESS_KEY" = secret_access_key)
Sys.setenv("AWS_DEFAULT_REGION"    = aws_default_region)




project_s3_bucket <- "ros-analytics-datalake-test"
s3_path_raw_data <- "palmer_penguins.csv"

survey_raw <- aws.s3::s3read_using(readr::read_csv,
                                   lazy = FALSE,
                                   object = s3_path_raw_data,
                                   bucket = project_s3_bucket)


# Now I didn't do a good job preparing this data and there are some untidy column names and data
survey_clean <- survey_raw %>% 
  rename_with(str_to_lower) %>% 
  janitor::clean_names() %>% 
# na.omit() %>% 
  filter(! is.na(species)) %>% 
  mutate(sex = case_when(is.na(sex) ~ "unknown",
                         TRUE       ~ sex)) %>% 
  mutate(bill_length_cat = case_when(bill_length_mm > 50 ~ "Long", 
                                     bill_length_mm > 40 ~ "Medium", 
                                     TRUE                ~ "Short"))

glimpse(survey_clean)


survey_clean %>% 
  janitor::tabyl(year)


survey_long <- survey_clean %>% 
  group_by(species, island, sex) %>% 
  summarise(bill_length    = round_half_up(mean(bill_length_mm,    na.rm = TRUE), digits = 2),
            bill_depth     = round_half_up(mean(bill_depth_mm,     na.rm = TRUE), digits = 2),
            flipper_length = round_half_up(mean(flipper_length_mm, na.rm = TRUE), digits = 2)) %>% 
  mutate(sex = str_to_title(sex))

survey_long %>% 
  select(-species) %>% 
  kableExtra::kbl(co.names = c("Species", "Island", "Sex", "Bill length", "Bill Depth", "Flipper Length")) %>% 
  kable_classic_2()


index <- survey_long %>% 
  group_by(species) %>% 
  count() %>% 
  deframe()


survey_long %>% 
  select(-species) %>% 
  arrange(desc(bill_length)) %>% 
  kableExtra::kbl(co.names = c("Island", "Sex", "Bill length", "Bill Depth", "Flipper Length")) %>% 
  kable_classic_2() %>% 
  group_rows(group_label = "Species", index = index) %>% 
  row_spec(0, bold = TRUE) %>% 
  column_spec(4, background = spec_color(survey_long$bill_length, end = 0.7), color = "white", bold = TRUE) %>% 
  add_header_above(c("Groupings" = 3, "Average Feature Values" = 3)) %>% 
  add_header_above(c("Palmer's Penguins" = 6), bold = TRUE) %>% 
  footnote(general = "Values in MM")
  
# For more on kableExtra: https://haozhu233.github.io/kableExtra/awesome_table_in_pdf.pdf










# Redshift Database
# Sys.setenv("ROS_DB_HOST" = config::get(config = "default", value =  "host_url"))
# Sys.setenv("ROS_DB_NAME" = config::get(config = "default", value =   "db_name"))
# Sys.setenv("ROS_DB_USER" = config::get(config = "default", value = "user_name"))
# Sys.setenv("ROS_DB_PW"   = config::get(config = "default", value =  "password"))
# Sys.setenv("ROS_DB_PORT" = config::get(config = "default", value =      "port"))
# 
# 
# 
# rs <- RPostgres::Postgres() %>%
#   DBI::dbConnect(
#     drv       = .,
#     host      = Sys.getenv("ROS_DB_HOST"),
#     dbname    = Sys.getenv("ROS_DB_NAME"),
#     user      = Sys.getenv("ROS_DB_USER"),
#     password  = Sys.getenv("ROS_DB_PW"),
#     port      = Sys.getenv("ROS_DB_PORT"),
#     bigint    = "numeric")





# Example Render Command
build_crosstab_doc <- function(output_dir,
                               output_file,
                               input,
                               toc,
                               number_sections,
                               header,
                               slack_config_file,
                               subtitle,
                               short,
                               question_section,
                               custom_section,
                               demo_section,
                               demos,
                               filtered_section = filtered_section,
                               filtered_section_title = filtered_section_title,
                               filter_logic = filter_logic,
                               project_id,
                               project_s3_bucket,
                               project_s3_prefix,
                               path_crosswalk,
                               by_region,
                               region){
  
  invisible(knitr::knit_meta(clean = TRUE))
  
  rmarkdown::render(
    output_dir = output_dir, 
    output_file = output_file, 
    input = input, 
    params = list(
      subtitle = subtitle,
      short = short,
      question_section = question_section,
      custom_section = custom_section,
      demo_section = demo_section,
      demos = demos,
      filtered_section = filtered_section,
      filtered_section_title = filtered_section_title,
      filter_logic = filter_logic,
      project_id = project_id,
      project_s3_bucket = project_s3_bucket,
      project_s3_prefix = project_s3_prefix,
      path_crosswalk = path_crosswalk,
      by_region = by_region,
      region = region),
  bookdown::pdf_document2(
  latex_engine = "xelatex",
  toc = toc, 
  number_sections = number_sections, includes = list(in_header = header)
))

slackr::slackr_setup(config_file = slack_config_file)
Sys.setenv("SLACK_CHANNEL" = paste0("#target_outputs_", firewall))


write_to_bucket <- function(file_path, folder_name, file_name, bucket_name){
  
  object_key <- paste(folder_name, file_name, sep = "/")
  
  robust_s3_uploader <- purrr::insistently(
    f = ~aws.s3::put_object(file      = file_path,
                            object    = object_key, 
                            bucket    = bucket_name,
                            multipart = TRUE), 
    rate = purrr::rate_backoff(pause_cap = 10, max_times = 20, jitter = TRUE), 
    quiet = TRUE)
  
  response <- robust_s3_uploader()
  
  # Return timestamp if upload is successful
  if(response){
    return(lubridate::now(tzone = "America/New_York"))
  } else {
    return(FALSE)
  }
}

out <- if(by_region %in% TRUE){
  write_to_bucket(file_path = safe_path(paste0(output_dir, "/", output_file)),
                  folder_name = safe_path(paste0(project_s3_prefix, "/reports/survey_deliverables")),
                  file_name   = paste(region, output_file),
                  bucket_name = project_s3_bucket)} else{
                    
                    write_to_bucket(file_path = safe_path(paste0(output_dir, "/", output_file)),
                                    folder_name = safe_path(paste0(project_s3_prefix, "/reports/survey_deliverables")),
                                    file_name   = output_file,
                                    bucket_name = project_s3_bucket)
                  } 

slackr::slackr_upload(filename = safe_path(paste0(output_dir, "/", output_file)),
                      initial_comment  = glue::glue("Survey Crosstabs : ", str_remove(output_file, ".pdf")),
                      title            = glue::glue("Survey Crosstabs : ", str_remove(output_file, ".pdf")),
                      channel          = Sys.getenv("SLACK_CHANNEL"))

slackr::slackr_msg(txt = paste0("Crosstabs Doc Uploaded to S3: ", output_file, ". Timestamp: ", out))

}


build_crosstab_doc(
  output_dir = output_dir,
  output_file = paste0(project_name, " Survey Crosstabs.pdf"),
  input = "~/redoak-modeling-code/templates/crosstabs/crosstabs_production.Rmd", 
  toc = TRUE,
  number_sections = FALSE,
  header = header,
  slack_config_file = "~/redoak-modeling-code/ros_slackr_config",
  subtitle = project_name,
  short = FALSE,
  question_section = TRUE,
  demo_section = TRUE,
  demos = c(                         "countyname"                           = "County",
                                     "dmaname"                              = "DMA",
                                     "race"                                 = "Reported Voterfile Race",
                                     "congressional_district_next_election" = "Congressional District",
                                     "statelegupperdistrict_next_election"  = "State Senate District",
                                     "stateleglowerdistrict_next_election"  = "State House District",
                                     "partisanship_combined"                = "DT Voterfile Modeled Party",
                                     "voter_frequency_general"              = "Voter Frequency General",
                                     "voter_frequency_primary"              = "Voter Frequency Primary",
                                     "voter_generation"                     = "Generation",
                                     "usr_flag"                             = "ROS Modeled Geographic Density",
                                     "college_plus_flag"                    = "ROS Modeled Education"),
  
  filtered_section = FALSE,
  filtered_section_title = c("LIKELY PRIMARY VOTERS"),
  filter_logic = c("voter_frequency_primary >= 2"),
  custom_section = FALSE,
  project_id = project_id,
  project_s3_bucket = project_s3_bucket,
  project_s3_prefix = project_s3_prefix,
  path_crosswalk = path_crosswalk,
  by_region = FALSE,
  region = character(0)
)




