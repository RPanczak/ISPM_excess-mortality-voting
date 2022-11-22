# data retrieval from OSF using osfr package
library(tidyverse)
library(osfr)

# authenticate yourself following instructions on
# https://cran.r-project.org/web/packages/osfr/vignettes/auth.html
# store the token in an osf_pat.txt text file inside secrets directory

osf_auth(read_file("secrets/osf_pat.txt"))

project = osf_retrieve_node("gcxt7")

project

# components
project %>% 
  osf_ls_nodes() 

# folders inside Data component
# files 
project %>%
  osf_ls_nodes() %>%
  filter(name == "Results") %>%
  osf_ls_files() 

# downloading data 
project %>%
  osf_ls_nodes() %>%
  filter(name == "Results") %>%
  osf_ls_files() %>%
  filter(name == "exp_deaths_2020_year.Rds") %>%
  osf_download(path = "results",
               conflicts = "error")

# more info on osfr
# https://cran.r-project.org/web/packages/osfr/vignettes/getting_started.html