# load packages
source("packagesused.R")
options(shiny.maxRequestSize=80*1024^2)
# load modules
modules <- list("mod_sideBar","utils_tab_body_render","mod_body","mod_loanlimits", "mod_trx_monitoring",
                "mod_overdrawn_accounts","mod_loanlimits_utils_loanlimits","mod_disbinputs")
modules %>% map(~source(glue("R/{.}.R")))
