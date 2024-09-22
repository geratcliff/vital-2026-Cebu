library(tidyverse)
library(MortalityLaws)
library(here)
library(glue)

countries = c("Bangladesh", "Cambodia", "Cameroon", "China", "Ethiopia", "India", "Kenya", "Mozambique", "Philippines", "Rwanda", "South Africa", "Sri Lanka", "Thailand", "Zimbabwe", "United Republic of Tanzania", "Uganda", "Zambia")

 cbd_life_tables <- c(
     "https://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_GBD_2019_LIFE_TABLES_1950_2019_ID_28_0.zip",
     "https://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_GBD_2019_LIFE_TABLES_1950_2019_ID_5_0.zip",
     "https://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_GBD_2019_LIFE_TABLES_1950_2019_ID_6_0.zip",
     "https://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_GBD_2019_LIFE_TABLES_1950_2019_ID_7_0.zip",
     "https://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_GBD_2019_LIFE_TABLES_1950_2019_ID_8_0.zip",
     "https://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_GBD_2019_LIFE_TABLES_1950_2019_ID_9_0.zip",
     "https://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_GBD_2019_LIFE_TABLES_1950_2019_ID_10_0.zip",
     "https://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_GBD_2019_LIFE_TABLES_1950_2019_ID_11_0.zip",
     "https://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_GBD_2019_LIFE_TABLES_1950_2019_ID_12_0.zip",
     "https://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_GBD_2019_LIFE_TABLES_1950_2019_ID_13_0.zip",
     "https://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_GBD_2019_LIFE_TABLES_1950_2019_ID_14_0.zip",
     "https://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_GBD_2019_LIFE_TABLES_1950_2019_ID_15_0.zip",
     "https://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_GBD_2019_LIFE_TABLES_1950_2019_ID_16_0.zip",
     "https://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_GBD_2019_LIFE_TABLES_1950_2019_ID_17_0.zip",
     "https://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_GBD_2019_LIFE_TABLES_1950_2019_ID_18_0.zip",
     "https://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_GBD_2019_LIFE_TABLES_1950_2019_ID_19_0.zip",
     "https://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_GBD_2019_LIFE_TABLES_1950_2019_ID_20_0.zip",
     "https://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_GBD_2019_LIFE_TABLES_1950_2019_ID_30_0.zip",
     "https://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_GBD_2019_LIFE_TABLES_1950_2019_ID_31_0.zip",
     "https://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_GBD_2019_LIFE_TABLES_1950_2019_ID_32_0.zip",
     "https://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_GBD_2019_LIFE_TABLES_1950_2019_ID_33_0.zip",
     "https://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_GBD_2019_LIFE_TABLES_1950_2019_ID_44_0.zip",
     "https://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_GBD_2019_LIFE_TABLES_1950_2019_ID_45_0.zip",
     "https://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_GBD_2019_LIFE_TABLES_1950_2019_ID_148_0.zip"
 )

 # cbd_life_tables %>% map(~({
 #     cat(glue::glue("File: {.x}\n"))
 #     tmp <- tempfile()
 #     tmpdir <- tempdir()
 #     download.file(.x, paste0(tmpdir,"/temp.zip"))
 #     nn <- gsub("https://ghdx.healthdata.org/sites/default/files/record-attached-files/","",.x) %>% gsub(".ZIP|.zip","",.)
 #     unzip(paste0(tmpdir,"/temp.zip"))
 #     ff <- list.files(here(),pattern = ".CSV|.csv")
 #     files <- ff[grep("WSHOCK",ff)] %>% map(~(read.csv(.x) %>% as_tibble() %>% mutate(file = .x))) %>% pluck(1)
 #     write_rds(files,file = glue::glue("_gbd_lifetables/{nn}"))
 #     file.remove(ff)
 # }))

 radix = 100000

 gbd_lt_ <-
     list.files(here("_gbd_lifetables"), pattern = "IHME") %>%
     map_df(~({
         read_rds(here(glue("_gbd_lifetables/{.x}")))
     }))

 age_group_lut <- c("28" = 0, "5" = 5, "6" = 10, "7" = 15, "8" = 20,
                    "9" = 25, "10" = 30, "11" = 35, "12"= 40, "13" = 45,
                    "14" = 50, "15" = 55, "16" = 60, "17"=65, "18"=70,
                    "19" = 75, "20" = 80, "30"= 85,"31" = 90,"32" = 95, "33" = 100,
                    "44"=105,"45" = 110,"148" = 111)

 gbd_lt <-
     gbd_lt_ %>%
     filter(year_id==2019) %>%
     group_by(location_name,location_id) %>%
     nest()  %>%
     mutate(life_table = map(data,~({
         .x %>% select(sex_name,age_group_id,age_group_name,measure_name,val) %>%
             mutate(age = age_group_lut[paste0(age_group_id)]) %>%
             spread(measure_name,val) %>%
             janitor::clean_names() %>%
             rename(ex = life_expectancy,
                    qx = probability_of_death) %>%
             arrange(sex_name,age) %>%
             select(sex_name,age,ex,qx) %>%
             gather(measure,value,-sex_name,-age) %>%
             mutate(measure = paste0(measure,gsub("_both","",paste0("_",sex_name)))) %>%
             select(-sex_name)  %>%
             spread(measure,value) %>%
             mutate(p = 1 - qx,
                    p_male = 1 - qx_male,
                    p_female = 1 - qx_female) %>%
             mutate(lx = radix * cumprod(c(1,p[-nrow(.)])),
                    lx_male = radix * cumprod(c(1,p_male[-nrow(.)])),
                    lx_female = radix * cumprod(c(1,p_female[-nrow(.)])))

     })))
 
 gbd_lt2 <- countries %>% map(~({
   gbd_lt %>% filter(grepl(tolower(paste0(.x,"")),tolower(location_name)))
 })) %>% 
   bind_rows() %>% 
   filter(!location_name == "Indiana" & !location_name=="Taiwan (Province of China)") 
 
 setdiff(countries,gbd_lt2$location_name)
 
 gbd_lt2 <-
     gbd_lt2 %>%
     mutate(hp_mort = map(life_table,~({
         ages     <- .x$age
         deaths   <- .x$lx * .x$qx
         exposure <- .x$lx

         mort_fit <- MortalityLaw(
             x  = ages,
             Dx  = deaths,   # vector with death counts
             Ex  = exposure, # vector containing exposures
             law = "HP",
             opt.method = "LF2")
         mort_fit
         #coef(mort_fit) %>% data.frame() %>% t()
     })))
 
 
 
 gbd_lt2 %>% mutate(p_death = map(hp_mort,~({
   hp_ <-   .x
   tibble(index = 0:111, p_die = approxfun(names(.x$fitted.values), .x$fitted.values )(0:111))
 })))  %>% 
   unnest(cols = p_death) %>% 
   select(location_name,index, p_die) %>% 
   group_by(location_name) %>% 
   nest() %>% mutate(tmp = map2(data,location_name, ~({
     name = paste0(gsub(" ","_",tolower(.y)),".csv")
     x <- tibble(.x) %>% ungroup() %>% select(index,p_die) 
     rownames(x) = NULL
     cat(paste0("https://graveja0.github.io/vital-istanbul-2024/_gbd_lifetables/output/",name))
     cat("\n")
     x %>% data.table::fwrite(here(paste0("_gbd_lifetables/output/",name)))
     
     
   }))) %>% 
   unnest(cols = c(tmp))
 

 gbd_lt2 %>% mutate(le = map(hp_mort,~({
   hp_ <-   .x
   p_ <- tibble(index = 0:111, p_die = approxfun(names(.x$fitted.values), .x$fitted.values )(0:111))
   s_ <- c(1,0) 
   tr <- 0:111 %>% map(~({
     px <- p_[.x+1,]$p_die
     P = matrix(c(1-px,0,px,1),nrow=2,ncol=2, dimnames = list(c("A","D"),c("A","D")))
     s_ <<- s_ %*% P
     data.frame(s_)
   })) %>% 
     bind_rows() %>% 
     as.matrix()
   hc <- rep(1,length(0:111))
   hc[1] = 0.5
   hc[length(hc)] = .5
   le <- sum((tr %*% c(1,0))*hc)
   cat(paste0("Life Expectanccy ",round(le,2),"\n"))
   as.numeric(le)
 }))) %>% 
   unnest(cols = c(le))
 