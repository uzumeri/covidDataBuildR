library(tidyverse)
library(aws.s3)
library(data.table)

# bucketlist()
setwd("~/SafeGraph")

files <- list.files(pattern = "social-distancing.*", recursive=T)

f <- files[str_detect(files,"^social-distancing/v2.*") == TRUE]

collect <- fread(f[1]) %>% filter(str_detect(origin_census_block_group, "^13.*"))

for (currentfile in f[2:length(f)]) {
    collect <- collect %>% bind_rows(fread(currentfile) %>% filter(str_detect(origin_census_block_group, "^13.*")))
}
# s3saveRDS(x = collect, bucket = "rds.fluidprojects.com", object = "socialdistancev2.rds")



