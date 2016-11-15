setwd("/Users/patty/Documents/Dlab/dlab_workshops/rmapsByYear")
dir()
library(tidyr)

d <- read.csv("data/Frank_WTID_2013_top1_long.csv", stringsAsFactors = FALSE)
head(d)
?spread
dw <- spread(d,Year,Top1_adj)
head(dw)

dw <- subset(dw, State != 'United States')
unique(dw$State)

dw2long <- gather(dw,year,vals,2:length(dw)) 
head(dw2long)

write.csv(dw,file="Frank_WTID_2013_top1_wide.csv", row.names = FALSE)
write.csv(dw2long,file="Frank_WTID_2013_top1_long.csv", row.names = FALSE)

x <- read.csv("https://raw.githubusercontent.com/dlab-geo/rmapsByYear/master/data/Frank_WTID_2013_top1_long.csv")
