# The script for cleaning the data

# 1. Find outliers in the data
boxplot(all_shoots[,35])

# 2. Find "wird" patterns in the data
# To be honest, I have no idea how to deal with this right now...
# For chaerophyllum roseum:
pl_k_b <- read.csv("../PL-K-B_all_years.csv")

# Looking for mistakes
ch_ros_I_2 <- pl_k_b %>% filter(species == ".Chaerophyllum roseum") %>%
  select(grep("I_2", names(ch_ros)))
# Find the mistakes about Umbellifera family members at sample square I_2
I_2 <- pl_k_b %>%  select(species, state, grep("^I_2", names(pl_k_b))) %>%
  group_by(species, state) %>% summarise(c_across(where(is.numeric)))

I_2 <- pl_k_b %>%  select(species, state, grep("^I_2", names(pl_k_b))) %>%
  rowwise(species, state) %>% mutate(sum = sum(c_across(where(is.numeric))))
I_2 <- I_2 %>% select(species, state, sum) %>% filter(sum > 0)
plot(I_2$sum)

# Find out when this species was post into the table by mistake:
# in 2019
# maybe in 2015 (check who counted the shoots!)
# maybe in 2013 (check who counted the shoots!)
# in 2010, 2011, 2012, 2009
ses_alp <- pl_k_b %>% filter(species == ".Seseli alpinum") %>%
  select(grep("^I_2", names(pl_k_b))) %>% select_if(~is.numeric(.) & sum(., na.rm=TRUE) > 0)

