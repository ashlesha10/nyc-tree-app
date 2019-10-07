library(ggplot2)
library(plyr)
library(dplyr)
library(stringr)

##############

problems <- trees_man %>%
  select(root_stone, root_grate, root_other, trunk_wire, trnk_light, trnk_other,
         brch_light, brch_shoe, brch_other)

## Get all problems of trees

problems_total <- trees %>%
  select(root_stone, root_grate, root_other, trunk_wire, trnk_light, trnk_other,
         brch_light, brch_shoe, brch_other)

## Get per borough problems

problems_man <- trees %>%
  filter(borough == "Manhattan") %>%
  select(root_stone, root_grate, root_other, trunk_wire, trnk_light, trnk_other,
         brch_light, brch_shoe, brch_other)

problems_bronx <- trees %>%
  filter(borough == "Bronx") %>%
  select(root_stone, root_grate, root_other, trunk_wire, trnk_light, trnk_other,
         brch_light, brch_shoe, brch_other)

problems_brook <- trees %>%
  filter(borough == "Brooklyn") %>%
  select(root_stone, root_grate, root_other, trunk_wire, trnk_light, trnk_other,
         brch_light, brch_shoe, brch_other)

problems_qu <- trees %>%
  filter(borough == "Queens") %>%
  select(root_stone, root_grate, root_other, trunk_wire, trnk_light, trnk_other,
         brch_light, brch_shoe, brch_other)

problems_st <- trees %>%
  filter(borough == "Staten Island") %>%
  select(root_stone, root_grate, root_other, trunk_wire, trnk_light, trnk_other,
         brch_light, brch_shoe, brch_other)

## Get per borough problem counts

prob_count_man <- c() 

for(i in colnames(problems_man)){
  
  result <- sum(problems_man[[i]] == "Yes")
  
  prob_count_man <- cbind(prob_count_man, result)
}

#

prob_count_bronx <- c() 

for(i in colnames(problems_bronx)){
  
  result <- sum(problems_bronx[[i]] == "Yes")
  
  prob_count_bronx <- cbind(prob_count_bronx, result)
}

#

prob_count_brook <- c() 

for(i in colnames(problems_brook)){
  
  result <- sum(problems_brook[[i]] == "Yes")
  
  prob_count_brook <- cbind(prob_count_brook, result)
}

#

prob_count_qu <- c() 

for(i in colnames(problems_qu)){
  
  result <- sum(problems_qu[[i]] == "Yes")
  
  prob_count_qu <- cbind(prob_count_qu, result)
}

#

prob_count_st <- c() 

for(i in colnames(problems_st)){
  
  result <- sum(problems_st[[i]] == "Yes")
  
  prob_count_st <- cbind(prob_count_st, result)
}

## Create new df mit per borough problem counts

problem_count_per_borough <- rbind(prob_count_man, prob_count_bronx, prob_count_brook,
                                   prob_count_qu, prob_count_st)

colnames(problem_count_per_borough) <- c("Root_stone", "Root_grate", "Root_other", "Trunk_wire", 
                                         "Trunk_light", "Trunk_other", "Branch_light", "Branch_shoe",
                                         "Branch_other")

rownames(problem_count_per_borough) <- c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island")

df <- as.data.frame(t(problem_count_per_borough))

Problem <- c("Root_stone", "Root_grate", "Root_other", "Trunk_wire", 
         "Trunk_light", "Trunk_other", "Branch_light", "Branch_shoe",
         "Branch_other")

borough_problems <- cbind(df, Problem)

save(borough_problems, file = "borough_problems.RData")









