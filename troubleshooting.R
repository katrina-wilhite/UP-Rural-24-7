#Code used to fix 8104-KS error message: "Error in dat$methrs[j] <- second_factor * second_position_time : replacement has length zero" 

dat <- dat %>% 
  slice(2:27434, 1)