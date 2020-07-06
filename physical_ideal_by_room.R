# By room:
Ideal_total_time = 570

WEINBERG_ideal_util <- array()


for(b in 1:length(WEINBERG_ROOM)) {
#for(b in 8:8) {
  cur_room <- WEINBERG_ROOM[b]
  print(cur_room)
  ideal_util_rate_arr <- rep(0, num_date)
  
  for(d in 1:num_date) {
    cur_date <- all_date[d]
    ideal_phy_time_per_day = 0
    for(i in 1:WEINBERG.count) {
    #for(i in 1259:1259) {
      if(toString(WEINBERG.df$Room[i]) == cur_room && WEINBERG.df$CaseDate[i] == cur_date && (WEINBERG.df$PhysEnd[i] != "NULL") && (WEINBERG.df$PhysStart[i] != "NULL")) {
        start.hour = as.POSIXlt(WEINBERG.df$PhysStart[i])$hour
        start.min = as.POSIXlt(WEINBERG.df$PhysStart[i])$min
        end.hour = as.POSIXlt(WEINBERG.df$PhysEnd[i])$hour
        end.min = as.POSIXlt(WEINBERG.df$PhysEnd[i])$min
        
        start.day = as.POSIXlt(WEINBERG.df$PhysStart[i])$mday
        end.day = as.POSIXlt(WEINBERG.df$PhysEnd[i])$mday
        start.mon = as.POSIXlt(WEINBERG.df$PhysStart[i])$mon + 1
        end.mon = as.POSIXlt(WEINBERG.df$PhysEnd[i])$mon + 1
        
        if(start.day == end.day && start.mon == end.mon) {
          diff.start = (start.hour - 7) * 60 + (start.min - 30)
          diff.start2 = (start.hour - 17) * 60 + start.min
          diff.end = (end.hour - 17) * 60 + end.min
          if(diff.start >= 0 && diff.end <= 0) { # happened between 7:30-5:00
            ideal_phy_time_per_day = ideal_phy_time_per_day + (end.hour - start.hour) * 60 + (end.min - start.min)
          } else if(diff.start < 0 && diff.end <= 0) {
            ideal_phy_time_per_day = ideal_phy_time_per_day + (end.hour - 7) * 60 + (end.min - 30)
          } else if(diff.start >= 0 && diff.start2 < 0 && diff.end > 0) {
            ideal_phy_time_per_day = ideal_phy_time_per_day + (17 - start.hour) * 60 - start.min
          } else if(diff.start >= 0 && diff.start2 >= 0 && diff.end > 0) {
            ideal_phy_time_per_day = ideal_phy_time_per_day
          } else if(diff.start < 0 && diff.end > 0) {
            ideal_phy_time_per_day = ideal_phy_time_per_day + (17 - 7) * 60 - 30
          }
        } else { #overnight cases
          diff.start = (start.hour - 7) * 60 + (start.min - 30)
          diff.start2 = (start.hour - 17) * 60 + start.min
          if(diff.start >= 0 && diff.start2 < 0) {
            ideal_phy_time_per_day = ideal_phy_time_per_day + (17 - start.hour) * 60 - start.min
          } else if(diff.start < 0){
            ideal_phy_time_per_day = ideal_phy_time_per_day + (17 - 7) * 60 - 30
          } else {
            ideal_phy_time_per_day = 0
          }
        }
        
        
      }
    }
    
    ideal_util_rate_arr[d] <- ideal_phy_time_per_day/Ideal_total_time
  }
  
  weighted_ideal_util_rate_arr = remove_date(ideal_util_rate_arr)
  
  WEINBERG_ideal_util[b] <- median(weighted_ideal_util_rate_arr)
  print(WEINBERG_ideal_util[b])
}
