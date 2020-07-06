# By room:

cur_service = "Orthopedics"
service_day = c(1)

begin.hour = 7
begin.min = 30
finish.hour = 17
finish.min = 0

Ideal_total_time = (finish.hour - begin.hour) * 60 + (finish.min - begin.min)

cur_room <- JHOC_ROOM[8]


ideal_util_rate_arr1 = array()



count_day = 0
JHOC_ideal_util = 0
daily_count = rep(0, num_date)
for(d in 1:num_date) {
  cur_date <- all_date[d]
  if(as.POSIXlt(all_date[d])$wday %in% service_day) {
    count_day = count_day + 1
    
    ideal_op_time_per_day = 0
    for(i in 1:JHOC.count) {
      if((toString(JHOC.df$Service[i]) == cur_service) 
         && toString(JHOC.df$Room[i]) == cur_room 
         && JHOC.df$CaseDate[i] == cur_date) {
        daily_count[d] = daily_count[d] + 1
        start.hour = as.POSIXlt(JHOC.df$ActualStart[i])$hour
        start.min = as.POSIXlt(JHOC.df$ActualStart[i])$min
        end.hour = as.POSIXlt(JHOC.df$ActualEnd[i])$hour
        end.min = as.POSIXlt(JHOC.df$ActualEnd[i])$min
        
        start.day = as.POSIXlt(JHOC.df$ActualStart[i])$mday
        end.day = as.POSIXlt(JHOC.df$ActualEnd[i])$mday
        start.mon = as.POSIXlt(JHOC.df$ActualStart[i])$mon + 1
        end.mon = as.POSIXlt(JHOC.df$ActualEnd[i])$mon + 1
        
        if(start.day == end.day && start.mon == end.mon) {
          diff.start = (start.hour - begin.hour) * 60 + (start.min - begin.min)
          diff.start2 = (start.hour - finish.hour) * 60 + (start.min - finish.min)
          diff.end = (end.hour - finish.hour) * 60 + (end.min - finish.min)
          if(diff.start >= 0 && diff.end <= 0) { # happened between 7:30-5:00
            ideal_op_time_per_day = ideal_op_time_per_day + (end.hour - start.hour) * 60 + (end.min - start.min)
          } else if(diff.start < 0 && diff.end <= 0) {
            ideal_op_time_per_day = ideal_op_time_per_day + (end.hour - begin.hour) * 60 + (end.min - begin.min)
          } else if(diff.start >= 0 && diff.start2 < 0 && diff.end > 0) {
            ideal_op_time_per_day = ideal_op_time_per_day + (finish.hour - start.hour) * 60 + (finish.min - start.min)
          } else if(diff.start >= 0 && diff.start2 >= 0 && diff.end > 0) {
            ideal_op_time_per_day = ideal_op_time_per_day
          } else if(diff.start < 0 && diff.end > 0) {
            ideal_op_time_per_day = ideal_op_time_per_day + (finish.hour - begin.hour) * 60 + (finish.min - begin.min)
          }
        } else { #overnight cases
          diff.start = (start.hour - begin.hour) * 60 + (start.min - begin.min)
          diff.start2 = (start.hour - finish.hour) * 60 + (start.min - finish.min)
          if(diff.start >= 0 && diff.start2 < 0) {
            ideal_op_time_per_day = ideal_op_time_per_day + (finish.hour - start.hour) * 60 + (finish.min - start.min)
          } else if(diff.start < 0){
            ideal_op_time_per_day = ideal_op_time_per_day + (finish.hour - begin.hour) * 60 + (finish.min - begin.min)
          } else {
            ideal_op_time_per_day = 0
          }
        }
      }
    }
    
    if(daily_count[d] == 0) {
      ideal_util_rate_arr1[count_day] <- ideal_op_time_per_day/Ideal_total_time
    } else {
      ideal_util_rate_arr1[count_day] <- (ideal_op_time_per_day + ((daily_count[d] - 1) * 30))/Ideal_total_time
    }
    
  }
}

ideal_util_rate_arr2 <- array()

count_day2 = 0
JHOC_ideal_util = 0
for(d in 1:num_date) {
  cur_date <- all_date[d]
  if(as.POSIXlt(all_date[d])$wday %in% service_day) {
    count_day2 = count_day2 + 1
    
    ideal_phy_time_per_day = 0
    for(i in 1:JHOC.count) {
      if((toString(JHOC.df$Service[i]) == cur_service) 
         && toString(JHOC.df$Room[i]) == cur_room 
         && JHOC.df$CaseDate[i] == cur_date 
         && (JHOC.df$PhysEnd[i] != "NULL") 
         && (JHOC.df$PhysStart[i] != "NULL")) {
        start.hour = as.POSIXlt(JHOC.df$PhysStart[i])$hour
        start.min = as.POSIXlt(JHOC.df$PhysStart[i])$min
        end.hour = as.POSIXlt(JHOC.df$PhysEnd[i])$hour
        end.min = as.POSIXlt(JHOC.df$PhysEnd[i])$min
        as.POSIXlt(JHOC.df$PhysEnd[i])
        
        start.day = as.POSIXlt(JHOC.df$PhysStart[i])$mday
        end.day = as.POSIXlt(JHOC.df$PhysEnd[i])$mday
        start.mon = as.POSIXlt(JHOC.df$PhysStart[i])$mon + 1
        end.mon = as.POSIXlt(JHOC.df$PhysEnd[i])$mon + 1
        
        if(start.day == end.day && start.mon == end.mon) {
          diff.start = (start.hour - begin.hour) * 60 + (start.min - begin.min)
          diff.start2 = (start.hour - finish.hour) * 60 + (start.min - finish.min)
          diff.end = (end.hour - finish.hour) * 60 + (end.min - finish.min)
          if(diff.start >= 0 && diff.end <= 0) { # happened between 7:30-5:00
            ideal_phy_time_per_day = ideal_phy_time_per_day + (end.hour - start.hour) * 60 + (end.min - start.min)
          } else if(diff.start < 0 && diff.end <= 0) {
            ideal_phy_time_per_day = ideal_phy_time_per_day + (end.hour - begin.hour) * 60 + (end.min - begin.min)
          } else if(diff.start >= 0 && diff.start2 < 0 && diff.end > 0) {
            ideal_phy_time_per_day = ideal_phy_time_per_day + (finish.hour - start.hour) * 60 + (finish.min - start.min)
          } else if(diff.start >= 0 && diff.start2 >= 0 && diff.end > 0) {
            ideal_phy_time_per_day = ideal_phy_time_per_day
          } else if(diff.start < 0 && diff.end > 0) {
            ideal_phy_time_per_day = ideal_phy_time_per_day + (finish.hour - begin.hour) * 60 + (finish.min - begin.min)
          }
        } else { #overnight cases
          diff.start = (start.hour - begin.hour) * 60 + (start.min - begin.min)
          diff.start2 = (start.hour - finish.hour) * 60 + (start.min - finish.min)
          if(diff.start >= 0 && diff.start2 < 0) {
            ideal_phy_time_per_day = ideal_phy_time_per_day + (finish.hour - start.hour) * 60 + (finish.min - start.min)
          } else if(diff.start < 0){
            ideal_phy_time_per_day = ideal_phy_time_per_day + (finish.hour - begin.hour) * 60 + (finish.min - begin.min)
          } else {
            ideal_phy_time_per_day = 0
          }
        }
      }
    }
    ideal_util_rate_arr2[count_day2] <- ideal_phy_time_per_day/Ideal_total_time
  }
}
print(cur_room)
ideal_util_rate_arr11 = remove_zero(ideal_util_rate_arr1)
ideal_util_rate_arr22 = remove_zero(ideal_util_rate_arr2)
print(mean(ideal_util_rate_arr22) * 100)
print(mean(ideal_util_rate_arr11) * 100)

