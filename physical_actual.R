JHOC_ope_actual_util <- array()


for(b in 1:length(JHOC_ROOM)) {
  cur_room <- JHOC_ROOM[b]
  print(cur_room)
  
  operational_util_rate_arr = rep(0, num_date)
  
  daily_count = rep(0, num_date)
  
  day.duration = rep(0, num_date)
  
  actual.total = rep(0, num_date)
  
  for(d in 1:num_date) {
    cur_date <- all_date[d]
    actual_op_time_per_day = 0
    
    first.start.hour = 23
    first.start.min = 59
    
    last.end.hour = 0
    last.end.min = 0
    
    for(i in 1:JHOC.count) {
      if(toString(JHOC.df$Room[i]) == cur_room && JHOC.df$CaseDate[i] == cur_date && (JHOC.df$PhysEnd[i] != "NULL") && (JHOC.df$PhysStart[i] != "NULL")) {
        daily_count[d] = daily_count[d] + 1
        
        start.hour = as.POSIXlt(JHOC.df$PhysStart[i])$hour
        start.min = as.POSIXlt(JHOC.df$PhysStart[i])$min
        end.hour = as.POSIXlt(JHOC.df$PhysEnd[i])$hour
        end.min = as.POSIXlt(JHOC.df$PhysEnd[i])$min
        
        start.day = as.POSIXlt(JHOC.df$PhysStart[i])$mday
        end.day = as.POSIXlt(JHOC.df$PhysEnd[i])$mday
        start.mon = as.POSIXlt(JHOC.df$PhysStart[i])$mon + 1
        end.mon = as.POSIXlt(JHOC.df$PhysEnd[i])$mon + 1
        
        case.duration = 0
        
        if(start.day == end.day && start.mon == end.mon) { #setup time range
          case.duration = (end.hour - start.hour) * 60 + (end.min - start.min)
          if(start.hour < first.start.hour) {
            first.start.hour = start.hour
            first.start.min = start.min
          } else if(start.hour == first.start.hour && start.min < first.start.min) {
            first.start.min = start.min
          }
          
          if(end.hour > last.end.hour) {
            last.end.hour = end.hour
            last.end.min = end.min
          } else if(end.hour == last.end.hour && end.min > last.end.min) {
            last.end.min = end.min
          }
        } else { #overnight cases
          case.duration = (24 - start.hour) * 60 - start.min
          if(start.hour < first.start.hour) {
            first.start.hour = start.hour
            first.start.min = start.min
          } else if(start.hour == first.start.hour && start.min < first.start.min) {
            first.start.min = start.min
          }
          last.end.hour = 24
          last.end.min = 0
        }
        day.duration[d] = day.duration[d] + case.duration
        
        
      }
    }
    
    if(daily_count[d] == 0) {
      actual.total[d] = Inf
    } else {
      actual.total[d] = (last.end.hour - first.start.hour) * 60 + (last.end.min - first.start.min)
    }
    
    operational_util_rate_arr[d] <- day.duration[d]/actual.total[d]
    
  }
  weighted_ope_actual_util_arr = remove_date(operational_util_rate_arr)
  
  JHOC_ope_actual_util[b] <- mean(weighted_ope_actual_util_arr)
  print(JHOC_ope_actual_util[b])
  
}
