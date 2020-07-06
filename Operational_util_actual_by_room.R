WEINBERG_ope_actual_util <- array()


for(b in 1:length(WEINBERG_ROOM)) {
  cur_room <- WEINBERG_ROOM[b]
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
    
    for(i in 1:WEINBERG.count) {
      if(toString(WEINBERG.df$Room[i]) == cur_room && WEINBERG.df$CaseDate[i] == cur_date) {
        daily_count[d] = daily_count[d] + 1
        
        start.hour = as.POSIXlt(WEINBERG.df$ActualStart[i])$hour
        start.min = as.POSIXlt(WEINBERG.df$ActualStart[i])$min
        end.hour = as.POSIXlt(WEINBERG.df$ActualEnd[i])$hour
        end.min = as.POSIXlt(WEINBERG.df$ActualEnd[i])$min
        
        start.day = as.POSIXlt(WEINBERG.df$ActualStart[i])$mday
        end.day = as.POSIXlt(WEINBERG.df$ActualEnd[i])$mday
        start.mon = as.POSIXlt(WEINBERG.df$ActualStart[i])$mon + 1
        end.mon = as.POSIXlt(WEINBERG.df$ActualEnd[i])$mon + 1
        
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
        
        actual.total[d] = (last.end.hour - first.start.hour) * 60 + (last.end.min - first.start.min)
        day.duration[d] = day.duration[d] + case.duration
        
        
      }
    }
    
    
    if(daily_count[d] == 0) {
      actual.total[d] = Inf
      operational_util_rate_arr[d] <- day.duration[d]/actual.total[d]
    } else {
      operational_util_rate_arr[d] <- (day.duration[d] + ((daily_count[d] - 1) * 30))/actual.total[d]
    }
    
  }
  weighted_ope_actual_util_arr = remove_date(operational_util_rate_arr)
  
  WEINBERG_ope_actual_util[b] <- median(weighted_ope_actual_util_arr)
  print(WEINBERG_ope_actual_util[b])
  
}
