
JHOC.block.schedule <- read.csv("~/Desktop/JHH/JHOC_BlockSchedule.csv")

JHOC.all.service = sort(unique(JHOC.Service))
JHOC.all.room = sort(unique(JHOC_ROOM))


JHOC.sched_elec.hour = matrix(0, nrow = length(JHOC.all.service), ncol = length(JHOC.all.room))
JHOC.sched_emer.hour = matrix(0, nrow = length(JHOC.all.service), ncol = length(JHOC.all.room))
JHOC.unsched_elec.hour = matrix(0, nrow = length(JHOC.all.service), ncol = length(JHOC.all.room))
JHOC.unsched_emer.hour = matrix(0, nrow = length(JHOC.all.service), ncol = length(JHOC.all.room))
JHOC.sched.over.hour = matrix(0, nrow = length(JHOC.all.service), ncol = length(JHOC.all.room))
JHOC.sched.over.time = matrix(0, nrow = length(JHOC.all.service), ncol = length(JHOC.all.room))

check_scheduled <- function(r,cur_service,dw,wm,start,end) {
  day.schedule = ""
  if(dw == "Monday") {
    day.schedule = strsplit(toString(JHOC.block.schedule$Monday[r]), ",")[[1]]
  } else if(dw == "Tuesday") {
    day.schedule = strsplit(toString(JHOC.block.schedule$Tuesday[r]), ",")[[1]]
  } else if(dw == "Wednesday") {
    day.schedule = strsplit(toString(JHOC.block.schedule$Wednesday[r]), ",")[[1]]
  } else if(dw == "Thursday") {
    day.schedule = strsplit(toString(JHOC.block.schedule$Thursday[r]), ",")[[1]]
  } else if(dw == "Friday") {
    day.schedule = strsplit(toString(JHOC.block.schedule$Friday[r]), ",")[[1]]
  }
  
  to.return = rep(0, 4) #1 is sched.time, 2 is unsched.time, 3 is sched.over.time
  
  case.duration = end - start
  
  to.return[1] = 0
  to.return[2] = case.duration
  to.return[3] = 0
  to.return[4] = 0
  
  if(cur_service %in% day.schedule) { # service scheduled on day of the week
    # check if service scheduled on week of the month'
    pos_arr = which(day.schedule == cur_service)
    for(l in 1:length(pos_arr)) {
      cur_wmon = day.schedule[pos_arr[l] + 1]
      cur_time = day.schedule[pos_arr[l] + 2]
      
      cur_time.start = 7.5
      cur_time.end = 17
      
      if(cur_time != "full") {
        cur_time.start = as.double(strsplit(cur_time, "-")[[1]][1])
        cur_time.end = as.double(strsplit(cur_time, "-")[[1]][2])
      }
      
      
      
      if(grepl(wm, cur_wmon)) { #scheduled based on wmon
        if(start > cur_time.end || end < cur_time.start) { #not scheduled based on hour
          return(to.return)
        } else { #scheduled based on hour
          case.over.time = end - 17
          
          if(case.over.time > 0) {
            to.return[4] = case.over.time
          } else {
            to.return[4] = 0
          }
          to.return[1] = min(end, cur_time.end) - max(start, cur_time.start)
          to.return[2] = 0
          to.return[3] = case.duration - to.return[1]
          return(to.return)
        }
      }
    }
    return(to.return)
  } else {
    return(to.return)
  }
}


time_calc <- function(start,end,cur_time.start, cur_time.end) {
  return(min(end, cur_time.end) - max(start, cur_time.start))
}


for (r in 1:length(JHOC.all.room)) {
  cur_room = JHOC.all.room[r]
  for (s in 1:length(JHOC.all.service)) {
    cur_service = JHOC.all.service[s]
    
    for(i in 1:JHOC.count) {
      if(toString(JHOC.df$Room[i]) == cur_room && toString(JHOC.df$Service[i]) == cur_service) {
        #check if scheduled or not
        wmon = toString(as.integer(JHOC.df$wmon[i]))
        start.time = as.POSIXlt(JHOC.df$ActualStart[i])$hour + (as.POSIXlt(JHOC.df$ActualStart[i])$min/60)
        end.time = as.POSIXlt(JHOC.df$ActualEnd[i])$hour + (as.POSIXlt(JHOC.df$ActualEnd[i])$min/60)
        
        if(!((as.POSIXlt(JHOC.df$ActualEnd[i])$mon == as.POSIXlt(JHOC.df$ActualStart[i])$mon)
             && (as.POSIXlt(JHOC.df$ActualEnd[i])$mday == as.POSIXlt(JHOC.df$ActualStart[i])$mday))) { # Overnight case
          end.time = 24
        }
        
        scheduled.or.not = check_scheduled(r, cur_service, toString(JHOC.df$dweek[i]), wmon, start.time, end.time)
        if(JHOC.df$CaseClass[i] == "Elective") {
          JHOC.sched_elec.hour[s,r] = JHOC.sched_elec.hour[s,r] + scheduled.or.not[1]
          JHOC.unsched_elec.hour[s,r] = JHOC.unsched_elec.hour[s,r] + scheduled.or.not[2]
          JHOC.sched.over.hour[s,r] = JHOC.sched.over.hour[s,r] + scheduled.or.not[3]
          JHOC.sched.over.time[s,r] = JHOC.sched.over.time[s,r] + scheduled.or.not[4]
        } else if(JHOC.df$CaseClass[i] == "Emergency") {
          JHOC.sched_emer.hour[s,r] = JHOC.sched_emer.hour[s,r] + scheduled.or.not[1]
          JHOC.unsched_emer.hour[s,r] = JHOC.unsched_emer.hour[s,r] + scheduled.or.not[2]
          JHOC.sched.over.hour[s,r] = JHOC.sched.over.hour[s,r] + scheduled.or.not[3]
          JHOC.sched.over.time[s,r] = JHOC.sched.over.time[s,r] + scheduled.or.not[4]
        }
      }
    }
  }
}

# write.table(round(JHOC.sched_elec.hour), "~/Desktop/JHH/JHOC_ScheduledElectiveHour.csv", sep=",", row.names = JHOC.all.service, col.names = T)
# 
# write.table(round(JHOC.sched_emer.hour), "~/Desktop/JHH/JHOC_ScheduledEmergencyHour.csv", sep=",", row.names = JHOC.all.service, col.names = T)
# 
# write.table(round(JHOC.unsched_elec.hour), "~/Desktop/JHH/JHOC_UnscheduledElectiveHour.csv", sep=",", row.names = JHOC.all.service, col.names = T)
# 
# write.table(round(JHOC.unsched_emer.hour), "~/Desktop/JHH/JHOC_UnscheduledEmergencyHour.csv", sep=",", row.names = JHOC.all.service, col.names = T)
# 
# write.table(round(JHOC.sched.over.hour), "~/Desktop/JHH/OverHour/JHOC_OverHour.csv", sep=",", row.names = JHOC.all.service, col.names = T)



JHOC.over.hour = matrix(0, nrow = length(JHOC.all.service)+1, ncol = length(JHOC.all.room)+1)
JHOC.over.hour[1:length(JHOC.all.service), 1:length(JHOC.all.room)] = JHOC.sched.over.hour


JHOC.over.time = matrix(0, nrow = length(JHOC.all.service)+1, ncol = length(JHOC.all.room)+1)
JHOC.over.time[1:length(JHOC.all.service), 1:length(JHOC.all.room)] = JHOC.sched.over.time


for(col in 1:(length(JHOC.all.room)+1)) {
  JHOC.over.hour[length(JHOC.all.service)+1,col] = sum(JHOC.over.hour[,col])
  JHOC.over.time[length(JHOC.all.service)+1,col] = sum(JHOC.over.time[,col])
}
for(row in 1:(length(JHOC.all.service)+1)) {
  JHOC.over.hour[row, length(JHOC.all.room)+1] = sum(JHOC.over.hour[row,])
  JHOC.over.time[row, length(JHOC.all.room)+1] = sum(JHOC.over.time[row,])
}

JHOC.row.names = JHOC.all.service
JHOC.row.names[length(JHOC.row.names) + 1] = "Total"

write.table(round(JHOC.over.hour), "~/Desktop/JHH/OverHour/JHOC_OverHour.csv", sep=",", row.names = JHOC.row.names, col.names = T)

write.table(round(JHOC.over.time), "~/Desktop/JHH/OverHour/JHOC_OverTime.csv", sep=",", row.names = JHOC.row.names, col.names = T)

