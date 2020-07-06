# Scheduled days of each service in each room

WEINBERG.block.schedule <- read.csv("~/Desktop/JHH/WEINBERG_BlockSchedule.csv")

WEINBERG.all.service = sort(unique(WEINBERG.Service))
WEINBERG.all.room = sort(unique(WEINBERG_ROOM))

WEINBERG.sched_days = matrix(0, nrow = length(WEINBERG.all.service), ncol = length(WEINBERG.all.room))
WEINBERG.sched_elec = matrix(0, nrow = length(WEINBERG.all.service), ncol = length(WEINBERG.all.room))
WEINBERG.sched_emer = matrix(0, nrow = length(WEINBERG.all.service), ncol = length(WEINBERG.all.room))
WEINBERG.unsched_elec = matrix(0, nrow = length(WEINBERG.all.service), ncol = length(WEINBERG.all.room))
WEINBERG.unsched_emer = matrix(0, nrow = length(WEINBERG.all.service), ncol = length(WEINBERG.all.room))




count_days <- function(s1, t1, d) {
  # print(s1)
  count = 0
  increment = 1
  t = toString(t1)
  if(!grepl(t, "full")) {
    time = strsplit(t, "-")
    start = as.numeric(time[[1]][1])
    end = as.numeric(time[[1]][2])
    increment = (end - start)/(9.5)
  }
  
  if(d == 1 || d == 2) { # monday tuesday
    if(grepl("1", s1)) {
      count = count + 1*increment
    }
    if(grepl("2", s1)) {
      count = count + 3*increment
    }
    if(grepl("3", s1)) {
      count = count + 3*increment
    }
    if(grepl("4", s1)) {
      count = count + 3*increment
    }
    if(grepl("5", s1)) {
      count = count + 3*increment
    }
  } else if(d == 3 || d == 4 || d == 5) {
    if(grepl("1", s1)) {
      count = count + 2*increment
    }
    if(grepl("2", s1)) {
      count = count + 3*increment
    }
    if(grepl("3", s1)) {
      count = count + 3*increment
    }
    if(grepl("4", s1)) {
      count = count + 3*increment
    }
    if(grepl("5", s1)) {
      count = count + 2*increment
    }
  }
  return(count)
}


for(r in 1:length(WEINBERG.all.room)) {
# for(r in 5:5) {
  cur_room = WEINBERG.all.room[r]
  mon.sch = strsplit(toString(WEINBERG.block.schedule$Monday[r]), ",")
  tues.sch = strsplit(toString(WEINBERG.block.schedule$Tuesday[r]), ",")
  wed.sch = strsplit(toString(WEINBERG.block.schedule$Wednesday[r]), ",")
  thu.sch = strsplit(toString(WEINBERG.block.schedule$Thursday[r]), ",")
  fri.sch = strsplit(toString(WEINBERG.block.schedule$Friday[r]), ",")
  for(s in 1:length(WEINBERG.all.service)) {
  # for(s in 6:6) {
    cur_ser = WEINBERG.all.service[s]
    
    num_days = 0
    
    if(cur_ser %in% mon.sch[[1]]) {
      pos_arr = which(mon.sch[[1]] == cur_ser)
      for(l in 1:length(pos_arr)) {
        pos = pos_arr[l]
        num_days = num_days + count_days(mon.sch[[1]][pos + 1], mon.sch[[1]][pos + 2], 1)
      }
    }
    if(cur_ser %in% tues.sch[[1]]) {
      pos_arr = which(tues.sch[[1]] == cur_ser)
      for(l in 1:length(pos_arr)) {
        # print(pos_arr)
        pos = pos_arr[l]
        num_days = num_days + count_days(tues.sch[[1]][pos + 1], tues.sch[[1]][pos + 2], 2)
      }
    }
    
    
    if(cur_ser %in% wed.sch[[1]]) {
      pos_arr = which(wed.sch[[1]] == cur_ser)
      for(l in 1:length(pos_arr)) {
        # print(wed.sch)
        pos = pos_arr[l]
        num_days = num_days + count_days(wed.sch[[1]][pos + 1], wed.sch[[1]][pos + 2], 3)
      }
    }
    if(cur_ser %in% thu.sch[[1]]) {
      pos_arr = which(thu.sch[[1]] == cur_ser)
      for(l in 1:length(pos_arr)) {
        pos = pos_arr[l]
        num_days = num_days + count_days(thu.sch[[1]][pos + 1], thu.sch[[1]][pos + 2], 4)
      }
    }
    if(cur_ser %in% fri.sch[[1]]) {
      pos_arr = which(fri.sch[[1]] == cur_ser)
      for(l in 1:length(pos_arr)) {
        pos = pos_arr[l]
        num_days = num_days + count_days(fri.sch[[1]][pos + 1], fri.sch[[1]][pos + 2], 5)
      }
    }
    
    WEINBERG.sched_days[s,r] = round(num_days, digits = 2)
    
  }
}



WEINBERG.sched_days.total = matrix(0, nrow = length(WEINBERG.all.service)+1, ncol = length(WEINBERG.all.room)+1)

WEINBERG.sched_days.total[1:length(WEINBERG.all.service), 1:length(WEINBERG.all.room)] = WEINBERG.sched_days

for(col in 1:(length(WEINBERG.all.room)+1)) {
  WEINBERG.sched_days.total[length(WEINBERG.all.service)+1,col] = sum(WEINBERG.sched_days.total[,col])
}
for(row in 1:(length(WEINBERG.all.service)+1)) {
  WEINBERG.sched_days.total[row, length(WEINBERG.all.room)+1] = sum(WEINBERG.sched_days.total[row,])
}

WEINBERG.row.names = WEINBERG.all.service
WEINBERG.row.names[length(WEINBERG.row.names) + 1] = "Total"


write.table(round(WEINBERG.sched_days.total*9.5), "~/Desktop/JHH/WEINBERG_ScheduleNumOfDays.csv", sep=",", row.names = WEINBERG.row.names, col.names = T)
