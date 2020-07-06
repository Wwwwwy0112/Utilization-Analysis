#For initialization only:
df <- read.csv("~/Desktop/JHH/data/deidentified_sandbox.csv", fileEncoding="latin1")

population.size = 34040
JHH_SiteName = c("BENDANN", "WEINBERG", "ZBOR3", "ZBOR4", "ZBOR5", "JHOC")
JHH_Sites <- c("JHH OPH BENDANN", "JHH WEINBERG OR", "JHH ZBOR 3", "JHH ZBOR 4", "JHH ZBOR 5", "JHH JHOC OR")

OR_Facility <- df$OR_Facility
OR_Site <- df$OR_Site
OR_F_CaseOverrun <- df$OR_F_CaseOverrun
OR_CaseOverrunMinutes <- df$OR_CaseOverrunMinutes
len <- length(unique(OR_Site))

OR_ActualStartDateTime <- df$OR_ActualStartDateTime
OR_ActualEndDateTime <- df$OR_ActualEndDateTime
OR_SchedStartDateTime <- df$OR_SchedStartDateTime
OR_SchedEndDateTime <- df$OR_SchedEndDateTime

OR_CaseScheduledDurationMinutes <- df$OR_CaseScheduledDurationMinutes
OR_CaseActualDurationMinutes <- df$OR_CaseActualDurationMinutes


OR_PrevCaseActualEndDateTime <- df$OR_PrevCaseActualEndDateTime
OR_CaseClass <- df$OR_CaseClass
OR_CaseDate <- df$OR_CaseDate
OR_Service <- df$OR_Service
OR_Room <- df$OR_Room
OR_OutofRoomDateTime <- df$OR_OutofRoomDateTime

OR_ProcedureStartDateTime <- df$OR_ProcedureStartDateTime #physical start
OR_CaseFinishDateTime <- df$OR_CaseFinishDateTime #physical end

uniq_all_date = unique(OR_CaseDate)

OR_CaseLeadTimeActual <- df$OR_CaseLeadTimeActual

all_date = array()
for(i in 1:length(uniq_all_date)) {
  all_date[i] = toString(uniq_all_date[i])
}
all_date = sort(all_date)
num_date = length(all_date)



week_of_month <- function(d,m) {
  if(m == 4) {
    return(ceiling(d/7))
  } else if(m == 5) {
    return(ceiling((d-5)/7) + 1)
  } else if(m == 6) {
    return(ceiling((d-2)/7) + 1)
  }
}



remove_date <- function (x) {
  copy = array()
  count = 0
  rem = c(6,7,13,14,20,21,27,28,34,35,41,42,48,49,55,56,57,62,63,69,70,76,77,83,84,90,91)
  for(i in 1:length(x)) {
    if(!(i %in% rem)) {
      count = count + 1
      copy[count] = x[i]
    }
  }
  return(copy)
}

remove_zero <- function (x) {
  copy = array()
  count = 0
  for(i in 1:length(x)) {
    if(x[i] != 0) {
      count = count + 1
      copy[count] = x[i]
    }
  }
  return(copy)
}




BENDANN_ROOM <- array()
WEINBERG_ROOM <- array()
ZBOR3_ROOM <- array()
ZBOR4_ROOM <- array()
ZBOR5_ROOM <- array()
JHOC_ROOM <- array()


count1 = 0
count2 = 0
count3 = 0
count4 = 0
count5 = 0
count6 = 0
for(i in 1:population.size) {
  if(toString(OR_Site[i]) == JHH_Sites[1]) {
    count1 = count1 + 1
    BENDANN_ROOM[count1] = toString(OR_Room[i])
  } else if(toString(OR_Site[i]) == JHH_Sites[2]) {
    count2 = count2 + 1
    WEINBERG_ROOM[count2] = toString(OR_Room[i])
  } else if(toString(OR_Site[i]) == JHH_Sites[3]) {
    count3 = count3 + 1
    ZBOR3_ROOM[count3] = toString(OR_Room[i])
  } else if(toString(OR_Site[i]) == JHH_Sites[4]) {
    count4 = count4 + 1
    ZBOR4_ROOM[count4] = toString(OR_Room[i])
  } else if(toString(OR_Site[i]) == JHH_Sites[5]) {
    count5 = count5 + 1
    ZBOR5_ROOM[count5] = toString(OR_Room[i])
  } else if(toString(OR_Site[i]) == JHH_Sites[6]) {
    count6 = count6 + 1
    JHOC_ROOM[count6] = toString(OR_Room[i])
  }
}
BENDANN_ROOM <- sort(unique(BENDANN_ROOM))
WEINBERG_ROOM <- sort(unique(WEINBERG_ROOM))
ZBOR3_ROOM <- sort(unique(ZBOR3_ROOM))
ZBOR4_ROOM <- sort(unique(ZBOR4_ROOM))
ZBOR5_ROOM <- sort(unique(ZBOR5_ROOM))
JHOC_ROOM <- sort(unique(JHOC_ROOM))




BENDANN.Room <- array()
BENDANN.ScheduledStart <- array()
BENDANN.ScheduledEnd <- array()
BENDANN.ActualStart <- array()
BENDANN.ActualEnd <- array()
BENDANN.CaseDate <- array()
BENDANN.Service <- array()
BENDANN.count <- 0
BENDANN.outofroom <- array()
BENDANN.PhysStart <- array()
BENDANN.PhysEnd <- array()
BENDANN.CaseClass <- array()
BENDANN.weekofmonth <- array()
BENDANN.dayofweek <- array()
for(i in 1:population.size) {
  if(toString(OR_Site[i]) == JHH_Sites[1]) {
    BENDANN.count = BENDANN.count + 1
    BENDANN.CaseDate[BENDANN.count] = toString(OR_CaseDate[i])
    BENDANN.Room[BENDANN.count] = toString(OR_Room[i])
    BENDANN.ScheduledStart[BENDANN.count] = toString(OR_SchedStartDateTime[i])
    BENDANN.ScheduledEnd[BENDANN.count] = toString(OR_SchedEndDateTime[i])
    BENDANN.ActualStart[BENDANN.count] = toString(OR_ActualStartDateTime[i])
    BENDANN.ActualEnd[BENDANN.count] = toString(OR_ActualEndDateTime[i])
    BENDANN.Service[BENDANN.count] = toString(OR_Service[i])
    BENDANN.outofroom[BENDANN.count] = toString(OR_OutofRoomDateTime[i])
    BENDANN.PhysStart[BENDANN.count] = toString(OR_ProcedureStartDateTime[i])
    BENDANN.PhysEnd[BENDANN.count] = toString(OR_CaseFinishDateTime[i])
    if((toString(OR_CaseClass[i]) %in% c("Elective", "Standby", "Add On")) || (toString(OR_CaseClass[i]) == "NULL" && OR_CaseLeadTimeActual >= 24)) {
      BENDANN.CaseClass[BENDANN.count] = "Elective"
    } else {
      BENDANN.CaseClass[BENDANN.count] = "Emergency"
    }
    BENDANN.dayofweek[BENDANN.count] = weekdays(as.Date(OR_ActualStartDateTime[i]))
    BENDANN.weekofmonth[BENDANN.count] = week_of_month(as.POSIXlt(OR_ActualStartDateTime[i])$mday, as.POSIXlt(OR_ActualStartDateTime[i])$mon + 1)
  }
}

BENDANN.df <- data.frame("Service"=BENDANN.Service, "CaseDate"=BENDANN.CaseDate, "ScheduledStart"=BENDANN.ScheduledStart,
                         "ScheduledEnd"=BENDANN.ScheduledEnd, "Room"=BENDANN.Room,
                         "ActualStart"=BENDANN.ActualStart, "ActualEnd"=BENDANN.ActualEnd,
                         "OutofRoom"=BENDANN.outofroom, "PhysStart"=BENDANN.PhysStart,
                         "PhysEnd"=BENDANN.PhysEnd, "CaseClass"=BENDANN.CaseClass,
                         "dweek" = BENDANN.dayofweek, "wmon" = BENDANN.weekofmonth)



WEINBERG.Room <- array()
WEINBERG.ScheduledStart <- array()
WEINBERG.ScheduledEnd <- array()
WEINBERG.ActualStart <- array()
WEINBERG.ActualEnd <- array()
WEINBERG.CaseDate <- array()
WEINBERG.Service <- array()
WEINBERG.count <- 0
WEINBERG.outofroom <- array()
WEINBERG.PhysStart <- array()
WEINBERG.PhysEnd <- array()
WEINBERG.CaseClass <- array()
WEINBERG.weekofmonth <- array()
WEINBERG.dayofweek <- array()
for(i in 1:population.size) {
  if(toString(OR_Site[i]) == JHH_Sites[2]) {
    WEINBERG.count = WEINBERG.count + 1
    WEINBERG.CaseDate[WEINBERG.count] = toString(OR_CaseDate[i])
    WEINBERG.Room[WEINBERG.count] = toString(OR_Room[i])
    WEINBERG.ScheduledStart[WEINBERG.count] = toString(OR_SchedStartDateTime[i])
    WEINBERG.ScheduledEnd[WEINBERG.count] = toString(OR_SchedEndDateTime[i])
    WEINBERG.ActualStart[WEINBERG.count] = toString(OR_ActualStartDateTime[i])
    WEINBERG.ActualEnd[WEINBERG.count] = toString(OR_ActualEndDateTime[i])
    WEINBERG.Service[WEINBERG.count] = toString(OR_Service[i])
    WEINBERG.outofroom[WEINBERG.count] = toString(OR_OutofRoomDateTime[i])
    WEINBERG.PhysStart[WEINBERG.count] = toString(OR_ProcedureStartDateTime[i])
    WEINBERG.PhysEnd[WEINBERG.count] = toString(OR_CaseFinishDateTime[i])
    if((toString(OR_CaseClass[i]) %in% c("Elective", "Standby", "Add On")) || (toString(OR_CaseClass[i]) == "NULL" && OR_CaseLeadTimeActual >= 24)) {
      WEINBERG.CaseClass[WEINBERG.count] = "Elective"
    } else {
      WEINBERG.CaseClass[WEINBERG.count] = "Emergency"
    }
    WEINBERG.dayofweek[WEINBERG.count] = weekdays(as.Date(OR_ActualStartDateTime[i]))
    WEINBERG.weekofmonth[WEINBERG.count] = week_of_month(as.POSIXlt(OR_ActualStartDateTime[i])$mday, as.POSIXlt(OR_ActualStartDateTime[i])$mon + 1)
    
  }
}


WEINBERG.df <- data.frame("Service"=WEINBERG.Service, "CaseDate"=WEINBERG.CaseDate, "ScheduledStart"=WEINBERG.ScheduledStart,
                          "ScheduledEnd"=WEINBERG.ScheduledEnd, "Room"=WEINBERG.Room,
                          "ActualStart"=WEINBERG.ActualStart, "ActualEnd"=WEINBERG.ActualEnd,
                          "OutofRoom"=WEINBERG.outofroom, "PhysStart"=WEINBERG.PhysStart,
                          "PhysEnd"=WEINBERG.PhysEnd, "CaseClass"=WEINBERG.CaseClass,
                          "dweek"=WEINBERG.dayofweek, "wmon"=WEINBERG.weekofmonth)







ZBOR3.Room <- array()
ZBOR3.ScheduledStart <- array()
ZBOR3.ScheduledEnd <- array()
ZBOR3.ActualStart <- array()
ZBOR3.ActualEnd <- array()
ZBOR3.CaseDate <- array()
ZBOR3.Service <- array()
ZBOR3.count <- 0
ZBOR3.outofroom <- array()
ZBOR3.PhysStart <- array()
ZBOR3.PhysEnd <- array()
ZBOR3.CaseClass <- array()
ZBOR3.weekofmonth <- array()
ZBOR3.dayofweek <- array()
for(i in 1:population.size) {
  if(toString(OR_Site[i]) == JHH_Sites[3]) {
    ZBOR3.count = ZBOR3.count + 1
    ZBOR3.CaseDate[ZBOR3.count] = toString(OR_CaseDate[i])
    ZBOR3.Room[ZBOR3.count] = toString(OR_Room[i])
    ZBOR3.ScheduledStart[ZBOR3.count] = toString(OR_SchedStartDateTime[i])
    ZBOR3.ScheduledEnd[ZBOR3.count] = toString(OR_SchedEndDateTime[i])
    ZBOR3.ActualStart[ZBOR3.count] = toString(OR_ActualStartDateTime[i])
    ZBOR3.ActualEnd[ZBOR3.count] = toString(OR_ActualEndDateTime[i])
    ZBOR3.Service[ZBOR3.count] = toString(OR_Service[i])
    ZBOR3.outofroom[ZBOR3.count] = toString(OR_OutofRoomDateTime[i])
    ZBOR3.PhysStart[ZBOR3.count] = toString(OR_ProcedureStartDateTime[i])
    ZBOR3.PhysEnd[ZBOR3.count] = toString(OR_CaseFinishDateTime[i])
    if((toString(OR_CaseClass[i]) %in% c("Elective", "Standby", "Add On")) || (toString(OR_CaseClass[i]) == "NULL" && OR_CaseLeadTimeActual >= 24)) {
      ZBOR3.CaseClass[ZBOR3.count] = "Elective"
    } else {
      ZBOR3.CaseClass[ZBOR3.count] = "Emergency"
    }
    ZBOR3.dayofweek[ZBOR3.count] = weekdays(as.Date(OR_ActualStartDateTime[i]))
    ZBOR3.weekofmonth[ZBOR3.count] = week_of_month(as.POSIXlt(OR_ActualStartDateTime[i])$mday, as.POSIXlt(OR_ActualStartDateTime[i])$mon + 1)
    
  }
}


ZBOR3.df <- data.frame("Service"=ZBOR3.Service, "CaseDate"=ZBOR3.CaseDate, "ScheduledStart"=ZBOR3.ScheduledStart,
                       "ScheduledEnd"=ZBOR3.ScheduledEnd, "Room"=ZBOR3.Room,
                       "ActualStart"=ZBOR3.ActualStart, "ActualEnd"=ZBOR3.ActualEnd,
                       "OutofRoom"=ZBOR3.outofroom, "PhysStart"=ZBOR3.PhysStart,
                       "PhysEnd"=ZBOR3.PhysEnd, "CaseClass"=ZBOR3.CaseClass,
                       "dweek"=ZBOR3.dayofweek, "wmon"=ZBOR3.weekofmonth)








ZBOR4.Room <- array()
ZBOR4.ScheduledStart <- array()
ZBOR4.ScheduledEnd <- array()
ZBOR4.ActualStart <- array()
ZBOR4.ActualEnd <- array()
ZBOR4.CaseDate <- array()
ZBOR4.Service <- array()
ZBOR4.count <- 0
ZBOR4.outofroom <- array()
ZBOR4.PhysStart <- array()
ZBOR4.PhysEnd <- array()
ZBOR4.CaseClass <- array()
ZBOR4.weekofmonth <- array()
ZBOR4.dayofweek <- array()
for(i in 1:population.size) {
  if(toString(OR_Site[i]) == JHH_Sites[4]) {
    ZBOR4.count = ZBOR4.count + 1
    ZBOR4.CaseDate[ZBOR4.count] = toString(OR_CaseDate[i])
    ZBOR4.Room[ZBOR4.count] = toString(OR_Room[i])
    ZBOR4.ScheduledStart[ZBOR4.count] = toString(OR_SchedStartDateTime[i])
    ZBOR4.ScheduledEnd[ZBOR4.count] = toString(OR_SchedEndDateTime[i])
    ZBOR4.ActualStart[ZBOR4.count] = toString(OR_ActualStartDateTime[i])
    ZBOR4.ActualEnd[ZBOR4.count] = toString(OR_ActualEndDateTime[i])
    ZBOR4.Service[ZBOR4.count] = toString(OR_Service[i])
    ZBOR4.outofroom[ZBOR4.count] = toString(OR_OutofRoomDateTime[i])
    ZBOR4.PhysStart[ZBOR4.count] = toString(OR_ProcedureStartDateTime[i])
    ZBOR4.PhysEnd[ZBOR4.count] = toString(OR_CaseFinishDateTime[i])
    if((toString(OR_CaseClass[i]) %in% c("Elective", "Standby", "Add On")) || (toString(OR_CaseClass[i]) == "NULL" && OR_CaseLeadTimeActual >= 24)) {
      ZBOR4.CaseClass[ZBOR4.count] = "Elective"
    } else {
      ZBOR4.CaseClass[ZBOR4.count] = "Emergency"
    }
    ZBOR4.dayofweek[ZBOR4.count] = weekdays(as.Date(OR_ActualStartDateTime[i]))
    ZBOR4.weekofmonth[ZBOR4.count] = week_of_month(as.POSIXlt(OR_ActualStartDateTime[i])$mday, as.POSIXlt(OR_ActualStartDateTime[i])$mon + 1)
    
  }
}


ZBOR4.df <- data.frame("Service"=ZBOR4.Service, "CaseDate"=ZBOR4.CaseDate, "ScheduledStart"=ZBOR4.ScheduledStart,
                       "ScheduledEnd"=ZBOR4.ScheduledEnd, "Room"=ZBOR4.Room,
                       "ActualStart"=ZBOR4.ActualStart, "ActualEnd"=ZBOR4.ActualEnd,
                       "OutofRoom"=ZBOR4.outofroom, "PhysStart"=ZBOR4.PhysStart,
                       "PhysEnd"=ZBOR4.PhysEnd, "CaseClass"=ZBOR4.CaseClass,
                       "dweek"=ZBOR4.dayofweek, "wmon"=ZBOR4.weekofmonth)








ZBOR5.Room <- array()
ZBOR5.ScheduledStart <- array()
ZBOR5.ScheduledEnd <- array()
ZBOR5.ActualStart <- array()
ZBOR5.ActualEnd <- array()
ZBOR5.CaseDate <- array()
ZBOR5.Service <- array()
ZBOR5.count <- 0
ZBOR5.outofroom <- array()
ZBOR5.PhysStart <- array()
ZBOR5.PhysEnd <- array()
ZBOR5.CaseClass <- array()
ZBOR5.weekofmonth <- array()
ZBOR5.dayofweek <- array()
for(i in 1:population.size) {
  if(toString(OR_Site[i]) == JHH_Sites[5]) {
    ZBOR5.count = ZBOR5.count + 1
    ZBOR5.CaseDate[ZBOR5.count] = toString(OR_CaseDate[i])
    ZBOR5.Room[ZBOR5.count] = toString(OR_Room[i])
    ZBOR5.ScheduledStart[ZBOR5.count] = toString(OR_SchedStartDateTime[i])
    ZBOR5.ScheduledEnd[ZBOR5.count] = toString(OR_SchedEndDateTime[i])
    ZBOR5.ActualStart[ZBOR5.count] = toString(OR_ActualStartDateTime[i])
    ZBOR5.ActualEnd[ZBOR5.count] = toString(OR_ActualEndDateTime[i])
    ZBOR5.Service[ZBOR5.count] = toString(OR_Service[i])
    ZBOR5.outofroom[ZBOR5.count] = toString(OR_OutofRoomDateTime[i])
    ZBOR5.PhysStart[ZBOR5.count] = toString(OR_ProcedureStartDateTime[i])
    ZBOR5.PhysEnd[ZBOR5.count] = toString(OR_CaseFinishDateTime[i])
    if((toString(OR_CaseClass[i]) %in% c("Elective", "Standby", "Add On")) || (toString(OR_CaseClass[i]) == "NULL" && OR_CaseLeadTimeActual >= 24)) {
      ZBOR5.CaseClass[ZBOR5.count] = "Elective"
    } else {
      ZBOR5.CaseClass[ZBOR5.count] = "Emergency"
    }
    ZBOR5.dayofweek[ZBOR5.count] = weekdays(as.Date(OR_ActualStartDateTime[i]))
    ZBOR5.weekofmonth[ZBOR5.count] = week_of_month(as.POSIXlt(OR_ActualStartDateTime[i])$mday, as.POSIXlt(OR_ActualStartDateTime[i])$mon + 1)
    
  }
}


ZBOR5.df <- data.frame("Service"=ZBOR5.Service, "CaseDate"=ZBOR5.CaseDate, "ScheduledStart"=ZBOR5.ScheduledStart,
                       "ScheduledEnd"=ZBOR5.ScheduledEnd, "Room"=ZBOR5.Room,
                       "ActualStart"=ZBOR5.ActualStart, "ActualEnd"=ZBOR5.ActualEnd,
                       "OutofRoom"=ZBOR5.outofroom, "PhysStart"=ZBOR5.PhysStart,
                       "PhysEnd"=ZBOR5.PhysEnd, "CaseClass"=ZBOR5.CaseClass,
                       "dweek"=ZBOR5.dayofweek, "wmon"=ZBOR5.weekofmonth)









JHOC.Room <- array()
JHOC.ScheduledStart <- array()
JHOC.ScheduledEnd <- array()
JHOC.ActualStart <- array()
JHOC.ActualEnd <- array()
JHOC.CaseDate <- array()
JHOC.Service <- array()
JHOC.count <- 0
JHOC.outofroom <- array()
JHOC.PhysStart <- array()
JHOC.PhysEnd <- array()
JHOC.CaseClass <- array()
JHOC.weekofmonth <- array()
JHOC.dayofweek <- array()
for(i in 1:population.size) {
  if(toString(OR_Site[i]) == JHH_Sites[6]) {
    JHOC.count = JHOC.count + 1
    JHOC.CaseDate[JHOC.count] = toString(OR_CaseDate[i])
    JHOC.Room[JHOC.count] = toString(OR_Room[i])
    JHOC.ScheduledStart[JHOC.count] = toString(OR_SchedStartDateTime[i])
    JHOC.ScheduledEnd[JHOC.count] = toString(OR_SchedEndDateTime[i])
    JHOC.ActualStart[JHOC.count] = toString(OR_ActualStartDateTime[i])
    JHOC.ActualEnd[JHOC.count] = toString(OR_ActualEndDateTime[i])
    JHOC.Service[JHOC.count] = toString(OR_Service[i])
    JHOC.outofroom[JHOC.count] = toString(OR_OutofRoomDateTime[i])
    JHOC.PhysStart[JHOC.count] = toString(OR_ProcedureStartDateTime[i])
    JHOC.PhysEnd[JHOC.count] = toString(OR_CaseFinishDateTime[i])
    if((toString(OR_CaseClass[i]) %in% c("Elective", "Standby", "Add On")) || (toString(OR_CaseClass[i]) == "NULL" && OR_CaseLeadTimeActual >= 24)) {
      JHOC.CaseClass[JHOC.count] = "Elective"
    } else {
      JHOC.CaseClass[JHOC.count] = "Emergency"
    }
    JHOC.dayofweek[JHOC.count] = weekdays(as.Date(OR_ActualStartDateTime[i]))
    JHOC.weekofmonth[JHOC.count] = week_of_month(as.POSIXlt(OR_ActualStartDateTime[i])$mday, as.POSIXlt(OR_ActualStartDateTime[i])$mon + 1)
    
  }
}


JHOC.df <- data.frame("Service"=JHOC.Service, "CaseDate"=JHOC.CaseDate, "ScheduledStart"=JHOC.ScheduledStart,
                      "ScheduledEnd"=JHOC.ScheduledEnd, "Room"=JHOC.Room,
                      "ActualStart"=JHOC.ActualStart, "ActualEnd"=JHOC.ActualEnd,
                      "OutofRoom"=JHOC.outofroom, "PhysStart"=JHOC.PhysStart,
                      "PhysEnd"=JHOC.PhysEnd, "CaseClass"=JHOC.CaseClass,
                      "dweek"=JHOC.dayofweek, "wmon"=JHOC.weekofmonth)


BENDANN.uniq.service = unique(BENDANN.Service)
WEINBERG.uniq.service = unique(WEINBERG.Service)
ZBOR3.uniq.service = unique(ZBOR3.Service)
ZBOR4.uniq.service = unique(ZBOR4.Service)
ZBOR5.uniq.service = unique(ZBOR5.Service)
JHOC.uniq.service = unique(JHOC.Service)

no_weekend = remove_date(all_date)