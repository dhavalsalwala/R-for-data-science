#Dhaval Salwala
#Student ID: 1823 0845

#library import
library(lubridate)

#closure function timer()
timer<-function()
{
  #global variables for controlling our timer()
  init_time<-now() #initialised during timer creation.
  start_time<-NA #initially NA
  finish_time<-NA #initially NA
  do_stop=F #initially F
  
  list(start=function()
  {
    #whenever start is called: it sets the stop as False, initialize start time with current time
    #and reset the finish time with NA.
    
    do_stop<<-F
    start_time<<-now()
    finish_time<<-NA
  },
  stop=function()
  {
    #whenever stop is called: it checks if start time is called first otherwise
    #sets the stop as True and initialised finish time with current time.
    
    if(is.na(start_time))
    {
      stop("Error, Cannot stop as timer was not started...")
    }
    else
    {
      do_stop<<-T
      finish_time<<-now()
    }
  },
  get_state=function()
  {
    #Display all parameters of the timer()
    list(Init=init_time,Start=start_time,Finish=finish_time)
  },
  get_time=function()
  {
    #whenever get_time is called: it checks if stop is called first otherwise
    #calculate and display the time difference between start_time and finish_time in sec.
    
    if(do_stop==F)
    {
      stop("Error, Cannot get time as stop was not called...")
    }
    else
    {
      time_diff<-difftime(finish_time,start_time,units="secs")
      cat("Time difference of",time_diff,"secs")
    }
  })
}

#(1)	Create	the	timer	closure.
t<-timer()
str(t$get_state())

#(2)	Check	for	error 1
t$stop()

#(3)	Start	the	timer
t$start()
str(t$get_state())

#(4)	Check	for	error	2
t$get_time()

#(5)	Call	the	stop	function	and	calculate	the	overall	duration
t$stop()
t$get_time()