## System State

nhangers<-5   ## number of hangers
myrep<-10    ## number of repetitions
period<-730 ## run for two years (365 * 2 = 730)
myIAT<-3.65 ## Inter Arrival Time (in Days) ## our time = 3.65 for 200 flights/730 day
NumGenPt<- 0 ## number of flights to come at the exact time, default=0 (n-1)

#Simulation clock

start_time = Sys.time()

#Events

envs <- lapply(1:myrep, function(i) {     #### lapply() to repeat myrep times
  
  
  library(simmer)
  
  env <- simmer("Flight system")
  
  ############### START Implementation of Decision Tree ###################
  
  ## add a new flight trajectory
  
  newflight <- trajectory(name = "Flight_path") %>% 
    ## the use of function1() %>% function2() %>% function3()
    ## is the same as using function3(function2(function1()))
    ## if you need to set an attribute use set_attribute() function
    set_attribute("fuel_attribute", function() rnorm(1,70,3))%>% 
    
    branch(function() sample(c(1,2),size=1,replace=T,prob=c(0.90,0.10)), continue=c(T, T), 
           ### begin Process Phase1 
           trajectory(name = "Process_phase1") %>%
             seize("hanger", amount = 1) %>%
             set_prioritization(values = c(1,100,T)) %>%
             ## for counting purpose, use dummy resource like the following
             seize("Process_phase1_in", amount = 1) %>% 
             set_prioritization(values = c(1,100,T)) %>%
             ## use the proper distribution that match real distribution
             timeout(function() rnorm(1, 31, 2)) %>% 
             release("hanger", 1)%>%
             set_attribute("fuel_attribute", function() rnorm(1,50,3))%>%
             
             ######### Discharge after Phase 1  ########
           ## again dummy resource for counting purpose
           seize("takeoff_after_Phase1", amount = 1) %>%
             set_prioritization(values = c(99,100,T)) %>%
             timeout(function() sample(c(4,5,6),1)) %>% 
             release("takeoff_after_Phase1", 1)%>%
             
             ######### Discharge after Phase 1 ########
           
           
           
           #================ begin phase 2 of Process 
           branch(function() sample(c(1,2),size=1,replace=T,prob=c(0.964,0.036)), continue=c(T, T), 
                  trajectory(name = "Process_Phase2") %>%
                    seize("hanger", amount = 1) %>%
                    set_prioritization(values = c(9,100,T)) %>%
                    seize("Process_Phase2_in", amount = 1) %>%
                    set_prioritization(values = c(9,100,T)) %>%
                    ## 28 day
                    timeout(function() rnorm(1, 28, 2)) %>% 
                    release("hanger", 1)%>%
                    
                    
                    ######### Discharge after Phase 2 ########
                  
                  seize("takeoff_after_Phase2", amount = 1) %>%
                    set_prioritization(values = c(99,100,T)) %>%
                    timeout(function() sample(c(4,5,6),1)) %>% 
                    release("takeoff_after_Phase2", 1)%>%
                    
                    ######### END Discharge after Phase 2 ########
                  
                  #=============== begin phase 3 of Process 
                  
                  branch(function() sample(c(1,2),size=1,replace=T,prob=c(0.752,0.248)), continue=c(T, T), 
                         trajectory(name = "Process_Phase3") %>%
                           seize("hanger", amount = 1) %>%
                           set_prioritization(values = c(8,100,T)) %>%
                           seize("Process_Phase3_in", amount = 1) %>%
                           set_prioritization(values = c(8,100,T)) %>%
                           ## 25 day
                           timeout(function() rnorm(1, 25, 2)) %>% 
                           release("hanger", 1) %>%
                           
                           ######### Discharge after Phase 3   ########
                         
                         seize("takeoff_after_phase3", amount = 1) %>%
                           set_prioritization(values = c(99,100,T)) %>%
                           timeout(function() sample(c(4,5,6),1)) %>% 
                           release("takeoff_after_phase3", 1)%>%
                           
                           ######### END Discharge after Phase 3 ########
                         
                         #=============== begin phase 4 of Process 
                         
                         branch(function() sample(c(1,2),size=1,replace=T,prob=c(0.99,0.01)), continue=c(T, T), 
                                trajectory(name = "Process_Phase4") %>%
                                  seize("hanger", amount = 1) %>%
                                  set_prioritization(values = c(8,100,T)) %>%
                                  seize("Process_Phase4_in", amount = 1) %>%
                                  set_prioritization(values = c(8,100,T)) %>%
                                  ## 31.5 day
                                  timeout(function() rnorm(1, 31.5, 2)) %>% 
                                  release("hanger", 1) %>%
                                  
                                  
                                  ######### Discharge after Phase 4   ########
                                
                                seize("takeoff_after_phase4", amount = 1) %>%
                                  set_prioritization(values = c(99,100,T)) %>%
                                  timeout(function() sample(c(4,5,6),1)) %>% 
                                  release("takeoff_after_phase4", 1)%>%
                                  
                                  ######### END Discharge after Phase 4  ########
                                
                                #=====Relapse
                                
                                branch(function() sample(c(1,2),size=1,replace=T,prob=c(0.2,0.8)), continue=c(T, T), 
                                       trajectory(name = "relapse") %>%
                                         seize("hanger", amount = 1) %>%
                                         set_prioritization(values = c(8,100,T)) %>%
                                         seize("relapse", amount = 1) %>%
                                         set_prioritization(values = c(8,100,T)) %>%
                                         ## 40 day
                                         timeout(function() rnorm(1, 40, 2)) %>% 
                                         release("hanger", 1)
                                       
                                       ,
                                       trajectory(name = "relapse_out") %>%
                                         seize("relapse_out", amount = 1) %>%
                                         set_prioritization(values = c(1,100,T))
                                       
                                ) 
                                
                                #=====END Relapse
                                
                                ,
                                trajectory(name = "Phase4_out") %>%
                                  seize("Phase4_out", amount = 1) %>%
                                  set_prioritization(values = c(1,100,T))
                                
                         ) 
                         #=============End Phase 4
                         ,
                         trajectory(name = "Phase3_out") %>%
                           seize("Phase3_out", amount = 1)%>%
                           set_prioritization(values = c(1,100,T))
                         
                  ) 
                  #===============End phase 3
                  ,
                  trajectory(name = "Phase2_out") %>%
                    seize("Phase2_out", amount = 1) %>%
                    set_prioritization(values = c(1,100,T))
                  
           ) 
           #=================End phase 2
           
           ,
           trajectory(name = "Phase1_out") %>%
             set_attribute("fuel_attribute", function() rnorm(1,90,3))%>%
             seize("phase1_out", amount = 1) %>%
             set_prioritization(values = c(1,100,T)) 
           
    ) 
  
  ############### END Implementation of Decision Tree ###################
  
  env %>%
    
    ## add true resource
    
    add_resource("hanger", nhangers) %>% 
    
    ### Note: only resources with defined capacity can be 
    ### plotted in "Resource Utilization"
    
    
    ## add resources for counting purpose
    
    add_resource("Process_phase1_in", Inf) %>%
    add_resource("Process_Phase2_in", Inf) %>%
    add_resource("Process_Phase3_in", Inf) %>%
    add_resource("Process_Phase4_in", Inf) %>%
    add_resource("relapse", Inf) %>%
    
    add_resource("takeoff_after_Phase1", Inf) %>%
    add_resource("takeoff_after_Phase2", Inf) %>%
    add_resource("takeoff_after_phase3", Inf) %>%
    add_resource("takeoff_after_phase4", Inf) %>%
    
    add_resource("Phase4_out", Inf) %>%
    add_resource("Phase3_out", Inf) %>%
    add_resource("Phase2_out", Inf) %>%
    add_resource("phase1_out", Inf) %>%
    add_resource("relapse_out", Inf) %>%
    
    
    ## Add "Flight_path" generator, we assumed exp distribution (from real data)
    
    add_generator("test_flight", newflight, function() c(rexp(1, 1/myIAT), rep(0, NumGenPt)), mon=2)
  
  env %>% run(until=period) ## run the simulation until reach time specified
  
})

##########################  Recording Output ###########

out <- capture.output(envs)

### output the environment log

cat(paste(Sys.time(),"### Number of hanger =",nhangers,"### Number of Replica =",myrep,"### my IAT =",myIAT,"### Replicate Summary: \n"), out, file="envs.txt", sep="\n", append=TRUE)

### get monitored attributes as dataframe

attr<-envs %>% get_mon_attributes()

### get monitored resources as dataframe

resources<-envs %>% get_mon_resources()

n_resources<-resources[resources$resource == "hanger",] ## filter only true resource "hanger"

### get monitored arrivals per resource as dataframe

arrivals<-envs %>% get_mon_arrivals(per_resource=T)

## filter only true resource "hanger" i.e. filter out dummy resources

n_arrivals<-arrivals[arrivals$resource == "hanger",] 

### calculate waiting time per visit and ALOS and add to n_arrivals dataframe

n_arrivals$waiting <- n_arrivals$end_time - n_arrivals$start_time - n_arrivals$activity_time
n_arrivals$ALOS<-n_arrivals$activity_time
## sum the total ALOS per flight
## the next line works accurately only if myrep=1
## reason for not being accurate with more than one replication is that
## some flights will be created in one run but not in the other causing a 
## significant impact on the mean. 
## Next version of this code will fix this issue, till then, the display of results
## from the next line is limited by an "if(myrep==1)"
ALOS_n<-aggregate(n_arrivals$ALOS, by=list(n_arrivals$name), FUN=sum) 

ALOS_n$x<-ALOS_n$x/myrep ## divide by number of replications


### get monitored arrivals NOT per resource as dataframe

arrivals_no_resource<-get_mon_arrivals(envs)
arrivals_no_resource$ALOS<-arrivals_no_resource$activity_time
arrivals_no_resource$waiting<-arrivals_no_resource$end_time - arrivals_no_resource$start_time - arrivals_no_resource$activity_time

### save arrivals dataframe as csv with time stamp

filename_a<-paste0("arrival_",Sys.time(),".csv")

filename_a<-gsub(":", "_", filename_a)

write.csv(arrivals,filename_a)

### save attr dataframe as csv with time stamp

filename_attr<-paste0("attributes_",Sys.time(),".csv")

filename_attr<-gsub(":", "_", filename_attr)

write.csv(attr,filename_attr)

### save arrivals_no_resources dataframe as csv with time stamp

filename_an<-paste0("arrival_no_res_",Sys.time(),".csv")

filename_an<-gsub(":", "_", filename_an)

write.csv(arrivals_no_resource,filename_an)

### save resources dataframe as csv with time stamp

filename_r<-paste0("resources_",Sys.time(),".csv")

filename_r<-gsub(":", "_", filename_r)

write.csv(resources,filename_r)


################ Text mining of environment log ###############

r<-grep("\\brelapse\\b",out) ## get row containing number of relapses
n<-grep('n_generated',out) ## get row containing count of generated flights
q<-grep("\\bbed\\b",out) ## get row containing queue at end of period 

# extract numeric values from rows

relapse<-out[r]
for (i in 1:myrep){
  
  last<-regexpr('(Inf)', relapse[i])
  relapse[i]= substr(relapse[i], (last-3), (last-2))
}

relapse<-as.numeric(relapse)
relapse


newflights<-out[n]
for (i in 1:myrep){
  
  last<-regexpr('}', newflights[i])
  newflights[i]= substr(newflights[i], (last-5), (last-2))
}

newflights<-as.numeric(newflights)
newflights


queue<-out[q]
for (i in 1:myrep){
  
  last<-regexpr('(Inf)', queue[i])
  queue[i]= substr(queue[i], (last-4), (last-2))
}
queue ##this calculate que now i.e. at time <period>

mydata<-data.frame(newflights,relapse,queue)


write.csv(mydata, "my_log_data.csv")

################ END Text mining of environment log ###############

#library routines
############### Start Plotting #############

library(ggplot2)
library(gridExtra)
library(simmer.plot)                          

resources <- get_mon_resources(envs)
p1 <- plot(resources, metric = "usage","hanger")

p2<-plot(resources,metric = "utilization","hanger")

p3<-plot(arrivals,metrics="waiting_time")
p3<-p3 + geom_hline(yintercept=mean(arrivals_no_resource$waiting), colour="red", lwd=1)
p3<-p3 + geom_hline(yintercept=mean(n_arrivals$waiting), colour="green", lwd=1)

p4<-ggplot(data=n_arrivals, aes(n_arrivals$ALOS)) +
  geom_histogram(aes(), breaks=seq(20, 50, by = 2), col="red", fill="green", alpha = .2) +
  geom_density(col=2) +
  labs(title="Length of Stay") +
  labs(x="Stay", y="Count")


#cat("\014")

#report generator 

grid.arrange(p1, p2, p3,p4, ncol=2)

#########################################
# waiting time per resource = TRUE
summary(n_arrivals$waiting)
# waiting time per resource = FALSE
summary(arrivals_no_resource$waiting)

if(myrep==1){
  # ALOS total visits per resource = TRUE
  summary(ALOS_n$x)
}
# ALOS total visits per resource = FALSE
summary(arrivals_no_resource$ALOS)
# ALOS per visit (must be per resource = TRUE)
summary(n_arrivals$activity_time)
# Queue
summary(n_resources$queue)
# flights generated
summary(mydata$newflights)

end_time = Sys.time()

end_time - start_time