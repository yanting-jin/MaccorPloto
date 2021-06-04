read_echem <- function(file,mass){
  #' @author  Yanting Jin
  #' @param   file path of maccor ASCII
  #' @param   mass: active mass of that coin cell, unit: mg
  #' @return  df_echem dataframe contains time, voltage, current, charge, dQdV
  #' @return  df_cycle dataframe contains cycle number, charge/discharge capacity, CE

  echem <- read.csv(file[1],header = TRUE, skip = 1, sep = "\t")
  ##generate own dataframe
  capacity = echem$Amp.hr*1E6/mass #unit: mAh/g; the unit of mass is mg
  voltage = echem$Volts #unit: V
  time = echem$Test..Min./60 #unit: hour
  current = echem$Amps #unit: A

  index <- rep(0,length(time))
  index_cycle <-which(diff(echem$Step)!=0)+1
  index[index_cycle]<-0.5
  cycleno = cumsum(index)
  df_echem <-data.frame(time,voltage,current,capacity,cycleno)
  dQdV <- c(0,diff(df_echem$capacity)/diff(df_echem$voltage))
  df_echem$dQdV <- dQdV

  #______________________________________
  # extract the charge and discharge capacity from each cycle
  if (max(cycleno)>=1){
    max_cycle <-as.integer(max(cycleno))
    cycle <- seq(1,max_cycle,1)
    charge <- capacity[index_cycle[2*cycle]-1]
    discharge <- capacity[index_cycle[2*cycle+1]-1]
    CE <-discharge/charge*100
    df_cycle <-data.frame(cycle,charge,discharge,CE)
  } else{
    df_cycle <-data.frame()
  }

  #____calculate the energy density_____________
  voltage_avg_ch <-c()
  voltage_avg_dis <-c()
  voltage_hysteresis <-c()
  capacity_ch <-c()
  capacity_dis <-c()
  energy_efficiency <-c()
  energy_density_ch <-c()
  energy_density_dis <-c()
  time_ch <-c()
  time_dis <-c()
  power_ch <-c()
  power_dis <-c()
  df_energy_density <-data.frame()

  for (i in 1:max_cycle){
    filter <- df_echem$cycleno ==i-0.5
    capacity <-df_echem$capacity[filter]
    voltage <-df_echem$voltage[filter]
    dQ <-c(diff(capacity),0)
    energy <-sum(dQ*voltage) #unit: Wh/kg
    voltage <- energy/max(capacity)
    time <-df_echem$time[filter]
    time <-max(time)-min(time)

    energy_density_ch <-append(energy_density_ch,energy)
    voltage_avg_ch <-append(voltage_avg_ch,voltage)
    capacity_ch <-append(capacity_ch,max(capacity))
    time_ch <-append(time_ch, time)
    rm(filter,capacity,voltage,dQ,energy, time)
    #________________________
    filter <- df_echem$cycleno ==i
    capacity <-df_echem$capacity[filter]
    voltage <-df_echem$voltage[filter]
    dQ <-c(diff(capacity),0)
    energy <-sum(dQ*voltage) #unit: Wh/kg
    voltage <-energy/max(capacity)
    time <-df_echem$time[filter]
    time <-max(time)-min(time)


    energy_density_dis <-append(energy_density_dis,energy)
    voltage_avg_dis <-append(voltage_avg_dis,voltage)
    capacity_dis <-append(capacity_dis,max(capacity))
    time_dis <-append(time_dis, time)
    rm(filter,capacity,voltage,dQ,energy, time)
    #________________________
  }
  energy_efficiency<-energy_density_dis/energy_density_ch*100
  voltage_hysteresis<-voltage_avg_ch-voltage_avg_dis
  power_ch <-energy_density_ch/time_ch
  power_dis <-energy_density_dis/time_dis

  df_energy_density <- data.frame(cycle = df_cycle$cycle,

                                  voltage_avg_ch=voltage_avg_ch,
                                  voltage_avg_dis=voltage_avg_dis,
                                  voltage_hysteresis=voltage_hysteresis,

                                  capacity_ch=capacity_ch,
                                  capacity_dis=capacity_dis,
                                  time_ch = time_ch,
                                  time_dis = time_dis,
                                  power_ch=power_ch,#W/kg
                                  power_dis=power_dis,

                                  energy_density_ch=energy_density_ch, # Wh/kg
                                  energy_density_dis=energy_density_dis,
                                  energy_efficiency=energy_efficiency
                                  )

  return(list(df_echem=df_echem,df_cycle=df_cycle,df_energy_density=df_energy_density))

  rm(echem,capacity,voltage,time,current,
     index,index_cycle,
     cycleno,dQdV,
     discharge,charge,min_length,CE,cycle,
     cycle,voltage_avg_ch,voltage_avg_dis,voltage_hysteresis,
     capacity_ch,capacity_dis,time_ch,time_dis,
     energy_density_ch,energy_density_dis,energy_efficiency,
     power_ch,power_dis)
}
