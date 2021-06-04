plot_voltage<-function(echem,ymin, ymax){
  par(mar = c(5, 4, 1, 5))
  df_energy_density <- echem$df_energy_density
  max_cycle <-max(df_energy_density$cycle)

  #_________________________________________
  #average voltage and hysteresis
  plot(0,0,type='n',xaxs='i',yaxs='i',
       xlim=c(0,max_cycle),
       ylim=c(2,5),
       xlab='Cycle',ylab='Average voltage (V)')
  points(df_energy_density$cycle, df_energy_density$voltage_avg_ch,pch=15)
  points(df_energy_density$cycle, df_energy_density$voltage_avg_dis,pch=16)
  par(new=TRUE)
  plot(df_energy_density$cycle, df_energy_density$voltage_hysteresis,
       axes = FALSE,
       xlab='', ylab='',
       xaxs='i',yaxs='i',
       col='red',
       ylim=c(ymin,ymax),pch=13)
  axis(side = 4,  at=0.1*(0:15),col='red',col.axis='red')
  mtext("Voltage Hysteresis (V)",side =4, line = 3,col='red',cex=1)
  legend('bottomright', legend = c("Charge",'Discharge','Hysteresis'), pch=c(15,16,13),col=c('black','black','red'),cex=1)
}
