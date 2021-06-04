plot_energy<-function(echem,ymax){

  par(mar = c(5, 4, 1, 5))
  df_energy_density <- echem$df_energy_density
  max_cycle <-max(df_energy_density$cycle)
  #_________________________________________
  plot(0,0,type='n',xaxs='i',yaxs='i',
       xlim=c(0,max_cycle),
       ylim=c(0,ymax),
       xlab='Cycles',ylab='Specific  Energy Density (Wh/kg)')
  points(df_energy_density$cycle, df_energy_density$energy_density_ch,pch=0)
  points(df_energy_density$cycle, df_energy_density$energy_density_dis,pch=1)
  par(new=TRUE)
  plot(0,0,type='n',xaxs='i',yaxs='i',
       axes = FALSE,
       xlab='', ylab='',
       xlim=c(0,max_cycle),
       ylim=c(50,100))
  axis(side = 4,  at=10*(5:10),col='red',col.axis='red')
  points(df_energy_density$cycle, df_energy_density$energy_efficiency,col='red',pch=10)
  mtext('Energy Efficiency (%)',side =4, line = 3,col='red',cex=1)
  legend('bottomright', legend = c("Charge",'Discharge','Efficiency'), pch=c(0,1,10),col=c('black','black','red'),cex=1)

}
