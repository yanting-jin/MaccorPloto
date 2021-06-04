plot_echem<-function(echem,cycleno,xmax){
  df_echem <- echem$df_echem
  max_cycle <-max(df_echem$cycleno)
  print(paste0('max cycle no: ',max_cycle))

  cycles = seq(1,cycleno,1)
  colours <-magma(length(cycles)+5)

  plot(0,0,type='n',xaxs='i',yaxs='i',
       xlim=c(0,xmax),ylim=c(2,5),
       xlab='Specific Capacity (mAh/g)',
       ylab = expression(paste('Voltage vs. Li/Li'^'+', '(V)'))
       )

  for (i in 1:length(cycles)){
    filter <- df_echem$cycleno ==cycles[i]-0.5
    lines(df_echem$capacity[filter],df_echem$voltage[filter],lty=i,
          col=colours[i])
    filter <- df_echem$cycleno ==cycles[i]
    lines(df_echem$capacity[filter],df_echem$voltage[filter],lty=i,
          col=colours[i])
    rm(filter)
  }

  #abline(v=114.5, col='grey50',lty=3) # indicates the 0.5Li mol removal/insertion
  #abline(v=114.5*2, col='grey50',lty=3)
}
