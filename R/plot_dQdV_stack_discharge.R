plot_dQdV_stack_discharge <-function(echem,cycleno,increment = 5, height = 150){
  df_echem <- echem$df_echem
  cycles = seq(1,cycleno,1)
  colours <-magma(length(cycles)+5)

  y_max = -(increment*length(cycles)+height)
  #___Plot ______________
  plot(0,0,type='n',xaxs='i',yaxs='i',
       xlim=c(2,5),
       ylim=c(y_max,0),
       ylab = 'dQ/dV in Discharge',
       xlab = expression(paste('Voltage vs. Li/Li'^'+', '(V)'))
       )

  for (i in length(cycles):1){
    voltage<-c()
    smooth_dQdV<-c()
    filter <- (df_echem$cycleno==cycles[i])&(df_echem$dQdV<0)
    voltage <- df_echem$voltage[filter]
    smooth_dQdV<-movingAverage(df_echem$dQdV[filter],10,TRUE)
    lines(df_echem$voltage[filter],smooth_dQdV-(i-1)*increment ,lty=1,col=colours[i],lwd=1)
    rm(filter)
  }
  vertical_lines <-seq(2,4.8, 0.1)
  abline(v=vertical_lines,col='grey50',lty=3)

}
