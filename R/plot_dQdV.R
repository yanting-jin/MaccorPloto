plot_dQdV <- function(echem,cycleno){

  df_echem <- echem$df_echem
  cycles = seq(1,cycleno,1)
  colours <-magma(length(cycles)+5)

  # plot--------
  plot(0,0,type='n',xaxs='i',yaxs='i',
       xlim=c(2,5),
       ylim=c(-400,400),
       #ylim=c(-1000,1000),
       xlab=expression(paste('Voltage vs. Li/Li'^'+', '(V)')),
       ylab = 'dQ/dV')

  for (i in 1:length(cycles)){
    #print(paste0('dQdV no:',i))

    voltage<-c()
    smooth_dQdV<-c()
    #_________charge____________________________________
    filter <- (df_echem$cycleno==cycles[i]-0.5)&(df_echem$dQdV>0)
    voltage <- df_echem$voltage[filter]
    smooth_dQdV<-movingAverage(df_echem$dQdV[filter],10,TRUE)
    lines(df_echem$voltage[filter],smooth_dQdV,lty=i,col=colours[i])
    rm(filter)
    #_________discharge____________________________________
    filter <- (df_echem$cycleno==cycles[i])&(df_echem$dQdV<0)
    voltage <- df_echem$voltage[filter]
    smooth_dQdV<-movingAverage(df_echem$dQdV[filter],10,TRUE)
    lines(df_echem$voltage[filter],smooth_dQdV,lty=i,col=colours[i])
    rm(filter)
  }
  vertical_lines <-seq(2,4.8, 0.1)
  abline(v=vertical_lines,col='grey50',lty=3)
}
