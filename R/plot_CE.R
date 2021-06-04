plot_CE <- function(echem, ymin, ymax){
  df_cycle <- echem$df_cycle
  max_cycle <-max(df_cycle$cycle)
  plot(0,0,type='n',xaxs='i',yaxs='i',
       xlim=c(0,max_cycle),
       ylim=c(ymin,ymax),
       xlab='Cycle',ylab='Coulombic Efficiency (%)')
  points(df_cycle$cycle, df_cycle$CE,pch=5,cex=0.8)
}
