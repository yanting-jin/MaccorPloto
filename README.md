# MaccorPloto
An R shiny app for processing battery data from Maccor cycler

## Details


### Graphical interface for electrochemical processing

This package includes a graphical interface for importing, processing, and plotting battery cycling data from Maccor cyclers
This processing wizard can be summoned using:

   `library(shiny)`

   `runGitHub( "MaccorPloto", "yanting-jin",ref="main")`

### Importing Maccor ASCII data

Use the function `read_echem` to import raw Maccor data in the format of ASCII files.
This function read the raw data and extract the discharge/charge, cycling information, energy density, power density etc. and store the metadata in three dataframe: df_echem, df_cycle, df_energy, that can be exported and stored for later use.

Use `plot_echem` to plot the data


## License

This project is licensed under the GNU Affero General Public License v3.0 - see the [LICENSE](LICENSE) file for details

   
   
   
   
