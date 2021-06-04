# MaccorPloto
An R shiny app for processing battery data from Maccor cycler

## Installation
1. Install the devtools package if you do not already have it:

   `install.packages("devtools")`

2. Install this package

   `devtools::install_github("yanting-jin/MaccorPloto")`

## Details

First load the pacakge by:
  `library("MaccorPloto")`


### Graphical interface for electrochemical processing

This package includes a graphical interface for importing, processing, and plotting battery cycling data from Maccor cyclers
This processing wizard can be summoned using the incantation `maccorploto()`.

### Importing Maccor ASCII data

Use the function `read_echem` to import raw Maccor data in the format of ASCII files.
This function read the raw data and extract the discharge/charge, cycling information, energy density, power density etc. and store the metadata in three dataframe: df_echem, df_cycle, df_energy, that can be exported and stored for later use.

Use `plot_echem` to plot the data


## License

This project is licensed under the GNU Affero General Public License v3.0 - see the [LICENSE](LICENSE) file for details

   
   
   
   
