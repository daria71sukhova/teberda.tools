# teberda.tools 
Utilities for exploratory analysis on a dynamic of the species shoot number at the Teberda mountain grasslands:
- linear trends
- autocorrelations
- correlations between the number of generative shoots of the previous year and the number of vegetative shoots of the current year and vice versa
- correlations between all species
- correlations of the number of shoots and the wether conditions of the previous year
- some visualisations
## Getting started
You may download the .zip file with the package and install from your local machin or you can install it directly from GitHub.
### Installation
To install package from local source file (.zip archive) you sould do:
```
install.packages("path/to/your/source/file", repos = NULL, type = "source")
```
Example:
```
install.packages("~/Downloads/teberda.tools.zip", repos = NULL, type = "source")
```
To install the package from GitHub directly you should have devtools package installed and loaded first:
```
install.packages("devtools")
library(devtools)
```
Then you should use the command install_github:
```
install_github("daria71sukhova/teberda.tools")
```
### Use the package
As you always do, load the package in order to start using it:
```
library(teberda.tools)
```
Several commands are now available to you:
-ltrend: gives you the table with species which show significant linear trends 

-autocorrelations: gives you the table with species which have significant values of autocorrelatons

-speccor: gives the correlation matrix with significant values

-vggvprinextcorr: gives the table with the species which have significant correlations between number of generative shoots of the previous year and number of vegetative shoots of the current year and vise versa

-get_tidy_data: gives the wide tidy dataframe with years in the first column and  number of shoots of the species in the rest

Look into man pages for detailes

