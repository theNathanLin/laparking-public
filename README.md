# laparking-public
**An Analysis of Los Angeles Parking Citations (Public Repository)**

Produced by Nathan Lin, Johnathan Robinson, & Nick McDermott

An analysis of parking citations issued by various agencies in the City of Los Angeles from 1/1/2019 to 4/15/2019.

## Run the app online
A shinyapps.io instance can be accessed via this URL: https://thenathanlin.shinyapps.io/laparking/

This version has been specifically designed to work with shinyapps.io's 1 GB memory restriction placed on the free version of the platform.

*Note: Because of the memory limitation, the animation will not work on the platform. Attempting to play it may freeze/crash the instance.*

## Running in your local R instance
A version is available in Github so that you can run this instance on your local machine. This is the same instance that is running in the shinyapps.io instance. The memory consumption of this version in Github is also optimized to consume <1 GB of memory. All datasets are in `.RData` format, so the pre-packaged code requires no cleaning or specific data formats.
1. Download the `LA Parking Shiny App` folder in Github to your local machine.
2. Open `global.R` in RStudio
3. Ensure that the packages listed in the `global.R` file are installed
4. Set your R working directory to the folder that contains all those files (`setwd()`)
5. In the top right hand corner, click “Run App.” The global file will reference ui.R and server.R automatically

## Additional Extensions, Considerations & Dataset
The full dataset can be found here on Kaggle: https://www.kaggle.com/cityofLA/los-angeles-parking-citations

Note that the scope of the app/data can easily be extended to encompass a wider date range; we limited the size of the data inputs as well as the possible combinations of user inputs to best optimize the user experience as to ensure that this app could run with < 1 GB of available memory. As such, feel free to fork this repo and modify the data inputs to your own liking.
