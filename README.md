# NALMS-2019 

## Background Info

This repository contains the code used to create the `R Shiny` apps that were used in my talk, *Telling the story of Indiana lake water quality with interactive web-based mapping and data visualization*.  The talk was presented at the 39th International Symposium of the North American Lake Management Society in Burlington, VT on November 12, 2019.  

The sections below provide a brief description of what each web application does and how you can find the materials to recreate and interact with each app.  

Each folder in this repository contains all code/data/images that correspond to each app. My slides can also be found in the `slides` folder. 

A final friendly reminder. These apps are under development and the examples below serve as *examples* for how the actual apps will behave.  Therefore, some of the functionality will likely not work or break.  Please do not start a new issue if you come across problems as they will likely be fixed (or may be already fixed in a new version and not updated here).  I'd love to hear about suggestions you may have on the appearance and functionality of the apps!

## App 1 - Data distributions 

App 1 allows the user to generate frequency distributions, summary statistics, and tabulated data for water quality parameters contained in the *Indiana Lake Water Quality Assessment Report for 2015-2018*.  In addition, both plot and table outputs are able to be downloaded as .png and .csv files, respectively.  The `App_1_Distributions` folder contains the files used to render the app.  The web version for the app can be found [here](https://corysauve.shinyapps.io/nalms_distributions/).  

![alt text](https://github.com/corysauve/NALMS-2019/blob/master/readme_pics/app1_dist.png)

## App 2 - Lake Type Aggregations 

App 2 allows the user to aggregate data from the *Indiana Lake Water Quality Assessment Report for 2015-2018* by lake type and generate boxplots, summary statistics, and tabulated data.  In addition, both plot and table outputs are able to be downloaded as .png and .csv files, respectively.  The `App_2_LakeType` folder contains the files used to render the app. The web version for the app can be found [here](https://corysauve.shinyapps.io/nalms_laketype/).  

![alt text](https://github.com/corysauve/NALMS-2019/blob/master/readme_pics/app2_laketype.png)

## App 3 - Ecoregion Aggregations 

App 3 allows the user to aggregate data from the *Indiana Lake Water Quality Assessment Report for 2015-2018* by ecoregion and generate boxplots, summary statistics, and tabulated data.  In addition, both plot and table outputs are able to be downloaded as .png and .csv files, respectively.  The `App_3_Ecoregion` folder contains the files used to render the app. The web version for the app can be found [here](https://corysauve.shinyapps.io/nalms_ecoregion/). 

![alt text](https://github.com/corysauve/NALMS-2019/blob/master/readme_pics/app3_ecoregion.png)

## App 4 - Report Dashboard 

App 4 creates a dashboards that contains the functionality of the above apps plus a little more.  The user can see output from multiple inputs on one screen, access background information about the report, use an interactive table, and create a custom report based on a specific lake.  Currently, the dashboard and table tabs are functioning properly.  The custom report and background tabs are only place holders here. Once we have a final draft, these will be updated and functioning properly. In addition, it is possible to only have one .csv file to run this app.  Currently, there are multiple .csv files only due to save time to develop examples for my talk.  The `App_4_Dashboard` folder contains the files used to render the app.  The web version for the app can be found [here](https://corysauve.shinyapps.io/nalms_dashboard/). 

![alt text](https://github.com/corysauve/NALMS-2019/blob/master/readme_pics/app4_dashboard.png)


