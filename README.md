Global Mortality Visualization Application using R

Database: https://www.who.int/data/data-collection-tools/who-mortality-database

Rshiny Application link: 

This Project utilizes the WHO Mortality database “https://www.who.int/data/data-collection-tools/who-mortality-database” provided by the World Health Organization to generate user-friendly plots that aid in understanding the gravity of the subject matter. By analyzing the changing proportions of causes of death and the number of deaths in various regions, our project aims to provide people with a better understanding of global health status and the challenges faced by humanity worldwide. We hope that this project will inspire individuals to pay closer attention to their health by comprehending the causes of high death rates across different ages, genders, and regions.

This repository contains the following folders: 
1) shinyApp 

2) slides 

The shinyApp folder contains the ui.r, server.r and global.r files which is the user interface code, server code and the global code which contains the libraries we have used. It also contains a www folder which is segregated into 3 folders namely: figures, functions and the raw data folder which contains the images, functions such as histogram, line plots, etc and raw data with documentation that we have utilized respectively to implement the Application.

The process of implementation:

•	At first, we tidied the data, as there were multiple datasets with different features. We extracted the most prominent features that would yield us the best analysis.

•	Once done, we focused on creating the functions of histogram, line plots, radar charts and much more to understand the different trends and insights about mortality around the world. 

•	We designed an interactive UI that will allow users to select age group, gender, cause of death and year in order to visualize the trends.

•	We added a few more unique features here and there such as pop-up ads, tour of the app and adding few images to the application. 

•	Finally, we ran all the global.r files to debug and execute the R shiny application. 

Resources used : 

#https://rstudio.github.io/shinydashboard/get_started.html

#https://chat.openai.com/chat

#https://stackoverflow.com/questions/75969716/shiny-app-keep-panels-consistent-in-several-views

#https://stackoverflow.com/questions/75911344/shinydasboard-with-dynamic-menuitem-from-nestedlist

#https://rstudio.github.io/shinydashboard/behavior.html

#https://fontawesome.com/v4/icons/
