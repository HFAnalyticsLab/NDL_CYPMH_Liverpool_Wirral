<img src="ndlbanner.png" width="405" height="96">

# Networked Data Lab: NDL Liverpool and Wirral analysis on Children and Young People's Mental Health in Liverpool and Wirral

#### Project Status: In-progress

## Project Description

This Networked Data Lab analysis by the NDL lab in Liverpool and Wirral focusses on events relating to primary and secondary mental health services among children and young people under the age of 25. Analsysis accounts for rates of events by age band and group among males and females, rates by IMD quantiles, regressions among related health indicators including community needs, access to GP, and proximity to green space. 

Please note that these research outputs have not yet been peer-reviewed and should be treated as preliminary.

## Data sources

This analysis used the following data: 

•	Secondary User Service (SUS) – used to extract data on hospital admissions <br/>
•	Emergency Care Dataset (ECDS) – used to extract data on A&E attendances <br/> 
•	Mental health Service Data Set (MHSDS) – used to extracts referrals to and contacts with 
Children and Adolescent Mental Health Services (CAMHS) and Adult Mental Health Services
(AMHS). These we collectively refer to below as routine mental health services. These are 
contacts with services that provide routine care for people suspected or diagnosed mental 
health and wellbeing need, learning disabilities, autism, or other neurodevelopmental 
conditions. They are distinct from emergency admissions or A&E attendances in the other 
data sources, that may occur due to a crisis.  <br/> 
•	Emergency hospital admissions for self-harm, alcohol, and substance abuse, eating disorders 
and other mental health problems. (Extracted from SUS) <br/> 
•	Attendance to A&E for self-harm, alcohol, and substance abuse, eating disorders. (Extracted 
from ECDS) <br/> 
•	Any referrals to Mental Health Services. (Extracted from MHSDS) <br/> 
• Any contacts Mental Health Services. (Extracted from MHSDS) <br/> 
•	People with any contact with health services for a mental health problem – defined as any 
person with at least one of the outcomes 1-4, during this period. <br/>  <br/> 

Variables are defined the file 'variable_definitions.csv' which is included in the data bundle.

## How does it work?
Raw data is compiled and counts are taken for relevant events, including A&E attendances, hospital emergency admissions, referrals to and contacts with mental health services.  <br/> 
Counts for individual events are taken on a month-by-month basis for the period 2018-2021. Counts variables formed the basis of rates calculations in proportion to age-group populations per LSOA.  <br/> 
Relevant open-data indices are merged with the rates data by LSOA. <br/> 
Regression analysis is made by comparing events rates against these contextual rates: <br/>  <br/> 

•	Lone parent  <br/> 
•	Access to green space <br/> 
•	GP per 1000 pop <br/> 
•	% white ethnicity <br/> 
•	Index of Multiple Deprivations (IMD) <br/> 
•	Community Needs INdex (CNI) <br/> 

## Requirements

•	These scripts were written in R and can be run in RStudio <br/> 
•	Data are saved to .csv <br/> 
•	Geo-spatial LSOA boundaries are saved to .shp files <br/> 


## Getting started

All code for the analysis is included in the file 'ndl_cyp_analysis7_HF_github.R' <br/> 
All relevant data are included in the same directory as the .R file <br/> 
Hitting the Run button in the menu should successfully compile data and run the analysis <br/> 
Please note that larger data sets could take a minute or longer to load <br/> 
Running the code will result in several new .csv tables and new .jpeg files being saved to the file <br/> 


## Authors

- Ben Barr bbarr@liverpool.ac.uk
- Jamie O'Brien jamie75@liverpool.ac.uk
- Roberta Piroddi Roberta.Piroddi@liverpoolccg.nhs.org

## License

This project is licensed under the [MIT License](https://opensource.org/licenses/MIT).
