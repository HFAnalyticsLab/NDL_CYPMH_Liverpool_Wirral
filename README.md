<img src="ndlbanner.png" width="405" height="96">

# Networked Data Lab: NDL Liverpool and Wirral analysis on Children and Young People's Mental Health in Liverpool and Wirral

#### Project Status: In-progress

## Project Description

This Networked Data Lab analysis by the NDL lab in Liverpool and Wirral focusses on events relating to primary and secondary mental health services among children and young people under the age of 25. Analsysis accounts for rates of events by age band and group among males and females, rates by IMD quantiles, regressions among related health indicators including community needs, access to GP, and proximity to green space. 

Please note that these research outputs have not yet been peer-reviewed and should be treated as preliminary.

## Data sources

This analysis used the following data: 

•	Secondary User Service (SUS) – used to extract data on hospital admissions
•	Emergency Care Dataset (ECDS) – used to extract data on A&E attendances
•	Mental health Service Data Set (MHSDS) – used to extracts referrals to and contacts with 
Children and Adolescent Mental Health Services (CAMHS) and Adult Mental Health Services
(AMHS). These we collectively refer to below as routine mental health services. These are 
contacts with services that provide routine care for people suspected or diagnosed mental 
health and wellbeing need, learning disabilities, autism, or other neurodevelopmental 
conditions. They are distinct from emergency admissions or A&E attendances in the other 
data sources, that may occur due to a crisis. 
•	Emergency hospital admissions for self-harm, alcohol, and substance abuse, eating disorders 
and other mental health problems. (Extracted from SUS)
•	Attendance to A&E for self-harm, alcohol, and substance abuse, eating disorders. (Extracted 
from ECDS)
•	Any referrals to Mental Health Services. (Extracted from MHSDS)
• Any contacts Mental Health Services. (Extracted from MHSDS)
•	People with any contact with health services for a mental health problem – defined as any 
person with at least one of the outcomes 1-4, during this period.

## How does it work?
Raw data is compiled and counts are taken for relevant events, including A&E attendances, hospital emergency admissions, referrals to and contacts with mental health services. 
Counts for individual events are taken on a month-by-month basis for the period 2018-2021. Counts variables formed the basis of rates calculations in proportion to age-group populations per LSOA. 
Relevant open-data indices are merged with the rates data by LSOA.
Regression analysis is made by comparing events rates against these contextual rates:

•	Lone parent 
•	Access to green space
•	GP per 1000 pop
•	% white ethnicity
•	Index of Multiple Deprivations (IMD)
•	Community Needs INdex (CNI)

## Requirements

•	These scripts were written in R and can be run in RStudio
•	Data are saved to .csv
•	Geo-spatial LSOA boundaries are saved to .shp files


## Getting started

All code for the analysis is included in the file 'ndl_cyp_analysis7_HF_github.R'
All relevant data are included in the same directory as the .R file
Hitting the Run button in the menu should successfully compile data and run the analysis
Please note that larger data sets could take a minute or longer to load
Running the code will results in several new .csv tables and new .jpeg files being saved to the file


## Authors

- Ben Barr bbarr@liverpool.ac.uk
- Jamie O'Brien jamie75@liverpool.ac.uk
- Roberta Piroddi Roberta.Piroddi@liverpoolccg.nhs.org

## License

This project is licensed under the [MIT License](https://opensource.org/licenses/MIT).
