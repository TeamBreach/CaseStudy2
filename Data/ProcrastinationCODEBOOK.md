---
title: "Codebook"
author: "Arturo Casillas & Eric McCandless"
date: "November 26, 2017"
output: md_document
---


## Procrastination.csv
### General Description

### Variables & Descriptions

| Original Name | Format | Description |
| :------------:| :-----:|:-----------:|
| Age | Number | The participant's age in years |
| Kids| Binary: 'Yes Kids' or 'No Kids' | whether they have kids or not|
| Edu | Factor | Education level |
| Work Status| | Factor | What kind of job are they working?: "retired", "part-time" , "student", "full-time", "unemployed" |
| Annual Income| | Number | Respondent's income, converted to U.S. dollars |
| Current Occupation| | Character | A write-in for occupation. It contains typos |
| How long have you held this position? | Number - scientific | Number of years in this job. |
| How long have you held this position? | Number | Number of months in this job. |
| Community | Character/Factor  | Size of community: Large-City, Village, Large Town, Small Town, Rural/Country, Medium-Sized,  Small City |
| Country of Residence| | Factor | The country where the person holds citizenship. |
| Marital Status| Single, Married, Divorced, Separated, etc. | Whether the person has ever been married or divorced. |
| Number of sons | Character | integer number of sons., needs to be modified to be a number |
|  Number of daughters | Number | integer number of daughters. |
| All variables starting with DP | Number: 1 - 5 | the Decisional Procrastination Scale (Mann, 1982) |
| All variables starting with AIP | Number: 1 - 5 | Adult Inventory of Procrastination (McCown & Johnson, 1989) |
| All variables starting with GP | Number: 1 - 5 |  the General Procrastination scale (Lay, 1986) |
| All variables starting with SWLS | Number: 1 - 5 |  the Satisfaction with Life Scale (Diener et al., 1985) |
| Do you consider yourself a procrastinator?| a binary response | Self assessment of procrastination |
| Do others consider you a procrastinator?| a binary response | Perceived outside assessment of procrastination |


## hdi.csv
### General Description

### Variables & Descriptions

| Name | Format | Description |
| :---:| :-----:|:-----------:|
| Country | Character | The country where the person holds citizenship. |
| HDI | Number: 0-1 | Human Development Index |
| Development Level | Factor: Low to Very-High | A cateogry of development based on HDI.  |

## Procrastination_hdi.csv
### General Description

### Variables & Descriptions

| Name | Format | Description |
| :---:| :-----:| :---------: |
|Age| Number | The participant's age in years |
| Kids| Binary: 'Yes Kids' or 'No Kids' | whether they have kids or not|
| Education | Factor | Education level |
| Work Status| | Factor | What kind of job are they working?: "retired", "part-time" , "student", "full-time", "unemployed" |
| Income Year| | Number | Respondent's income, converted to U.S. dollars |
| Current Job| | Character | A write-in for occupation. |
| Years Emp. | Factor | Number of years in this job. |
| Months Emp. | Factor | Number of months in this job. |
| Comm. Size | Factor | Size of community |
| Country of Residence| | Factor | The country where the person holds citizenship. |
| Marital Stat| Single, Married, Divorced, Separated, etc. | Whether the person has ever been married or divorced. |
| Sons | Number | integer number of sons. |
| Daughters | Number | integer number of daughters. |
| All variables starting DP | Number: 1 - 5 | the Decisional Procrastination Scale (Mann, 1982) |
| All variables starting AIP | Number: 1 - 5 | Adult Inventory of Procrastination (McCown & Johnson, 1989) |
| All variables starting GP | Number: 1 - 5 |  the General Procrastination scale (Lay, 1986) |
| All variables starting SWLS | Number: 1 - 5 |  the Satisfaction with Life Scale (Diener et al., 1985) |
| Self Assess | a binary response | Self assessment of respondent's procrastination |
| Other Assess | a binary response | Perceived assessment from others of respondent's procrastination |
| DP Mean | Number: 1 - 5 | mean of the Decisional Procrastination Scale per respondent (Mann, 1982) |
| AIP Mean | Number: 1 - 5 | mean of Adult Inventory of Procrastination per respondent (McCown & Johnson, 1989) |
| GP Mean | Number: 1 - 5 |  mean of the General Procrastination scale per respondent (Lay, 1986) |
| SWLS Mean | Number: 1 - 5 | mean of  the Satisfaction with Life Scale per respondent (Diener et al., 1985) |
| HDI | Number: 0-1 |  Human Development Index |
| Development Level | Number | mean of  the Satisfaction with Life Scale per respondent (Diener et al., 1985) |

## DP_MeanFinal.csv
### General Description

| Name | Format | Description |
| :---:| :-----:| :---------: |
| Country | Character | The country where the person holds citizenship. |
| XDP.Mean | Number: 1-5 | mean of the Decisional Procrastination Scale per respondent (Mann, 1982) |
| Development_Level | Character | |
| HDI | Number: 0-1  | Human Development Index |

## DP_MeanFinal.csv
### General Description

| Name | Format | Description |
| :---:| :-----:| :---------: |
| Country | Character |  The country where the person holds citizenship.|
| XGP.Mean | Number: 1-5 | mean of the General Procrastination scale per respondent (Lay, 1986 |
| Development_Level | Character | |
| HDI | Number: 0-1  | Human Development Index |