# Portfolio
Hello! I can't show you everything I've built professionally, but I can show a few snippets of code I've written, as well as, some independent projects.

## Data Analysis Notebooks

### [E Commerce Data Analysis](NoteBooks/Google_Merch_Analysis.ipynb)
I used Google’s “Big Query” database to analyze the clickstream data for the Google Merch Store. 

### [Vaccination Public Opinion Analysis](NoteBooks/Vax_Modeling)
My team and I looked at social media data and vaccine preventable disease outbreaks. While we found correlations and significant evidence of a relationship, our model to forecast outbreaks based on social media did not reach a level to be depended on. After our analysis we saw convincing evidence to suggest that social media platforms which took active measure to stop the spread of misinformation and conspiracy theories had an impact on their metrics and would lead to increased vaccination rates.
- [Vaccination Analysis](NoteBooks/Vax_Modeling/Code/Project_Report.ipynb)
- [Facebook Analysis](NoteBooks/Vax_Modeling/Code/Modeling_Sec1Project_2.ipynb)
- [Twitter Analysis](NoteBooks/Twitter_Analysis.ipynb)


### [Statistical Analysis of School Vaccination with R](School_Vax_Stats/Vax_Analysis.pdf)
I examined California schools vaccine coverage. With this dataset I could use frequentist technique and Bayesian with a Markov-Chain Monte Carlo simulation to establish that private schools had a significantly higher rates of unvaccinated students. I also used a Bayesian approach to identified periods when rates of exceptions to the vaccine requirement increased or decreased. 

### [Using Gradient Boosting to Augment Polling Results](NoteBooks/Gradient_Boosting_With_Polling_Results.ipynb)
I use a script similar to this one to make creat demographic models based on survey results. Gradient Boosting allows me to aggregate weak predictors like individual polls and preform a more accurate classification.

### [Cleaning and Wrangling Messy Data](NoteBooks/LAPD_911_Data_Wrangling.ipynb)
Here I used a variety of techniques to clean an extremely messy data set of emergency call to the LAPD. This a project I'm still working on, with a goal to finding more data on emergency services in Los Angeles. 

### Zoom Export and Analysis
- [Survey Vizualization](NoteBooks/Zoom_Poll_ChartMaker.ipynb) 
I use this script to create vizualizations from the raw output of zoom polling data. It enables a user to load data as a csv and a export bar charts for ready for distribution.    
- [Text Analysis](NoteBooks/Zoom_Transcript_Text_Mining.ipynb)
I use this script to preform basic text mining from raw transcript files exported from a zoom meeting. This script is focused on very basic data cleaning and formatting with some aggregated metrics. It exports the data into a structure that makes it accesiable for more complex NLP like GPT-2.

## Dashboard Applications

### [Measuring Machine Learning Results with Streamlit](Streamlit_Apps/ML_APP/streamlit-ml/app.py)
Here I created an easy to use web app that can preform various predictive modeling techniques and evaluate them with zero code. Built with streamlit, the dashboard includes automated exploratory vizualizations, widgets to select a model, and various evaluations methodologies.

### [Display Changing Enviromental Metrics with Shiny](Shiny_Apps/ShinyDashBoard/App.R)
This an especially tricky problem that I run into often. This app displays multiple variables and domains on a time series with a single dashboard. It uses Shiny and is written in R.




