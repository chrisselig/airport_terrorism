# airport_terrorism
An Analysis of Airport/Aircraft Terrorism 1970-2015

The shiny dashboard of this analysis can be found here:

The data for this analysis was downloaded from:
National Consortium for the Study of Terrorism and Responses to Terrorism
START: A Center of Excellence of the U.S. Department of Homeland Security 
University of Maryland, College Park, MD 20740, USA 301.405.6600
http://www.start.umd.edu/gtd/
Accessed March 1st, 2017

#Summary
The dashboard shows an overview of attacks on airports and aircraft from around the world between the years 1970 to 2015.  It excludes military targets and only focuses on civilian targets.  

The dashboard also shows the casualities associated with each type of weapon used in attacks.

#Codebook
The original code book from START is found in this github repository.  Most of the data remains unchanged except in a few key areas:
1.  In the casuality columns, NA's were changed to 0 so total casualities can be calculated.
2.  For the few incidents where no month or day was listed, I entered Jan 1 so a timeline could be reasonably established for the year of the incident

The original R.script (terrorism.R) is included, and is essentially my scratch pad analysis.

The shiny dashboard code sample is also included (app.R).

Thank you for visiting.

