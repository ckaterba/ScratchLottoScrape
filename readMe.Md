## Scratch Lotto Scraping and Analysis

The R files in this repo scrape the listed state's scratch lottery page. They include a function to build a dataframe listing a bunch of summary stats for the payouts of each type of ticket for each state.

A silly, but long term goal may be to analyze each of every state's scratch lotto games to determine which state has the "best" scratch lotto tickets.  

When possible (ie with CA's scratch lotto), it may be interesting to scrape the number of prizes available, the number that have been paid, and (hopefully) the location of the awards to try to predict where the winning tickets may be.  Success not likely here. 

Note: Only Montana has been completed thus far and may be the only state that ever gets completed. The current code uses regex to brute force scrape the MT scratch lotto page, so any changes to their formatting may break the code. 

To Do: Get better at using rvest etc to scrape more efficiently and make it easier to analyze other states. 
