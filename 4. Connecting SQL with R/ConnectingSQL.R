#R script file for tutorial on connecting SQLite database and execute queries

####################### Part I: Link R to SQL Starts ######################################

#### Prep: Load R package; Set working directory; Load the database to R
# Remove all variables from the R environment to create a fresh start
rm(list=ls())

# We next load some packages that are generally useful for data analytics.
#In each case, check first to see if a required package is installed. If not, then install it 
# and invoke it as a library.
if(!require(RSQLite)){
  install.packages("RSQLite")
  library(RSQLite)
}
if(!require(treemap)){
  install.packages("treemap")
  library(treemap)
}


# Similarly, we need to change the working directory to the directory where we want to read and save data
# Define a variable, wd, with a string describing the path to the directory where we want to read and save data
# Change this to meet your needs.!!!!!!!!!
wd <- ""
# Set the working directory to this path
setwd(wd)
# Test by displaying the current working directory
getwd()

# Load the data base to R and test the RSQLite library. 
dbname <- "Chinook_Sqlite.sqlite"
  # Establish a database connection to this database file
  conn <- dbConnect(SQLite(),dbname)
  # Take a look at the structure of this connection: it should display the defaults, such as loadable.extensions=TRUE
  str(conn)
  # Assuming there is no error in connecting, we try now to get some data from the database.
  # Define an SQL query to get a small amount of data (use SQLite syntax)
  query <- "SELECT * FROM Invoice LIMIT 20"
  # Use the connection to get the query results
  queryresult <- dbGetQuery(conn,query)
  # Close the connection to the database
  dbDisconnect(conn)
  # Return the result (it is in the form of a data.frame)
  queryresult



#### Execute SQL queries in R
# Let's create a function to grab data from our database according to a query we provide. This will be useful.
getExtract <- function(query){
  # Capture the name of the database file (we assume it is located in the working directory)
  dbname <- "Chinook_Sqlite.sqlite"
  # Establish a database connection to this database file
  conn <- dbConnect(SQLite(),dbname)
  # Use the connection to get the query results
  queryresult <- dbGetQuery(conn,query)
  # Close the connection to the database
  dbDisconnect(conn)
  # Return the result (it is in the form of a data.frame)
  queryresult
}
# Run the next statement to test the getExtract function
getExtract("SELECT * FROM Invoice LIMIT 20")


#Execute Statements One at a Time
  # Define a query to aggregate sales by billing cities
  query <- "SELECT BillingCity, SUM(Total) AS CityTotal FROM Invoice GROUP BY BillingCity ORDER BY CityTotal DESC"
  # Grab the data into a data.frame
  result <- getExtract(query)
  # Display the result
  result
  # Plot the values
  plot(result$CityTotal)
  # Plot the values as a bar chart
  barplot(result$CityTotal)
  # Plot the values as a bar chart with labels
  barplot(result$CityTotal,names.arg=result$BillingCity)
  # Plot the values as a treemap with labels
  treemap(result,title="Total Sales by Cities",index="BillingCity",vSize="CityTotal")
  # Which plot pattern do you like best?

####################### Your excercise######################################
# Define a query to aggregate sales by billing cities and only keep the records with total sales more than 50 (You can refer to your lecture notes "Intro to SQL" for the code)




# Then plot results in terms of bar chart with labels and treemap with labels. Add the two plots into your answer sheet


####################Input your code below and run them##############
 

 