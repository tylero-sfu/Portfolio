# Yelp Database Management

Explanations

## Languages & Skills
* Python
* SQL

Usage Instructions

Setup:

Open the provided .py file in your preferred Python environment or IDE.
Ensure the required packages (pyodbc, random, string) are installed.

Connection:

- The program automatically initiates a connection to the SQL Server database at startup.

Functionalities:

- Login

Input a User ID to log into the application.
Successful login requires a valid user ID from the Yelp database.
Functions below require logged-in status.

- Search Business

Enter filters for business search, each on a separate line.
Use '-' to skip filtering by a specific value.
Displays businesses matching entered filters: (business_id, name, address, city, rating).
Entering '-' for all filters outputs the entire Business table.

- Search Users

Similar to Search Business functionality.
Output format: (user_id, name, useful, funny, cool, yelping_since).

- Make Friend

Input a user_id from the user_yelp table to add as a friend.
Creates a friendship entry in the friendship table.

- Review Business

Enter a business_id from the business table.
Input a rating (an integer from 1 to 5).
Repeatedly prompts for a valid rating until entered.
Creates a review entry in the review table with a randomly generated user_id, business_id, rating, current time, and default attributes for useful, funny, and cool.

- Log Out

Closes the database connection.
Terminates the program.