from pickle import TRUE
import pyodbc
from datetime import datetime
import string 
import random

conn = pyodbc.connect('driver={SQL Server};' 'server=CYPRESS.csil.sfu.ca;' 'uid=s_tylero;pwd=7nLQePPP7M6nGAgG;' 'database=tylero354')
print('Connect Successfully Established')
cursor = conn.cursor()

def login():
    while True:
        user_id = input('Enter user_id: ').strip()
        
        query = 'SELECT * FROM user_yelp WHERE user_id = ?'
        cursor.execute(query, (user_id,))

        user_row = cursor.fetchone()

        if user_row:
            print('Hello ' + user_row[1] + '.')
            return user_id
        else:
            print('Invalid user ID. Please enter a valid user_id.')

def get_valid_number(prompt, default):
    while True:
        value = input(prompt).strip() or default
        try:
            value = float(value)
            if 1 <= value <= 5:
                return value
            else:
                print('Please enter a number between 1 and 5.')
        except ValueError:
            print('Please enter a valid number.')

def search_business():
    print('Search for a business. Enter the following filters...')

    min_stars = get_valid_number('Enter minimum number of stars or - (No filter): ', '0')
    max_stars = get_valid_number('Enter maximum number of stars or - (No filter): ', '5')
    
    if min_stars > max_stars:
        min_stars, max_stars = max_stars, min_stars
        print('Swapped the min and max stars values for search.')

    city = input('Enter city or - (No filter): ').strip().lower()
    city = '%' if city == '-' else city
    
    name = input('Enter name or a part of the name or - (No filter): ').strip().lower()
    name = '%' if name == '-' else f'%{name}%'

    query = 'SELECT * FROM business WHERE stars BETWEEN ? AND ? AND LOWER(city) LIKE ? AND LOWER(name) LIKE ? ORDER BY name ASC'
    
    cursor.execute(query, (min_stars, max_stars, city, name))
    search_results = cursor.fetchall()

    # Display search results
    if search_results:
        print('\nResulting business search:\n')
        print('Business ID, Name, Address, City, Number of Stars')
        for row in search_results:
            stars = float(row[5])
            print(f"{row[0]}, {row[1]}, {row[2]}, {row[3]}, {stars}")
    else:
        print('No businesses matching inputted criteria.\n')


def search_users():
    print('Search for a user. Enter the following filters...')
    
    name = input('Enter name, or a part of the name or - (No filter): ').strip().lower()
    name = '%' if name == '-' else f'%{name}%'

    filters = {
        'useful': get_filter_value('Useful - yes/no, or - (No filter): ', 0, 1),
        'funny': get_filter_value('Funny - yes/no, or - (No filter): ', 0, 1),
        'cool': get_filter_value('Cool - yes/no, or - (No filter):: ', 0, 1)
    }

    query = '''
        SELECT * FROM user_yelp 
        WHERE useful >= ? AND useful <= ? 
        AND funny >= ? AND funny <= ? 
        AND cool >= ? AND cool <= ? 
        AND lower(name) LIKE ? 
        ORDER BY name ASC
    '''

    cursor.execute(query, (
        filters['useful']['min'], filters['useful']['max'],
        filters['funny']['min'], filters['funny']['max'],
        filters['cool']['min'], filters['cool']['max'],
        name
    ))

    search_results = cursor.fetchall()

    # Display search results
    if search_results:
        print('\nResulting user search:\n')
        print('User ID, Name, Useful (yes/no), Funny (yes/no), Cool (yes/no), Date Registered')
        for row in search_results:
            useful = 'yes' if row[4] > 0 else 'no'
            funny = 'yes' if row[5] > 0 else 'no'
            cool = 'yes' if row[6] > 0 else 'no'
            print(f"{row[0]}, {row[1]}, {useful}, {funny}, {cool}, {row[3]}")
    else:
        print('No user matching inputted criteria.\n')

def get_filter_value(prompt, min_val, max_val):
    while True:
        value = input(prompt).strip().lower()
        if value == '-':
            return {'min': min_val, 'max': max_val}
        elif value == 'yes':
            return {'min': 1, 'max': max_val}
        elif value == 'no':
            return {'min': 0, 'max': 0}
        else:
            print('Please enter yes/no or -\n')

def user_exists(user_id):
    search_query = 'SELECT * FROM user_yelp WHERE user_id = ?'
    cursor.execute(search_query, (user_id,))
    return cursor.fetchone() is not None

def friendship_exists(user_id, friend_id):
    search_friendship = 'SELECT * FROM friendship WHERE (user_id = ? AND friend = ?) OR (user_id = ? AND friend = ?)'
    cursor.execute(search_friendship, (user_id, friend_id, friend_id, user_id))
    return cursor.fetchone() is not None

def create_friendship(user_id, friend_id):
    insert_query = 'INSERT INTO friendship VALUES (?, ?)'
    cursor.execute(insert_query, (user_id, friend_id))
    conn.commit()

def make_friend(user_id):
    friend_id = input("Please enter the user ID of the person you'd like to add as a friend: ").strip()
    
    if user_exists(friend_id):
        if not friendship_exists(user_id, friend_id):
            create_friendship(user_id, friend_id)
            print('Friend successfully added!')
        else:
            print('You are already friends with this user.')
    else:
        print('The user with the entered ID does not exist.')

def is_valid_rating(num_stars):
    # Check if the user entered a number 
    try:
        float(num_stars)
        isValid = True
        num_stars = float(num_stars)
    except ValueError:
        isValid = False
    
    # Check if the user entered an integer between 1 and 5
    if(num_stars != 1 and num_stars != 2 and num_stars != 3 and num_stars != 4 and num_stars != 5):
        isValid = False

    return isValid

def business_exists(business_id):
    search_query = 'SELECT * FROM business WHERE business_id = ?'
    cursor.execute(search_query, (business_id,))
    return cursor.fetchone() is not None

def generate_random_review_id():
    review_id = ''.join(random.choice(string.ascii_uppercase + string.ascii_lowercase + string.digits) for _ in range(22))

    # Check if such an ID already exists
    search_query = 'SELECT * FROM review WHERE review_id = ?'
    cursor.execute(search_query, (review_id,))
    not_empty = cursor.fetchone()

    # Regenerate until a unique review ID is created
    while(not_empty):
        review_id = ''.join(random.choice(string.ascii_uppercase + string.ascii_lowercase + string.digits) for _ in range(22))

        cursor.execute(search_query, (review_id,))
        not_empty = cursor.fetchone()

    return review_id

def review_business(user_id):
    business_id = input('Please enter the business ID of the business you wish to review: ').strip()

    if business_exists(business_id):
        num_stars = input('Please rate the business (choose an integer from 1 to 5): ').strip()

        while not is_valid_rating(num_stars):
            num_stars = input('Invalid rating. Please enter an integer between 1 and 5: ').strip()

        review_id = generate_random_review_id()
        current_time = datetime.now().strftime("%Y-%m-%dT%H:%M:%S")

        insert_review_query = 'INSERT INTO review VALUES (?, ?, ?, ?, DEFAULT, DEFAULT, DEFAULT, ?)'
        cursor.execute(insert_review_query, (review_id, user_id, business_id, int(num_stars), current_time))
        conn.commit()

        update_business_query = 'UPDATE business SET stars = (SELECT AVG(stars) FROM review WHERE business_id = ?), review_count = review_count + 1 WHERE business_id = ?'
        cursor.execute(update_business_query, (business_id, business_id))
        conn.commit()

        print(f'Review successfully created! ID = {review_id}\n')
    else:
        print('The business with the entered ID does not exist.\n')

user_id = None
logged_in = False

while True:
    if not logged_in:
        print("Please Enter your User ID: \n")
        user_id = login()
        logged_in = True
        
    print("\n1: Search Business")
    print("2: Search Users")
    print("3: Make Friend")
    print("4: Review Business")
    print("5: Logout\n")

    option = input("Select One of the Following Options (1, 2, 3, 4, 5): ")
    option = option.strip()

    if option == "1":
        search_business()
    elif option == "2":
        search_users()
    elif option == "3":
        make_friend(user_id)
    elif option == "4":
        review_business(user_id)
    elif option == "5":
        print("\nLogged out.")
        logged_in = False
        break
    else:
        print("Invalid Option. Please Try Again.\n")

print("Closing Connection")
conn.close()