# Make sure you do not have more people attending your movie than you have seats.
# The number of adults and children in the theater should be randomized.
# Make sure to answer the 3 questions at the bottom of the starter code.
# 1. Create snacks that the customers can buy and randomize who buys which snack
# 2. Pretend you own multiple theaters and run two simulations to represent each theater and plot the results
# 3. Create conditional statements for movies that may be PG-13 and children are not allowed to watch



ticket_cost <- 65
ticket_cost_child <- 35
movies <- c('Red', 'Top Gun', 'Bergen', 'Bullet Train', 'A day To day')  # List 5 of your favorite movies
screens <-  5
seats <-  100
week_days <- rep(0, 7)  # Store totals for each day
days_week = c("Sun" , "Mon" , "Tue" , "Wed" , "Thu" , "Fri" , "Sat")
movie_rating = c('PG-13' , '+18' , 'Not Rated' , 'PG' , 'NC-17' , 'R')
snacks <-
  data.frame(
    type = c('Popcorn' , 'Nachos' , 'soft drink' , 'pizzal' , 'Ice Cream') ,
    price = c(10 , 15 , 5 , 7 , 10)
  )
total_revnues_per_day <- rep(0, 7)
total_snack_revenue <- rep(0, 7)
total_prices <- rep(0 , 5)
total_revenue <- 0
total_visitoer <- 0
theater<- c(1:5)

adults_revenue<- 0
children_revenue<- 0

adults_snack_revenue<-0
children_snack_revenue<-0

snacks_revenue <- function(theater_vistors , snacks){
  
  # every adult can have 1 type of snack or none
  
  for(vist in 1:theater_vistors){
    
    num_of_snacks_bought = sample(c(0,1) , 1)
    
    # if the customer buys something
    
    if(num_of_snacks_bought > 0){
      
      # choose 1 random snack
      
      random_type_of_snack = sample (length(snacks[, 1]) , 1) # snack index
      
      # add the price to the total price for that index
      
      total_prices[random_type_of_snack] = total_prices[random_type_of_snack] + snacks[random_type_of_snack , 2]
      
    }
  }
  total_prices #<< # Save total price of snacks
 
}

#-----------------------
calculate_revenue <- function(visitors_adults , visitors_children , snacks , ticket_cost , ticket_cost_child){
  # Calculate the revenue for adults and children
  adults_revenue <- visitors_adults * ticket_cost
  children_revenue <- visitors_children * ticket_cost_child
  
  #  adult snack revenue
  adults_snack_revenue = snacks_revenue(visitors_adults , snacks)
  
  # children snacks revenue
  children_snack_revenue = snacks_revenue(visitors_children , snacks)
  
  # total snack revenue
  total_snack_revenue = adults_snack_revenue + children_snack_revenue
  
  # Calculate revenue, and add to running total for the day
  revenue = (adults_revenue  + children_revenue + sum(total_snack_revenue))
  
  revenue
}
theater <- function(theater_num , screens , seats , movies , movie_rating){
  
  
  # iterate through the week
  for (day in 1:length(week_days)) {
    # Keep track of total revenue for the day
    total_revenue <- 0
    
    # iterate through the amount of screens on a particular day
    for (s in 1:screens) {
      # Calculate  how many adults and children are watching the movie
      visitors_adults <- 0
      visitors_children <- 0
      
      # check the rating for each movie
      if(movie_rating[s] == 'PG-13' | movie_rating[s] == 'R' | movie_rating[s] == 'Not Rated'){
        # only adults
        visitors_adults = sample(seats, 1)
        revenue = calculate_revenue(visitors_adults , visitors_children = 0 , snacks , ticket_cost , ticket_cost_child)
      }else{
        # both adults and children can watch 
        visitors_adults = sample(seats, 1)
        seats_remaining <- seats - visitors_adults
        visitors_children <- sample(seats_remaining, 1)
        revenue = calculate_revenue(visitors_adults , visitors_children , snacks , ticket_cost , ticket_cost_child)
      }
      # calculate the total revenue for each screen in a day
      total_revenue = total_revenue + revenue
    }
    # Save total to the corresponding day
    total_revnues_per_day[day] = total_revenue
  }
  week_days
  total_snack_revenue
  total_revnues_per_day
  
  revenue_visual(theater_num , total_revnues_per_day , days_week)
  # Which day had the highest revenue?
  index_of_max = which.max(total_revnues_per_day)
  day_max = days_week[index_of_max]
  print(
    paste0(
      "The Busiest Day in theater number ",theater_num," is : ",
      day_max ,
      ",  With total revenue : " ,
      total_revnues_per_day[index_of_max]
    )
  )
}
visitors_adults = sample(seats, 1)
seats_remaining <- seats - visitors_adults
visitors_children <- sample(seats_remaining, 1)
adults_revenue <- visitors_adults * ticket_cost
children_revenue <- visitors_children * ticket_cost_child
both<- cbind(adults_revenue,children_revenue)
barplot(
  both, col = cm.colors(length(visitors_children)), main = "Ticket revenues adults & children", border ="black", width = c(0.4, 0.2))
(grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
)


revenue_visual <- function(theater_num , total_revnues_per_day , days_week){
  
  # Make a bar chart showing total revenue per day
  
  barplot(
    
    total_revnues_per_day,
    
    xlab = "Days of the week",
    
    ylab = "Revenue",
    
    names.arg = days_week ,
    
    main = paste0("The Total Revenue in a week, theater ",theater_num),
    # color for bar 
    col =("#F4AD7C")
    # col = cm.colors(length(total_revenue_per_day))
    
  )
  
  # Make any other chart
  coul <- brewer.pal(7, "Set3")
  
  # Simple Pie Chart
  library(plotrix)
  pie(
    total_revnues_per_day,
    labels = total_revnues_per_day,
    explode = 0.1 ,
    main = paste0("The Total Revenue in a week, theater ",theater_num),
    #colors = c("red", "yellow", "green", "violet", "orange", "blue", "pink"), 
    col =coul
  )
  legend(
    "bottom",
    days_week ,
    cex = 0.7,
    ncol = 4,
    fill = coul
  )
}


theater(theater_num = 1 , screens = screens , seats = seats  , movies , movie_rating)
theater(theater_num = 2 , screens = screens , seats = seats  , movies , movie_rating)

b<-snacks_revenue(sum(visitors_adults,visitors_children),snacks)
plot(b,col='red'
     ,ylab = 'Revenue'
     , xlab ='Days Number'
     ,main = 'Snacks revenue per day'
     ,type = 'o')
 
#----------------------------new function-------------------------------
theater_visitor <- function(theater_num , screens , seats , movies , movie_rating){
  
  movies_name <- rep(0, 5)  # Store totals for each day
  total_visitoer_by_movies <- rep(0, 5)
  
  # iterate through the week
  for (mov in 1:length(movies_name)) {
   
    
    # iterate through the amount of screens on a particular day
    for (s in 1:screens) {
      # Calculate  how many adults and children are watching the movie
      visitors_adults <- 0
      visitors_children <- 0
      
      # check the rating for each movie
      if(movie_rating[s] == 'PG-13' | movie_rating[s] == 'R' | movie_rating[s] == 'Not Rated'){
        # only adults
        visitors_adults = sample(seats, 1)
        revenue = calculate_revenue(visitors_adults , visitors_children = 0 , snacks , ticket_cost , ticket_cost_child)
      }else{
        # both adults and children can watch 
        visitors_adults = sample(seats, 1)
        seats_remaining <- seats - visitors_adults
        visitors_children <- sample(seats_remaining, 1)
        revenue = calculate_revenue(visitors_adults , visitors_children , snacks , ticket_cost , ticket_cost_child)
      }
      # calculate the total revenue for each screen in a day
      total_revenue = total_revenue + revenue
      # calculate the total vistor for each screen in a day
      total_visitoer = total_visitoer + sum(visitors_adults,visitors_children)
    }
    # Save total to the corresponding day
    #total_revnues_by_day[mov] = total_revenue
    total_visitoer_by_movies[mov]= total_visitoer
  }
  movies_name
  total_snack_revenue
  total_visitoer_by_movies
  movies_titles = c('harry potter', 'twilight','the lord of the rings', 'Ice age', ' maleficent')  # List 5 of your favorite movies
  revenue_visual_movies(theater_num , total_visitoer_by_movies , movies_titles)
  # Which day had the highest revenue?
  index_of_max = which.max(total_visitoer_by_movies)
  day_max = movies_titles[index_of_max]
  print(
    paste0(
      "The Busiest Day in theater number ",theater_num," is : ",
      day_max ,
      ",  With total revenue : " ,
      total_visitoer_by_movies[index_of_max]
    )
  )
}
#--------------------------------------------
revenue_visual_movies <- function(theater_num , total_visitoer_by_movies , movies_titles){
  
  # Make a bar chart showing total revenue per day
  
  barplot(
    
    total_visitoer_by_movies,
    
    xlab = "Movies names",
    
    ylab = "Total of visitors",
    
    names.arg = movies_titles ,
    
    main = paste0("The Total visitor for each  movie "),
    col =("#BBE8EE")
    #col = cm.colors(length(total_visitoer_by_movies))
    
  )
}
theater_visitor(theater_num = 2 , screens = screens , seats = seats  , movies , movie_rating)
