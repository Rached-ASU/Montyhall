#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
}



#' @title selecting one of the three door randomly
#'
#' @description `select_door()`generate a random door
#'
#' @details this function will represent the choice of the participant where he
#' will have equal chances to select a door
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length of 1 character vector
#'   indicating the selected door
#'
#' @examples select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title Selecting one goat door
#'
#' @description `open_goat_door` generates a number referring to a door with one goat behind
#' different than the one previously selected
#'
#' @details
#' The host will make the game with more suspense will choose a door with one goat behind it and
#' will leave the participant with two doors that one contains another
#' goat and the other contain the car
#'
#' @param tis function will take two parameters, the first parameter game is a vector indicating the positions
#' of goats and the car.
#'
#'  the second parameter will define the door previously selected from the participant
#'
#' @return the function returns a length of 1 character vector
#'   indicating the opened door
#'
#' @examples open_goat_door ( game, a.pick)
#'
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats
   if( game[ a.pick ] == "car" )
   {
     goat.doors <- doors[ game != "car" ]
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   {
     opened.door <- doors[ game != "car" & doors != a.pick ]
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' Give a chance to change the door
#'
#' @description
#' `change_door` Asks the user to change or keep the previously selected door
#'
#' @details
#'  The game will give a last chance to the user to either keep his door or change it and with
#'  this game we will try to get the results of the two scenarios
#'
#' @param Stay a Boolean that can take wither true if the user will change the door or false when
#' the user will keep the original door
#'
#' opened.door a vector that will indicate which door that was previously opened
#'
#' a pick a vector that will indicate the door that was first selected
#'
#' @return the function returns a length 1 vector indicating the final pick of the participant
#'
#' @examples function( stay=T, opened.door, a.pick )
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3)

   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ]
   }

   return( final.pick )  # number between 1 and 3
}



#' @title Determine if the participant won
#'
#' @description `determine_winner` generates a Win or lose depending on the situation
#'
#' @details This function will show the user if he won or lost depending on his final pick.
#' If door behind the final pick was a car that the player win and if not the player will lose
#'
#' @param  final pick a vector that will indicate the door that was last selected
#'
#' @return The function will return a string either WIN or Lose
#'
#' @examples determine_winner( final.pick, game )
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title Play the game one time
#'
#' @description play_game general function that contains all the previous functions will return
#' WIN  or Lose
#'
#' @details The game consist of choosing one door from three doors, one of which has a car
#' behind it and two have goats The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. If the last pick was a car the participants
#'   considered a winner otherwise a looser
#' @param N/A
#'
#' @return will return a small table that indicates the winngin and losing strategy
#'
#' @examples play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}

play_game()



#' @title Play the game one hundred time
#' @description play_n_game is a general function that contains all the previous functions will repeat the game 100 times
#'  and will return  WIN  or Lose
#'
#' @details The game consist of choosing one door from three doors, one of which has a car
#' behind it and two have goats The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. If the last pick was a car the participants
#'   considered a winner otherwise a looser
#'
#' @param n which indicates how many times the game is repeated
#'
#' @return  will return a small table that indicates the winning and losing strategy
#'
#' @examples function( 100 )
#'
#' @export
play_n_games <- function( n=100 )
{

  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>%
  print()

  return( results.df )

}

