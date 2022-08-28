## Step 1

# step 1: create a vector of 3 doors: 2 goats and 1 car
# step 2: randomize the position of the car for a new game
# step 3: return the new game vector
create_game <- function( )
{
  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
  return( a.game )
}


## Step 2 - CONTESTANT SELECTS A DOOR


# step 1: create a vector of doors numbered 1,2,3
# step 2: randomly select ONE of the doors
# step 3: return the selection

select_door <- function( )
{

  doors <- c(1,2,3)
  a.pick <- sample(doors, 1) # YOUR CODE HERE...
  return( a.pick )  # number between 1 and 3

}


## Step 3 - HOST OPENS GOAT DOOR

open_goat_door <- function( a.game, a.pick )
{
  index <- which(a.game == "car")
  goat.set = setdiff(1:3, index)
  goat.left = setdiff(goat.set, a.pick)

  if(length(goat.left) == 1){
    return(goat.left)
  } else{
    opened.door = sample(goat.left, 1)
    return( opened.door )
  }
  # number between 1 and 3

}

## Step 4 - CHANGE DOORS


change_door <- function( stay=T, opened.door, a.pick )
{
  ava.doors = setdiff(1:3, opened.door)

  if(!stay){
    final.pick = setdiff(ava.doors, a.pick)
  }else   {
    final.pick = a.pick
  }
  return( final.pick )  # number between 1 and 3

}



##Step 5 - DETERMINE IF CONTENSTANT HAS WON

determine_winner <- function( final.pick, game )
{
  index <- which(game == "car")

  if(final.pick == index)
  {
    return( "WIN" )
  }
  if(final.pick != index )
  {
    return( "LOSE" )
  }

}
