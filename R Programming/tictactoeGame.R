


#User move function declaration
  #board as input parameter
  #Asks for row and column, checks that it's a valid input
  #Checks if cell is taken
  #Adds symbol to board in board[row,col] if empty
  #returns board to main program.

user_move <- function(symbol, board) {
  repeat{
    repeat {
      print("Pick a row: ")
      row <- readLines(con = con, n = 1)
      if(row == "1"| row == "2" | row == "3") {
      break()  
      } else { 
        print("Please choose 1,2, or 3")
      }
    }
    repeat {
      print("Pick a column: ")
      col <- readLines(con = con, n = 1)
      if(col == "1"| col == "2" | col == "3") {
      break()  
      } else {
        print("Please choose 1,2, or 3")
      }
    }
  
  row <- strtoi(row)
  col <- strtoi(col)
  
   if((board[row,col] == "-")) {
      board[row,col] <- symbol;
      print("Nice! Here is the board so far:") ;
      break()
    } else {
      print("This cell is already full, please pick again.")
    }
  }
  return(board)
}

#computer_move function declaration.
  #board as input parameter
  #pick random numbers for [row,col]
  #make sure it's an empty cell
  #place the symbol in that cell.
  #return board to main program.

computer_move <- function(symbol, board) {
  repeat{
    row_select <- floor(runif(1, min = 1, max = 4))
    col_select <- floor(runif(1, min = 1, max = 4))
     if((board[row_select, col_select] == "-")) {
      board[row_select, col_select] <- symbol;
      break()
    }
  }
  print("My turn.")
  Sys.sleep(1)
  return(board)
}

#check if the board if full to check for a tie.
check_if_full <- function(board) { 
  for(col in 1 : ncol(board)) {
    for(row in 1 : nrow(board)) {
      if((board[row,col] == "-")) {
        return(FALSE)
      } 
    }
  }
 return(TRUE)
}

#check if there is a win function declaration
check_win <- function(board) {
  
  #player X horizontal wins
  if(board[1,1] == "x" & board[1,2] == "x" & board[1,3] == "x") {
    return("x")
  }
  if(board[2,1] == "x" & board[2,2] == "x" & board[2,3] == "x") {
    return("x")
  }
  if(board[3,1] == "x" & board[3,2] == "x" & board[3,3] == "x") {
    return("x")
  }
  
  #player o horizontal wins
  if(board[1,1] == "o" & board[1,2] == "o" & board[1,3] == "o") {
    return("o")
  }
  if(board[2,1] == "o" & board[2,2] == "o" & board[2,3] == "o") {
    return("o")
  }
  if(board[3,1] == "o" & board[3,2] == "o" & board[3,3] == "o") {
    return("o")
  }
  
  #player x vertical wins 
  if(board[1,1] == "x" & board[2,1] == "x" & board[3,1] == "x") {
    return("x")
  }
  if(board[1,2] == "x" & board[2,2] == "x" & board[3,2] == "x") {
    return("x")
  }
  if(board[1,3] == "x" & board[2,3] == "x" & board[3,3] == "x") {
    return("x")
  }
  
  #player o vertical wins 
  if(board[1,1] == "o" & board[2,1] == "o" & board[3,1] == "o") {
    return("o")
  }
  if(board[1,2] == "o" & board[2,2] == "o" & board[3,2] == "o") {
    return("o")
  }
  if(board[1,3] == "o" & board[2,3] == "o" & board[3,3] == "o") {
    return("o")
  }
  
  #player x wins diagonally 
  if(board[1,1] == "x" & board[2,2] == "x" & board[3,3] == "x") {
    return("x")
  }
  if(board[1,3] == "x" & board[2,2] == "x" & board[3,1] == "x") {
    return("x")
  }
  
  #player o wins diagonally 
  if(board[1,1] == "o" & board[2,2] == "o" & board[3,3] == "o") {
    return("o")
  }
  if(board[1,3] == "o" & board[2,2] == "o" & board[3,1] == "o") {
    return("o")
  }
  return(FALSE)
}

#Main program 

#Board initialization
board <- matrix("-", nrow=3, ncol=3)

if (interactive()) {
  con <- stdin()
} else {
  con <- "stdin"
}

#User chooses symbol. x or o
repeat{
  print("x or o?")
  playerSymbol <- readLines (con = con, n = 1)
  if(all(playerSymbol == "x")) {
    print("You start, here is the board: ")
    print(board)
    break()
  }
  if(all(playerSymbol == "o")) {
    break()
  }
  else {
    print("Please type x or o")
  }
} 
Sys.sleep(1)

#Game Logic. Alternating turns.
repeat {
  if (playerSymbol == "x") {
    board <- user_move('x', board)
    print(board)

    if(check_win(board) == "x") {
      print("You win!!")
      return()
    }

    if(check_if_full(board)) {
      print("It's a tie. Try again.")
      return()
    }
    Sys.sleep(1)
    board <- computer_move('o', board)
    print(board)

    if(check_win(board) == "o") {
      print("I win! Try again.")
      return()
    }
    if(check_if_full(board)) {
      print("It's a tie. Try again.")
      return()
    }
    
    Sys.sleep(1)
  } 
  else { #playerSymbol = o
    board <- computer_move('x', board)
    print(board)
    if(check_win(board) == "x") {
      print("I win! Try again.")
      return()
    }

    if(check_if_full(board)) {
      print("It's a tie. Try again.")
      return()
    }
    
    Sys.sleep(1)
    board <- user_move('o', board)
    print(board)

    if(check_win(board) == "o") {
      print("You win!!")
      return()
    Sys.sleep(1)
    }
    if(check_if_full(board)) {
      print("It's a tie. Try again.")
      return()
    }
  }
}