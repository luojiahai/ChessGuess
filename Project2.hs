----------------------------------------------
-- | COMP90048 Declarative Programming    | --
-- | Project 1                            | --
-- | Name: Geoffrey Ka-Hoi Law            | --
-- | Student No.: 759218                  | --
-- | Email: glaw@student.unimelb.edu.au   | --
----------------------------------------------


---- the strategy is described on the footer ----


module Project2 (initialGuess, nextGuess, GameState) where

data Color = Black | White
             deriving (Eq)
data Kind = King | Queen | Rook | Knight | Bishop | Pawn
             deriving (Eq)
data Piece = Piece Color Kind
             deriving (Eq)

instance Show Color where
    show Black  = "B"
    show White  = "W"

instance Show Kind where
    show King   = "K"
    show Queen  = "Q"
    show Rook   = "R"
    show Knight = "N"
    show Bishop = "B"
    show Pawn   = "P"

-- to represent Piece as concatenation of Color and Kind
instance Show Piece where
    show (Piece c k) = show c ++ show k

-- (size, current Piece state, previous answer, solution)
type GameState = (Int, Piece, (Int,Int,Int), [String])

-- to initilize with pawn pieces up to size, if size > 8 then up to 8
initialGuess :: Int -> ([String],GameState)
initialGuess 0 = ([],(0, Piece Black Pawn, (0,0,0),[]))
initialGuess size = let guess = if size <= 8 
                                then generate size (Piece Black Pawn) 
                                else generate 8 (Piece Black Pawn) 
                    in (guess, (size, Piece Black Pawn, (0,0,0), []))

-- to generate pieces with given quantity and piece, 
-- returns a list of pieces each in string format
generate :: Int -> Piece -> [String]
generate 0 _ = []
generate n piece = show piece : generate (n-1) piece

-- to get the next solution in Pawn state
nextPawnSoln :: (Int, Int, Int) -> [String]
nextPawnSoln (x,y,z) = 
    generate x (Piece Black Pawn) ++ generate y (Piece White Pawn)

-- to make a guess in Pawn state
nextPawnGuess :: ([String],GameState) -> (Int, Int, Int) -> [String]
nextPawnGuess (prev_guess, (size, piece, prev_ans, soln)) (x,y,z) 
    | piece == Piece Black Pawn = nextPawnSoln (x,y,z)
    | piece == Piece White Pawn = 
        if size - length soln < 2 
        then soln ++ generate 1 (Piece Black Knight) 
        else soln ++ generate 2 (Piece Black Knight)

-- to get the next solution in Knight/Bishop/Rook state
nextNBRSoln :: [String] -> Piece -> (Int, Int, Int) -> (Int, Int, Int) -> [String]
nextNBRSoln prev_soln piece (prev_x,prev_y,prev_z) (x,y,z)
    | x - prev_x == 0 = prev_soln
    | x - prev_x == 1 = prev_soln ++ generate 1 piece
    | x - prev_x >= 2 = prev_soln ++ generate 2 piece

-- to make a guess in Knight/Bishop/Rook state
nextNBRGuess :: ([String],GameState) -> (Int, Int, Int) -> Piece -> [String]
nextNBRGuess (prev_guess, (size, piece, (prev_x,prev_y,prev_z), soln)) (x,y,z) next_piece = 
    let next_soln = nextNBRSoln soln piece (prev_x,prev_y,prev_z) (x,y,z) in 
        if size - length next_soln == 0 
            then next_soln
        else if size - length next_soln == 1 
            then next_soln ++ generate 1 next_piece
        else if next_piece == Piece Black Queen 
            then next_soln ++ generate 1 next_piece
        else 
            next_soln ++ generate 2 next_piece

-- to get the next solution in Queen/King state
nextQKSoln :: [String] -> Piece -> (Int, Int, Int) -> (Int, Int, Int) -> [String]
nextQKSoln prev_soln piece (prev_x,prev_y,prev_z) (x,y,z)
    | x - prev_x == 0 = prev_soln
    | x - prev_x == 1 = prev_soln ++ generate 1 piece

-- to make a guess in Queen/King state
nextQKGuess :: ([String],GameState) -> (Int, Int, Int) -> Piece -> [String]
nextQKGuess (prev_guess, (size, piece, (prev_x,prev_y,prev_z), soln)) (x,y,z) next_piece =   
    let next_soln = newQKSoln soln piece (prev_x,prev_y,prev_z) (x,y,z) in 
        if x - prev_x == 0 && y - prev_y == 0 && z - prev_z == 0 then next_soln
        else if size - length next_soln == 0 then next_soln
        else next_soln ++ generate 1 next_piece

-- black pawn state
bPawnGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
bPawnGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    = let new_guess = nextPawnGuess (prev_guess,(size, piece, prev_ans, soln)) new_ans
    in (new_guess, (size, Piece White Pawn, new_ans, nextPawnSoln new_ans))

-- white pawn state
wPawnGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
wPawnGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    = let new_guess = nextPawnGuess (prev_guess,(size, piece, prev_ans, soln)) new_ans
    in (new_guess, (size, Piece Black Knight, new_ans, soln))

-- black knight state
bKnightGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
bKnightGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    = let new_guess = nextNBRGuess (prev_guess,(size, piece, prev_ans, soln)) new_ans (Piece White Knight)
    in (new_guess, (size, Piece White Knight, new_ans, nextNBRSoln soln piece prev_ans new_ans))

-- white knight state
wKnightGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
wKnightGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    = let new_guess = nextNBRGuess (prev_guess,(size, piece, prev_ans, soln)) new_ans (Piece Black Bishop)
    in (new_guess, (size, Piece Black Bishop, new_ans, nextNBRSoln soln piece prev_ans new_ans))

-- black bishop state
bBishopGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
bBishopGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    = let new_guess = nextNBRGuess (prev_guess,(size, piece, prev_ans, soln)) new_ans (Piece White Bishop)
    in (new_guess, (size, Piece White Bishop, new_ans, nextNBRSoln soln piece prev_ans new_ans))

-- white bishop state
wBishopGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
wBishopGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    = let new_guess = nextNBRGuess (prev_guess,(size, piece, prev_ans, soln)) new_ans (Piece Black Rook)
    in (new_guess, (size, Piece Black Rook, new_ans, nextNBRSoln soln piece prev_ans new_ans))                

-- black rook state
bRookGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
bRookGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    = let new_guess = nextNBRGuess (prev_guess,(size, piece, prev_ans, soln)) new_ans (Piece White Rook)
        in (new_guess, (size, Piece White Rook, new_ans, nextNBRSoln soln piece prev_ans new_ans))

-- white rook state
wRookGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
wRookGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    = let new_guess = nextNBRGuess (prev_guess,(size, piece, prev_ans, soln)) new_ans (Piece Black Queen)
    in (new_guess, (size, Piece Black Queen, new_ans, nextNBRSoln soln piece prev_ans new_ans))
    
-- black queen state
bQueenGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
bQueenGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    = let new_guess = nextQKGuess (prev_guess,(size, piece, prev_ans, soln)) new_ans (Piece White Queen)
    in (new_guess, (size, Piece White Queen, new_ans, nextQKSoln soln piece prev_ans new_ans))

-- white queen state
wQueenGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
wQueenGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    = let new_guess = nextQKGuess (prev_guess,(size, piece, prev_ans, soln)) new_ans (Piece Black King)
    in (new_guess, (size, Piece Black King, new_ans, nextQKSoln soln piece prev_ans new_ans))

-- black king state
bKingGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
bKingGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    = let new_guess = nextQKGuess (prev_guess,(size, piece, prev_ans, soln)) new_ans (Piece White King)
    in (new_guess, (size, Piece White King, new_ans, nextQKSoln soln piece prev_ans new_ans))

-- white king state
wKingGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
wKingGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    = let new_guess = nextQKGuess (prev_guess,(size, piece, prev_ans, soln)) new_ans (Piece White King)
    in (new_guess, (size, Piece White King, new_ans, nextQKSoln soln piece prev_ans new_ans))

-- to enumerate through all 12 pieces state in linear order
nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
nextGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    | piece == Piece Black Pawn = 
        bPawnGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    | piece == Piece White Pawn = 
        wPawnGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    | piece == Piece Black Knight = 
        bKnightGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    | piece == Piece White Knight = 
        wKnightGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    | piece == Piece Black Bishop = 
        bBishopGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans   
    | piece == Piece White Bishop = 
        wBishopGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    | piece == Piece Black Rook = 
        bRookGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    | piece == Piece White Rook = 
        wRookGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    | piece == Piece Black Queen = 
        bQueenGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    | piece == Piece White Queen = 
        wQueenGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    | piece == Piece Black King = 
        bKingGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    | piece == Piece White King = 
        wKingGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans

---- Strategy
--
-- In the piece space, there are 12 distinct pieces in total. The Game State
-- stores size, current piece state, previous answer and solution, solution
-- will store previous pieces which confirmed to be correct. The method is 
-- enumerate through all 12 states, to put only the piece of current state
-- into a guess, if the answer returns there is a correct piece (which the
-- first part of the answer is greater than the previous one), then append 
-- to the solution of GameState.
--
-- There are three different transition functions for pawn, knight/bishop/rook
-- and queen/king respectively. Because pawn has 8 pieces, knight/bishop/rook
-- has 2 pieces and queen/king has only 1. They have different strategies to
-- make an optimal guess. For pawn, since the initial guess is all black pawn
-- pieces, in this case if the returning answer is (x,y,z) then x = number of 
-- black pawns, y = number of white pawns. For knight/bishop/rook, to append 
-- the piece up to size or 1 or 2 depends on the situation, if the returning 
-- answer (x,y,z) the x is greater than the x from previous answer then append 
-- to the solution of GameState. For queen/king is also similar but easier, 
-- since they only have one piece, just put one piece into the guess.
--
-- The worst case is 12 guesses regarless of size, the correctness and time 
-- complexity are relatively good since the strategy will not iterate through 
-- all possible targets.
--
