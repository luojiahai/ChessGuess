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

instance Show Piece where
    show (Piece c k) = show c ++ show k

type GameState = (Int, Piece, (Int,Int,Int), [String])


initialGuess :: Int -> ([String],GameState)
initialGuess 0 = ([],(0, Piece Black Pawn, (0,0,0),[]))
initialGuess size = let guess = if size <= 8 then generate size (Piece Black Pawn) else generate 8 (Piece Black Pawn) 
                    in (guess, (size, Piece Black Pawn, (0,0,0), []))

generate :: Int -> Piece -> [String]
generate 0 _ = []
generate n piece = show piece : generate (n-1) piece

nextPawnSoln :: (Int, Int, Int) -> [String]
nextPawnSoln (x,y,z) = 
    generate x (Piece Black Pawn) ++ generate y (Piece White Pawn)

nextPawnGuess :: ([String],GameState) -> (Int, Int, Int) -> [String]
nextPawnGuess (prev_guess, (size, piece, prev_ans, soln)) (x,y,z) 
    | piece == Piece Black Pawn = nextPawnSoln (x,y,z)
    | piece == Piece White Pawn = 
        if size - length soln < 2 
        then soln ++ generate 1 (Piece Black Knight) 
        else soln ++ generate 2 (Piece Black Knight)

nextNBRSoln :: [String] -> Piece -> (Int, Int, Int) -> (Int, Int, Int) -> [String]
nextNBRSoln prev_soln piece (prev_x,prev_y,prev_z) (x,y,z)
    | x - prev_x == 0 = prev_soln
    | x - prev_x == 1 = prev_soln ++ generate 1 piece
    | x - prev_x >= 2 = prev_soln ++ generate 2 piece

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
            
newQKSoln :: [String] -> Piece -> (Int, Int, Int) -> (Int, Int, Int) -> [String]
newQKSoln prev_soln piece (prev_x,prev_y,prev_z) (x,y,z)
    | x - prev_x == 0 = prev_soln
    | x - prev_x == 1 = prev_soln ++ generate 1 piece

nextQKGuess :: ([String],GameState) -> (Int, Int, Int) -> Piece -> [String]
nextQKGuess (prev_guess, (size, piece, (prev_x,prev_y,prev_z), soln)) (x,y,z) next_piece =   
    let next_soln = newQKSoln soln piece (prev_x,prev_y,prev_z) (x,y,z) in 
        if x - prev_x == 0 && y - prev_y == 0 && z - prev_z == 0 then next_soln
        else if size - length next_soln == 0 then next_soln
        else next_soln ++ generate 1 next_piece

bPawnGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
bPawnGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    = let new_guess = nextPawnGuess (prev_guess,(size, piece, prev_ans, soln)) new_ans
    in (new_guess, (size, Piece White Pawn, new_ans, nextPawnSoln new_ans))

wPawnGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
wPawnGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    = let new_guess = nextPawnGuess (prev_guess,(size, piece, prev_ans, soln)) new_ans
    in (new_guess, (size, Piece Black Knight, new_ans, soln))

bKnightGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
bKnightGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    = let new_guess = nextNBRGuess (prev_guess,(size, piece, prev_ans, soln)) new_ans (Piece White Knight)
    in (new_guess, (size, Piece White Knight, new_ans, nextNBRSoln soln piece prev_ans new_ans))

wKnightGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
wKnightGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    = let new_guess = nextNBRGuess (prev_guess,(size, piece, prev_ans, soln)) new_ans (Piece Black Bishop)
    in (new_guess, (size, Piece Black Bishop, new_ans, nextNBRSoln soln piece prev_ans new_ans))

bBishopGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
bBishopGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    = let new_guess = nextNBRGuess (prev_guess,(size, piece, prev_ans, soln)) new_ans (Piece White Bishop)
    in (new_guess, (size, Piece White Bishop, new_ans, nextNBRSoln soln piece prev_ans new_ans))

wBishopGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
wBishopGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    = let new_guess = nextNBRGuess (prev_guess,(size, piece, prev_ans, soln)) new_ans (Piece Black Rook)
    in (new_guess, (size, Piece Black Rook, new_ans, nextNBRSoln soln piece prev_ans new_ans))                

bRookGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
bRookGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    = let new_guess = nextNBRGuess (prev_guess,(size, piece, prev_ans, soln)) new_ans (Piece White Rook)
        in (new_guess, (size, Piece White Rook, new_ans, nextNBRSoln soln piece prev_ans new_ans))

wRookGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
wRookGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    = let new_guess = nextNBRGuess (prev_guess,(size, piece, prev_ans, soln)) new_ans (Piece Black Queen)
    in (new_guess, (size, Piece Black Queen, new_ans, nextNBRSoln soln piece prev_ans new_ans))
    
bQueenGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
bQueenGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    = let new_guess = nextQKGuess (prev_guess,(size, piece, prev_ans, soln)) new_ans (Piece White Queen)
    in (new_guess, (size, Piece White Queen, new_ans, newQKSoln soln piece prev_ans new_ans))

wQueenGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
wQueenGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    = let new_guess = nextQKGuess (prev_guess,(size, piece, prev_ans, soln)) new_ans (Piece Black King)
    in (new_guess, (size, Piece Black King, new_ans, newQKSoln soln piece prev_ans new_ans))

bKingGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
bKingGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    = let new_guess = nextQKGuess (prev_guess,(size, piece, prev_ans, soln)) new_ans (Piece White King)
    in (new_guess, (size, Piece White King, new_ans, newQKSoln soln piece prev_ans new_ans))

wKingGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
wKingGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    = let new_guess = nextQKGuess (prev_guess,(size, piece, prev_ans, soln)) new_ans (Piece White King)
    in (new_guess, (size, Piece White King, new_ans, newQKSoln soln piece prev_ans new_ans))

nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
nextGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    | piece == Piece Black Pawn = bPawnGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    | piece == Piece White Pawn = wPawnGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    | piece == Piece Black Knight = bKnightGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    | piece == Piece White Knight = wKnightGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    | piece == Piece Black Bishop = bBishopGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans   
    | piece == Piece White Bishop = wBishopGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    | piece == Piece Black Rook = bRookGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    | piece == Piece White Rook = wRookGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    | piece == Piece Black Queen = bQueenGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    | piece == Piece White Queen = wQueenGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    | piece == Piece Black King = bKingGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
    | piece == Piece White King = wKingGuess (prev_guess, (size, piece, prev_ans, soln)) new_ans
