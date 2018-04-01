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

type GameState = (Int, Kind, (Int,Int,Int), [String])



initialGuess :: Int -> ([String],GameState)
initialGuess 0 = ([],(0, Pawn, (0,0,0),[]))
initialGuess size = let guess = if size <= 8 then generatePieces size (Piece Black Pawn) 
                                else generatePieces 8 (Piece Black Pawn) 
                    in
                    (guess, (size, Pawn, (0,0,0), []))

generatePieces :: Int -> Piece -> [String]
generatePieces 0 _ = []
generatePieces n piece = show piece : generatePieces (n-1) piece

pawnGuess :: ([String],GameState) -> (Int, Int, Int) -> [String]
pawnGuess (prev_guess, (size, kind, prev_ans, soln)) (x,y,z)
            | (size - x - y) < 2 = pawnSoln (x,y,z) ++ generatePieces 1 (Piece Black Knight) 
            | (size - x - y) == 2 = pawnSoln (x,y,z) ++ generatePieces 2 (Piece Black Knight)
            | (size - x - y) == 3 = pawnSoln (x,y,z) ++ generatePieces 2 (Piece Black Knight) ++ generatePieces 1 (Piece White Knight) 
            | (size - x - y) == 4 = pawnSoln (x,y,z) ++ generatePieces 2 (Piece Black Knight) ++ generatePieces 2 (Piece White Knight) 


pawnSoln :: (Int, Int, Int) -> [String]
pawnSoln (0,0,_) = []
pawnSoln (x,y,z)
            | x > 0 = show (Piece Black Pawn) : pawnSoln (x-1,y,z)
            | y > 0 = show (Piece White Pawn) : pawnSoln (x,y-1,z)
            
hasPiece :: Piece -> [String] -> Bool
hasPiece _ [] = False
hasPiece piece (g:guess) = if show piece == g 
                                then True 
                           else 
                                hasPiece piece guess

knightGuess :: ([String],GameState) -> (Int, Int, Int) -> [String]
knightGuess _ (0,0,0) = []
knightGuess (prev_guess, (size, kind, (prev_x,prev_y,prev_z), soln)) (x,y,z)
            | x - (prev_x + prev_y) == 1 && y == 1 = soln ++ generatePieces 1 (Piece Black Knight) ++ generatePieces 1 (Piece White Knight)
            | x - (prev_x + prev_y) == 1 = if hasPiece (Piece Black Knight) prev_guess 
                                                then soln ++ generatePieces 1 (Piece Black Knight) 
                                           else 
                                                soln ++ generatePieces 1 (Piece White Knight)
            | x - (prev_x + prev_y) == 2 && y == 1 = soln ++ generatePieces 1 (Piece Black Knight) ++ generatePieces 2 (Piece White Knight)
            | x - (prev_x + prev_y) == 2 && y == 0 = if hasPiece (Piece Black Knight) prev_guess 
                                                         then soln ++ generatePieces 2 (Piece Black Knight) 
                                                     else 
                                                         soln ++ generatePieces 2 (Piece White Knight)
            | x - (prev_x + prev_y) == 0 && y == 1 = soln ++ generatePieces 1 (Piece White Knight)
            | x - (prev_x + prev_y) == 0 && y == 2 = soln ++ generatePieces 2 (Piece White Knight)
            | x - (prev_x + prev_y) == 0 && y == 0 = if (size - x - y < 2)
                                                         then soln ++ generatePieces 1 (Piece Black Bishop)
                                                     else
                                                         soln ++ generatePieces 2 (Piece Black Bishop)

bishopGuess :: ([String],GameState) -> (Int, Int, Int) -> [String]
bishopGuess _ (0,0,0) = []
bishopGuess (prev_guess, (size, kind, (prev_x,prev_y,prev_z), soln)) (x,y,z) = []

nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
nextGuess (prev_guess, (size, kind, prev_ans, soln)) new_ans
            | kind == Pawn   = let new_guess = pawnGuess (prev_guess,(size, kind, prev_ans, soln)) new_ans
                               in (new_guess, (size, Knight, new_ans, pawnSoln new_ans))
            | kind == Knight = let new_guess = knightGuess (prev_guess,(size, kind, prev_ans, soln)) new_ans
                               in (new_guess, (size, Bishop, new_ans, soln))
            | kind == Bishop = let new_guess = bishopGuess (prev_guess,(size, kind, prev_ans, soln)) new_ans
                               in (new_guess, (size, Rook, new_ans, soln))
