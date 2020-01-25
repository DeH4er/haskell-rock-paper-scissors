module Logic where

data Choice = Rock
            | Paper
            | Scissors
              deriving (Read, Show, Eq)

instance Ord Choice where
  Rock     `compare` Scissors = GT
  Scissors `compare` Paper    = GT
  Paper    `compare` Rock     = GT
  x `compare` y | x == y      = EQ
                | otherwise   = LT


ordToWin :: Ordering -> String
ordToWin GT = "You win!"
ordToWin EQ = "Draw"
ordToWin LT = "You loose!"
