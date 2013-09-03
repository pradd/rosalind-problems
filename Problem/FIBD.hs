{-
Mortal rabbits with k = 1.

Given: Positive integers n≤100 and m≤20.

Return: The total number of pairs of rabbits that will remain after the n-th month if all rabbits live for m months.

Sample Dataset
6 3
Sample Output
4

-}


module Problem.FIBD where

test :: Bool
test = (process 6 3) == 4

process :: Int -> Int -> Integer
process n m = countRabbits $ rabbits n m  

countRabbits :: [RabbitGroup] -> Integer
countRabbits = foldl (+) 0 . map num

data Maturity = Newborn | Young | Mature deriving (Show, Eq)
data RabbitGroup = RabbitGroup {num :: Integer, ttl :: Int, maturity :: Maturity} deriving Show

rabbits :: Int -> Int -> [RabbitGroup]
rabbits 1 m = [(RabbitGroup 1 (m-1) Young)]
rabbits n m = die . age . mature . breed m $ rabbits (n - 1) m

mature :: [RabbitGroup] -> [RabbitGroup]
mature = map (\g -> g {maturity = up (maturity g)})
    where   up Newborn = Young
            up Young   = Mature
            up Mature  = Mature

age :: [RabbitGroup] -> [RabbitGroup]
age = map (\g -> g {ttl = (ttl g) - 1})

breed :: Int -> [RabbitGroup] -> [RabbitGroup]
breed m rgs = (RabbitGroup newbornCount m Newborn) : rgs
    where   newbornCount = countRabbits $ filter (\x -> maturity x == Mature) rgs

die :: [RabbitGroup] -> [RabbitGroup]
die = filter (\g -> ttl g >= 0)




