module Collection where

import Probability

import qualified List (delete)



type Collection a = [a]

selectOne :: Eq a => Collection a -> Dist (a,Collection a)
selectOne c = uniform [(v,List.delete v c) | v <- c]

select1 :: Eq a => Collection a -> Dist a
select1 = mapD fst . selectOne

select2 :: Eq a => Collection a -> Dist (a,a)
select2 c = do (x,c') <- selectOne c
               y      <- select1 c'
               return (x,y)

selectMany :: Eq a => Int -> Collection a -> Dist ([a],Collection a)
selectMany 0 c = return ([],c)
selectMany n c = do (x,c1)  <- selectOne c
                    (xs,c2) <- selectMany (n-1) c1
                    return (x:xs,c2)

select :: Eq a => Int -> Collection a -> Dist [a]
select n = mapD (reverse . fst) . selectMany n


-- Example collections
-- 

-- marbles
-- 
data Marble = R | G | B deriving (Eq,Ord,Show)

bucket :: Collection Marble
bucket = [R,R,R,R,R, G,G,G, B,B]

jar :: Collection Marble
jar = [R,R,G,G,B]

-- pRGB = prob (just [R,G,B]) (select 3 bucket) 
pRGB = (==[R,G,B]) ?? select 3 jar
pRG  = (oneOf [[R,G],[G,R]]) ?? select 2 jar

-- cards
-- 
data Suit = Club | Spade | Heart | Diamond
            deriving (Eq,Ord,Show,Enum)

data Rank = Plain Int | Jack | Queen | King | Ace
            deriving (Eq,Ord,Show)
           
type Card = (Rank,Suit)

plains :: [Rank]
plains = map Plain [2..10]

faces :: [Rank]
faces = [Jack,Queen,King,Ace]

isFace :: Card -> Bool
isFace (r,_) = r `elem` faces
-- isFace = (`elem` faces) . fst

isPlain :: Card -> Bool
isPlain (r,_) = r `elem` plains

ranks :: [Rank]
ranks = plains ++ faces

suits :: [Suit]
suits = [Club,Spade,Heart,Diamond]

deck :: Collection Card
deck = [(r,s) | r <- ranks, s <- suits]


-- Example
-- mini-blackjack:
-- draw 2 cards, and if value is less than 14, continue drawing
-- until value equals or exceeds 14.  if values exceeds 21,
-- you lose, otherwise you win.

value :: Card -> Int
value ((Plain n),_) = n
value (Ace,_) = 11
value _ = 10

draw :: ([Card], Collection Card) -> Dist ([Card], Collection Card)
draw (cards,cl) = fmap f (selectOne cl)
	where
	f (c,cl') = ((c:cards),cl')

drawTo16 _ = while (\(cards,_)->(sum (map value cards) < 16))
	(random draw) ([], deck)

win (cards,_) 	| sum (map value cards) <= 21 = True
		| otherwise = False

chanceWin = fmap (mapD win) ((100 ~. drawTo16) undefined)
