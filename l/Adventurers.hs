{-# LANGUAGE FlexibleInstances #-}
module Adventurers where

import Control.Monad.Zip
import Data.List
import Data.List.Extra
import DurationMonad
    
-- The list of adventurers
data Adventurer = P1 | P2 | P5 | P10 deriving (Show,Eq)

-- Adventurers + the lantern
type Objects = Either Adventurer ()

-- The time that each adventurer needs to cross the bridge
-- To implement
getTimeAdv :: Adventurer -> Int
getTimeAdv P1 = 1
getTimeAdv P2 = 2
getTimeAdv P5 = 5
getTimeAdv P10 = 10

{-- The state of the game, i.e. the current position of each adventurer
+ the lantern. The function (const False) represents the initial state
of the game, with all adventurers and the lantern on the left side of
the bridge. Similarly, the function (const True) represents the end
state of the game, with all adventurers and the lantern on the right
side of the bridge.  --}
type State = Objects -> Bool

instance Show State where
  show s = (show . (fmap show)) [s (Left P1),
                                 s (Left P2),
                                 s (Left P5),
                                 s (Left P10),
                                 s (Right ())]

instance Eq State where
  (==) s1 s2 = and [s1 (Left P1) == s2 (Left P1),
                    s1 (Left P2) == s2 (Left P2),
                    s1 (Left P5) == s2 (Left P5),
                    s1 (Left P10) == s2 (Left P10),
                    s1 (Right ()) == s2 (Right ())]



-- The initial state of the game
gInit :: State
gInit = const False

-- Changes the state of the game for a given object
changeState :: Objects -> State -> State
changeState a s = let v = s a in (\x -> if x == a then not v else s x)

-- Changes the state of the game of a list of objects
mChangeState :: [Objects] -> State -> State
mChangeState os s = if allSame (fmap s os) then foldr changeState s os else s 

{-- For a given state of the game, the function presents all the
possible moves that the adventurers can make.  --}
-- To implement
allValidPlays :: State -> ListDur State
allValidPlays x = manyChoice[
  waitP1 $ return $ mChangeState [Left P1, Right ()] x,
  waitP2 $ return $ mChangeState [Left P2, Right ()] x,
  waitP5 $ return $ mChangeState [Left P5, Right ()] x,
  waitP10 $ return $ mChangeState [Left P10, Right ()] x,
  waitP2 $ return $ mChangeState [Left P1, Left P2, Right ()] x,
  waitP5 $ return $ mChangeState [Left P1, Left P5, Right ()] x,
  waitP10 $ return $ mChangeState [Left P1, Left P10, Right ()] x,
  waitP5 $ return $ mChangeState [Left P2, Left P5, Right ()] x,
  waitP10 $ return $ mChangeState [Left P2, Left P10, Right ()] x,
  waitP10 $ return $ mChangeState [Left P5, Left P10, Right ()] x]


{-- For a given number n and initial state, the function calculates
all possible n-sequences of moves that the adventures can make --}
-- To implement
exec :: Int -> State -> ListDur State
exec n s = do s1 <- allValidPlays (s)
              s2 <- allValidPlays (s1)
              s3 <- allValidPlays (s2)
              s4 <- allValidPlays (s3)
              s5 <- allValidPlays (s4)
              return s5

--exec 1 s = do s1 <- allValidPlays (s)
--              return s1
--exec n s = do s1 <- allValidPlays (s)
--             return (exec n-1 (remLD s1))

{-- Is it possible for all adventurers to be on the other side
in <=17 min and not exceeding 5 moves ? --}
-- To implement
leq17 :: Bool
leq17 = case find (\(Duration (s,x)) -> s <= 17 && x == gFinal) (remLD (exec 5 gInit)) of
        Nothing -> False
        Just _ -> True


{-- Is it possible for all adventurers to be on the other side
in < 17 min ? --}
-- To implement
l17 :: Bool
l17 = case find (\(Duration (s,x)) -> s < 17 && x == gFinal) (remLD (exec 100 gInit)) of
        Nothing -> False
        Just _ -> True
 

-- Our definitions

-- 
allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame (x:xs) = all (x ==) xs

-- Wait time for P1
waitP1 :: ListDur State -> ListDur State
waitP1 (LD [(Duration (d,x))]) = LD [Duration (d+(getTimeAdv P1),x)]

-- Wait time for P2
waitP2 :: ListDur State -> ListDur State
waitP2 (LD [(Duration (d,x))]) = LD [Duration (d+(getTimeAdv P2),x)]

-- Wait time for P5
waitP5 :: ListDur State -> ListDur State
waitP5 (LD [(Duration (d,x))]) = LD [Duration (d+(getTimeAdv P5),x)]

-- Wait time for P10
waitP10 :: ListDur State -> ListDur State
waitP10 (LD [(Duration (d,x))]) = LD [Duration (d+(getTimeAdv P10),x)]

-- The final state of the game
gFinal :: State
gFinal = const True

-- Determines whether the adventurers arrived at the other side of the bridge or not
ltargetAchieved :: State -> ListDur State -> Maybe (Duration State)
ltargetAchieved t l = let l' = remLD l in find (\(Duration (s,x)) -> x == t) l'

--------------------------------------------------------------------------
{-- Implementation of the monad used for the problem of the adventurers.
Recall the Knight's quest --}

data ListDur a = LD [Duration a] deriving Show

remLD :: ListDur a -> [Duration a]
remLD (LD x) = x

-- To implement DONE
instance Functor ListDur where
    fmap f = LD . (map f') . remLD
        where f' = \(Duration (i,x)) -> (Duration (i, f x))

-- To implement
instance Applicative ListDur where
   pure x = LD [(Duration (0,x))]
   (<*>) l1 l2 = LD $ mzipWith f (remLD l1) (remLD l2) where
                             f = \(Duration (a,f)) (Duration (b,x)) -> Duration (a+b,f x)

-- instance Applicative ListDur where
--    pure x = LD [(Duration (0,x))]
--    (<*>) = LD . mzipWith f where
--        f = 
--            \(Duration (a,f)) (Duration (b,x)) -> Duration (a+b,f x)                                 

-- To implement
instance Monad ListDur where
  return = pure
  l >>= k = LD $ do x <- remLD l
                    g x where
                      g(Duration (s,x)) = let u = (remLD (k x)) in map (\(Duration (s',x)) -> (Duration (s + s', x))) u


manyChoice :: [ListDur a] -> ListDur a
manyChoice = LD . concat . (map remLD)
--------------------------------------------------------------------------
