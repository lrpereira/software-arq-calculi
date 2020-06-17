{-# LANGUAGE FlexibleInstances #-}
module Adventurers where

import Control.Monad.Zip
import Data.List.Extra
import Extra
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

-- The final state of the game
gFinal :: State
gFinal = const True

-- Changes the state of the game for a given object
changeState :: Objects -> State -> State
changeState a s = let v = s a
                  in (\x -> if x == a then not v else s x)

-- Changes the state of the game of a list of objects
mChangeState :: [Objects] -> State -> State
mChangeState os s = if allSame (fmap s os)
                    then foldr changeState s os
                    else s

{-- For a given state of the game, the function presents all the
possible moves that the adventurers can make.  --}
allValidPlays :: State -> ListDur State
allValidPlays x =
    manyChoice[ waitP P1  $ return $ mChangeState [Left P1, Right ()] x,
                waitP P2  $ return $ mChangeState [Left P2, Right ()] x,
                waitP P5  $ return $ mChangeState [Left P5, Right ()] x,
                waitP P10 $ return $ mChangeState [Left P10, Right ()] x,
                waitP P2  $ return $ mChangeState [Left P1, Left P2, Right ()] x,
                waitP P5  $ return $ mChangeState [Left P1, Left P5, Right ()] x,
                waitP P10 $ return $ mChangeState [Left P1, Left P10, Right ()] x,
                waitP P5  $ return $ mChangeState [Left P2, Left P5, Right ()] x,
                waitP P10 $ return $ mChangeState [Left P2, Left P10, Right ()] x,
                waitP P10 $ return $ mChangeState [Left P5, Left P10, Right ()] x]

{-- For a given number n and initial state, the function calculates
all possible n-sequences of moves that the adventures can make --}
exec :: Int -> State -> ListDur State
exec 0 s = return s
exec n s = do s1 <- allValidPlays s
              exec (n - 1) s1

{-- Is it possible for all adventurers to be on the other side
in <=17 min and not exceeding 5 moves ? --}
-- To implement DONE
leq17 :: Bool
leq17 = case find (\(Duration (s,x)) -> s <= 17 && x == gFinal) (remLD (exec 5 gInit)) of
        Nothing -> False
        Just _ -> True

{-- Is it possible for all adventurers to be on the other side
in < 17 min ? --}
l17 :: Bool
l17 = case find (\(Duration (s,x)) -> s < 17 && x == gFinal) (remLD (exec 6 gInit)) of
        Nothing -> False
        Just _ -> True

{----------------------------------- Monad ListDur -----------------------------------}

data ListDur a = LD [Duration a] deriving Show

remLD :: ListDur a -> [Duration a]
remLD (LD x) = x

instance Functor ListDur where
    fmap f = LD . (map f') . remLD
        where
          f' = \(Duration (i, x)) -> (Duration (i, f x))

instance Applicative ListDur where
  pure x = LD [Duration (0, x)]
  l1 <*> l2 = LD $ mzipWith f (remLD l1) (remLD l2)
    where
      f = \(Duration (a, f)) (Duration (b, x)) -> Duration (a + b, f x)

-- instance Applicative ListDur where
--    pure x = LD [(Duration (0,x))]
--    (<*>) = LD . mzipWith f where
--        f = \(Duration (a,f)) (Duration (b,x)) -> Duration (a+b,f x)

instance Monad ListDur where
  return = pure
  l >>= k = LD $
    do
      x <- remLD l
      g x
    where
      g (Duration (s, x)) = let u = (remLD (k x)) in map (\(Duration (s', x)) -> (Duration (s + s', x))) u


manyChoice :: [ListDur a] -> ListDur a
manyChoice = LD . concat . (map remLD)

-- Wait time for P
waitP :: Adventurer -> ListDur State -> ListDur State
waitP p (LD [d]) = LD [(wait (getTimeAdv p) d)]

{----------------------------------- Monad LogListDur -----------------------------------}

data LogListDur a = L [(String, Duration a)] deriving Show

remL :: LogListDur a -> [(String, Duration a)]
remL (L s) = s

instance Functor LogListDur where
    fmap f (L l) = L $ map (id >< Duration . (id >< f) . remD ) l

instance Applicative LogListDur where
    pure s = (L . singl) ([], Duration (0, s))
    (<*>) l1 l2 = L $ do
                    fs <- remL l1
                    xs <- remL l2
                    gs(fs,xs) where
                        gs((str1, Duration (d1,g)) , (str2, Duration (d2,x))) = return ((str1 ++ str2) , Duration (d1 + d2 , g x))

-- Is this alternative right ?
-- (<*>) (L f) (L x) = L $ mzipWith g f x where
--                             g (str1 , (Duration (d1,f))) (str2 , Duration (d2,x)) = (str1 ++ str2 , Duration (d1+d2, f x))

instance Monad LogListDur where
    return  = pure
    l >>= k = L $ do
                xs <- remL l
                g xs where
                    g (str , Duration (d,x)) = map (\(str', Duration (d',x)) -> (str ++ str' , (Duration (d + d' , x)))) u where
                          u = remL (k x)

manyLChoice :: [LogListDur a] -> LogListDur a
manyLChoice = L . concat . (map remL)

mwrite :: String -> LogListDur a -> LogListDur a
mwrite msg l = L $ let l' = remL l in map (\(s,x) -> (s ++ msg, x)) l'

lwaitP :: Adventurer -> LogListDur State -> LogListDur State
lwaitP p (L [(str , d)]) = L [(str, wait (getTimeAdv p) d)]

lvalidPlays :: State -> LogListDur State
lvalidPlays s =
    manyLChoice[ mwrite (" "++(show s)++" ") (lwaitP P1  $ return $ mChangeState [Left P1, Right ()] s),
                 mwrite (" "++(show s)++" ") (lwaitP P2  $ return $ mChangeState [Left P2, Right ()] s),
                 mwrite (" "++(show s)++" ") (lwaitP P5  $ return $ mChangeState [Left P5, Right ()] s),
                 mwrite (" "++(show s)++" ") (lwaitP P10 $ return $ mChangeState [Left P10, Right ()] s),
                 mwrite (" "++(show s)++" ") (lwaitP P2  $ return $ mChangeState [Left P1, Left P2, Right ()] s),
                 mwrite (" "++(show s)++" ") (lwaitP P5  $ return $ mChangeState [Left P1, Left P5, Right ()] s),
                 mwrite (" "++(show s)++" ") (lwaitP P10 $ return $ mChangeState [Left P1, Left P10, Right ()] s),
                 mwrite (" "++(show s)++" ") (lwaitP P5  $ return $ mChangeState [Left P2, Left P5, Right ()] s),
                 mwrite (" "++(show s)++" ") (lwaitP P10 $ return $ mChangeState [Left P2, Left P10, Right ()] s),
                 mwrite (" "++(show s)++" ") (lwaitP P10 $ return $ mChangeState [Left P5, Left P10, Right ()] s)]

lexec :: Int -> State -> LogListDur State
lexec 0 s = return s
lexec n s = do { s1 <- lvalidPlays s ; lexec (n-1) s1 }

lleq17 = find (\(log , Duration (d , s)) -> d == 17 && s == gFinal) (remL (lexec 5 gInit))

ll17 = find (\(log , Duration (d , s)) -> d < 17 && s == gFinal) (remL (lexec 5 gInit))
