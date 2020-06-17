-- | Lucas Pereira - Package compiled from CP - José Nuno Oliveira

module Extra where

infix 5  ><
infix 4  -|-

-- Basic functions, abbreviations

bang = (!)

dup = split id id

zero = const 0

one  = const 1

nil = const []

cons = uncurry (:)

add = uncurry (+)

mul = uncurry (*)

conc = uncurry (++)

true = const True

nothing = const Nothing

false = const False

inMaybe :: Either () a -> Maybe a
inMaybe = either (const Nothing) Just

{------ Bang ----------------------------------------------------------------}
(!) :: a -> ()
(!) = const ()

{------ Product -------------------------------------------------------------}
-- Product
p1      = fst
p2      = snd

split :: (a -> b) -> (a -> c) -> a -> (b,c)
split f g x = (f x, g x)

(><) :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
f >< g = split (f . p1) (g . p2)

{------ Coproduct -----------------------------------------------------------}
i1      = Left
i2      = Right

-- either is predefined
(-|-) :: (a -> b) -> (c -> d) -> Either a c -> Either b d
f -|- g = either (i1 . f) (i2 . g)

-- guards
grd :: (a -> Bool) -> a -> Either a a
grd p x = if p x then Left x else Right x

-- McCarthy's conditional:
cond p f g = (either f g) . (grd p)

{------ Lists ---------------------------------------------------------------}
singl :: a -> [a]
singl = return
