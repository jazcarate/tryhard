module Data.Algebra.Free where


data FreeSemiGroup a = FreeSemiGroup a | FreeSemiGroupWith a (FreeSemiGroup a)

instance Semigroup (FreeSemiGroup a) where
  a <> b = case a of
    FreeSemiGroup a'         -> FreeSemiGroupWith a' b
    FreeSemiGroupWith a' fsm -> FreeSemiGroupWith a' (fsm <> b)

collapse :: (Semigroup b) => (a -> b) -> FreeSemiGroup a -> b
collapse f fsm = case fsm of
  FreeSemiGroup a          -> f a
  FreeSemiGroupWith a fsm' -> f a <> (collapse f fsm')
