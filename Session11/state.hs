-- Defining states and state transformers


newtype State = SE [Int] deriving Show

newtype ST a = ST (State -> (a, State))

app :: ST a -> State -> (a,State)

app (ST st) x = st x


-- Defining the ST monad

-- We must first make ST a functor

instance Functor ST where
  -- fmap :: (a->b) -> ST(a->b)
   fmap g st = ST (\s -> let (x,s') = app st s in (g x,s'))

-- Then we must make ST an applicative functor

instance Applicative ST where
  -- pure :: a -> ST a
  pure x = ST (\s -> (x,s))

  -- <*> :: ST (a->b) -> ST a -> ST b

  stf <*> stx = ST (\s ->
                  let (f,s')  = app stf s
                      (x,s'') = app stx s' in (f x,s''))

-- Finally, we can make ST a monad; we only need to define >>= as return is simply the -- pure function

instance Monad ST where
  -- >>= :: a -> (a -> ST b) -> ST b
  st >>= f = ST (\s ->
                let (x,s') = app st s in app (f x) s')
