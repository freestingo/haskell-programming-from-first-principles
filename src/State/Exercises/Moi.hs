{-# LANGUAGE TupleSections #-}
module State.Exercises.Moi where

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
    fmap f (Moi g) = Moi $ \s -> let (a, ns) = g s
                                 in (f a, ns)

instance Applicative (Moi s) where
    pure a = Moi (a,)

    (Moi f) <*> (Moi g) =
        Moi $ \s -> let (a, ns) = g s
                        aToB = fst $ f s
                    in (aToB a, ns)

instance Monad (Moi s) where
    return = pure

    (Moi f) >>= g = Moi $ \s -> let (a, s') = f s
                                    (Moi sb) = g a
                                in sb s

get :: Moi s s
get = Moi $ \s -> (s, s)

put :: s -> Moi s ()
put s = Moi ((),)

exec :: Moi s a -> s -> s
exec (Moi sa) s = snd $ sa s

eval :: Moi s a -> s -> a
eval (Moi sa) s = fst $ sa s

modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> ((), f s)

