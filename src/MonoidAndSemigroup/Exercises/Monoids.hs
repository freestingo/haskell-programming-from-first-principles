module MonoidAndSemigroup.Exercises.Monoids where

{-|
    Write the `Monoid` instance for our `Maybe` type, renamed to `Optional`.

    NOTE: base-4.11.0.0 introduced breaking changes to the Monoid class;
    that's why it's also necessary to implement Semigroup.

    https://stackoverflow.com/questions/52237895/could-not-deduce-semigroup-optional-a-arising-from-the-superclasses-of-an-in
-}
data Optional a = Nada | Only a
                  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada

instance Semigroup a => Semigroup (Optional a) where
    Nada <> Nada = Nada
    Nada <> y = y
    x <> Nada = x
    (Only x) <> (Only y) = Only $ x <> y

