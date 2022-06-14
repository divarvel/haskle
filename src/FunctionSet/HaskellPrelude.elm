module FunctionSet.HaskellPrelude exposing (haskellPrelude)

haskellPrelude : List String
haskellPrelude =
  [ "(!!) :: List a -> Int -> a"
  , "($) :: (a -> b) -> a -> b"
  , "($!) :: (a -> b) -> a -> b"
  , "(&&) :: Bool -> Bool -> Bool"
  , "(++) :: List a -> List a -> List a"
  , "(.) :: (b -> c) -> (a -> b) -> a -> c"
  , "(<$>) :: Functor f => (a -> b) -> f a -> f b"
  , "(=<<) :: Monad m => (a -> m b) -> m a -> m b"
  , "pure :: Applicative f => a -> f a"
  , "(<*>) :: Applicative f => f (a -> b) -> f a -> f b"
  , "liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c"
  , "(*>) :: Applicative f => f a -> f b -> f b"
  , "(<*) :: Applicative f => f a -> f b -> f a"
  , "minBound :: Bounded a => a"
  , "maxBound :: Bounded a => a"
  , "succ :: Enum a => a -> a"
  , "pred :: Enum a => a -> a"
  , "toEnum :: Enum a => Int -> a"
  , "fromEnum :: Enum a => a -> Int"
  , "enumFrom :: Enum a => a -> List a"
  , "enumFromThen :: Enum a => a -> a -> List a"
  , "enumFromTo :: Enum a => a -> a -> List a"
  , "enumFromThenTo :: Enum a => a -> a -> a -> List a"
  , "(==) :: Eq a => a -> a -> Bool"
  , "(/=) :: Eq a => a -> a -> Bool"
  , "pi :: Floating a => a"
  , "exp :: Floating a => a -> a"
  , "log :: Floating a => a -> a"
  , "sqrt :: Floating a => a -> a"
  , "(**) :: Floating a => a -> a -> a"
  , "logBase :: Floating a => a -> a -> a"
  , "sin :: Floating a => a -> a"
  , "cos :: Floating a => a -> a"
  , "tan :: Floating a => a -> a"
  , "asin :: Floating a => a -> a"
  , "acos :: Floating a => a -> a"
  , "atan :: Floating a => a -> a"
  , "sinh :: Floating a => a -> a"
  , "cosh :: Floating a => a -> a"
  , "tanh :: Floating a => a -> a"
  , "asinh :: Floating a => a -> a"
  , "acosh :: Floating a => a -> a"
  , "atanh :: Floating a => a -> a"
  , "log1p :: Floating a => a -> a"
  , "expm1 :: Floating a => a -> a"
  , "log1pexp :: Floating a => a -> a"
  , "log1mexp :: Floating a => a -> a"
  , "fold :: Foldable t => Monoid m => t m -> m"
  , "foldMap :: Foldable t => Monoid m => (a -> m) -> t a -> m"
  , "foldMap' :: Foldable t => Monoid m => (a -> m) -> t a -> m"
  , "foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b"
  , "foldr' :: Foldable t => (a -> b -> b) -> b -> t a -> b"
  , "foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b"
  , "foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b"
  , "foldr1 :: Foldable t => (a -> a -> a) -> t a -> a"
  , "foldl1 :: Foldable t => (a -> a -> a) -> t a -> a"
  , "toList :: Foldable t => t a -> List a"
  , "null :: Foldable t => t a -> Bool"
  , "length :: Foldable t => t a -> Int"
  , "elem :: Foldable t => Eq a => a -> t a -> Bool"
  , "maximum :: Foldable t => Ord a => t a -> a"
  , "minimum :: Foldable t => Ord a => t a -> a"
  , "sum :: Foldable t => Num a => t a -> a"
  , "product :: Foldable t => Num a => t a -> a"
  , "(/) :: Fractional a => a -> a -> a"
  , "recip :: Fractional a => a -> a"
  , "fromRational :: Fractional a => Rational -> a"
  , "fmap :: Functor f => (a -> b) -> f a -> f b"
  , "(<$) :: Functor f => a -> f b -> f a"
  , "quot :: Integral a => a -> a -> a"
  , "rem :: Integral a => a -> a -> a"
  , "div :: Integral a => a -> a -> a"
  , "mod :: Integral a => a -> a -> a"
  , "quotRem :: Integral a => a -> a -> Tuple a a"
  , "divMod :: Integral a => a -> a -> Tuple a a"
  , "toInteger :: Integral a => a -> Integer"
  , "(>>=) :: Monad m => m a -> (a -> m b) -> m b"
  , "(>>) :: Monad m => m a -> m b -> m b"
  , "return :: Monad m => a -> m a"
  , "fail :: MonadFail => String -> m a"
  , "mempty :: Monoid a => a"
  , "mappend :: Monoid a => a -> a -> a"
  , "mconcat :: Monoid a => List a -> a"
  , "(+) :: Num a => a -> a -> a"
  , "(-) :: Num a => a -> a -> a"
  , "(*) :: Num a => a -> a -> a"
  , "negate :: Num a => a -> a"
  , "abs :: Num a => a -> a"
  , "signum :: Num a => a -> a"
  , "fromInteger :: Num a => Integer -> a"
  , "compare :: Ord a => a -> a -> Ordering"
  , "(<) :: Ord a => a -> a -> Bool"
  , "(<=) :: Ord a => a -> a -> Bool"
  , "(>) :: Ord a => a -> a -> Bool"
  , "(>=) :: Ord a => a -> a -> Bool"
  , "max :: Ord a => a -> a -> a"
  , "min :: Ord a => a -> a -> a"
  , "readsPrec :: Read a => Int -> ReadS a"
  , "readList :: Read a => ReadS (List a)"
  , "toRational :: Real a => a -> Rational"
  , "floatRadix :: RealFloat a => a -> Integer"
  , "floatDigits :: RealFloat a => a -> Int"
  , "floatRange :: RealFloat a => a -> Tuple Int Int"
  , "decodeFloat :: RealFloat a => a -> Tuple Integer Int"
  , "encodeFloat :: RealFloat a => Integer -> Int -> a"
  , "exponent :: RealFloat a => a -> Int"
  , "significand :: RealFloat a => a -> a"
  , "scaleFloat :: RealFloat a => Int -> a -> a"
  , "isNaN :: RealFloat a => a -> Bool"
  , "isInfinite :: RealFloat a => a -> Bool"
  , "isDenormalized :: RealFloat a => a -> Bool"
  , "isNegativeZero :: RealFloat a => a -> Bool"
  , "isIEEE :: RealFloat a => a -> Bool"
  , "atan2 :: RealFloat a => a -> a -> a"
  , "properFraction :: RealFrac a => Integral b => a -> Tuple b a"
  , "truncate :: RealFrac a => Integral b => a -> b"
  , "round :: RealFrac a => Integral b => a -> b"
  , "ceiling :: RealFrac a => Integral b => a -> b"
  , "floor :: RealFrac a => Integral b => a -> b"
  , "(<>) :: Semigroup a => a -> a -> a"
  , "sconcat :: Semigroup a => NonEmpty a -> a"
  , "stimes :: Semigroup a => Integral b => b -> a -> a"
  , "showsPrec :: Show a => Int -> a -> ShowS"
  , "show :: Show a => a -> String"
  , "showList :: Show a => List a -> ShowS"
  , "traverse :: Traversable t => Applicative f => (a -> f b) -> t a -> f (t b)"
  , "sequenceA :: Traversable t => Applicative f => t (f a) -> f (t a)"
  , "mapM :: Traversable t => Monad m => (a -> m b) -> t a -> m (t b)"
  , "sequence :: Traversable t => Monad m => t (m a) -> m (t a)"
  , "(^) :: (Num a, Integral b) => a -> b -> a"
  , "(^^) :: (Fractional a, Integral b) => a -> b -> a"
  , "all :: Foldable t => (a -> Bool) -> t a -> Bool"
  , "and :: Foldable t => t Bool -> Bool"
  , "any :: Foldable t => (a -> Bool) -> t a -> Bool"
  , "appendFile :: FilePath -> String -> IO Unit"
  , "asTypeOf :: a -> a -> a"
  , "break :: (a -> Bool) -> List a -> Tuple (List a) (List a)"
  , "concat :: Foldable t => t (List a) -> List a"
  , "concatMap :: Foldable t => (a -> List b) -> t a -> List b"
  , "const :: a -> b -> a"
  , "curry :: ((Tuple a b) -> c) -> a -> b -> c"
  , "cycle :: List a -> List a"
  , "drop :: Int -> List a -> List a"
  , "dropWhile :: (a -> Bool) -> List a -> List a"
  , "either :: (a -> c) -> (b -> c) -> Either a b -> c"
  , "error :: HasCallStack => List Char -> a"
  , "errorWithoutStackTrace :: List Char -> a"
  , "even :: Integral a => a -> Bool"
  , "filter :: (a -> Bool) -> List a -> List a"
  , "flip :: (a -> b -> c) -> b -> a -> c"
  , "fromIntegral :: (Integral a, Num b) => a -> b"
  , "fst :: Tuple a b -> a"
  , "gcd :: Integral a => a -> a -> a"
  , "getChar :: IO Char"
  , "getContents :: IO String"
  , "getLine :: IO String"
  , "head :: List a -> a"
  , "id :: a -> a"
  , "init :: List a -> List a"
  , "interact :: (String -> String) -> IO Unit"
  , "ioError :: IOError -> IO a"
  , "iterate :: (a -> a) -> a -> List a"
  , "last :: List a -> a"
  , "lcm :: Integral a => a -> a -> a"
  , "lex :: ReadS String"
  , "lines :: String -> List String"
  , "lookup :: Eq a => a -> List (Tuple a b) -> Maybe b"
  , "map :: (a -> b) -> List a -> List b"
  , "mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m Unit"
  , "maybe :: b -> (a -> b) -> Maybe a -> b"
  , "not :: Bool -> Bool"
  , "notElem :: (Foldable t, Eq a) => a -> t a -> Bool"
  , "odd :: Integral a => a -> Bool"
  , "or :: Foldable t => t Bool -> Bool"
  , "otherwise :: Bool"
  , "print :: Show a => a -> IO Unit"
  , "putChar :: Char -> IO Unit"
  , "putStr :: String -> IO Unit"
  , "putStrLn :: String -> IO Unit"
  , "read :: Read a => String -> a"
  , "readFile :: FilePath -> IO String"
  , "readIO :: Read a => String -> IO a"
  , "readLn :: Read a => IO a"
  , "readParen :: Bool -> ReadS a -> ReadS a"
  , "reads :: Read a => ReadS a"
  , "realToFrac :: (Real a, Fractional b) => a -> b"
  , "repeat :: a -> List a"
  , "replicate :: Int -> a -> List a"
  , "reverse :: List a -> List a"
  , "scanl :: (b -> a -> b) -> b -> List a -> List b"
  , "scanl1 :: (a -> a -> a) -> List a -> List a"
  , "scanr :: (a -> b -> b) -> b -> List a -> List b"
  , "scanr1 :: (a -> a -> a) -> List a -> List a"
  , "seq :: a -> b -> b"
  , "sequence_ :: (Foldable t, Monad m) => t (m a) -> m Unit"
  , "showChar :: Char -> ShowS"
  , "showParen :: Bool -> ShowS -> ShowS"
  , "showString :: String -> ShowS"
  , "shows :: Show a => a -> ShowS"
  , "snd :: Tuple a b -> b"
  , "span :: (a -> Bool) -> List a -> Tuple (List a) (List a)"
  , "splitAt :: Int -> List a -> Tuple (List a) (List a)"
  , "subtract :: Num a => a -> a -> a"
  , "tail :: List a -> List a"
  , "take :: Int -> List a -> List a"
  , "takeWhile :: (a -> Bool) -> List a -> List a"
  , "uncurry :: (a -> b -> c) -> Tuple a b -> c"
  , "undefined :: HasCallStack => a"
  , "unlines :: List String -> String"
  , "until :: (a -> Bool) -> (a -> a) -> a -> a"
  , "unwords :: List String -> String"
  , "unzip :: List (Tuple a b) -> Tuple (List a) (List b)"
  , "unzip3 :: List (Tuple3 a b c) -> Tuple3 (List a) (List b) (List c)"
  , "userError :: String -> IOError"
  , "words :: String -> List String"
  , "writeFile :: FilePath -> String -> IO Unit"
  , "zip :: List a -> List b -> List (Tuple a b)"
  , "zip3 :: List a -> List b -> List c -> List (Tuple3 a b c)"
  , "zipWith :: (a -> b -> c) -> List a -> List b -> List c"
  , "zipWith3 :: (a -> b -> c -> d) -> List a -> List b -> List c -> List d"
  , "(||) :: Bool -> Bool -> Bool"
  ]