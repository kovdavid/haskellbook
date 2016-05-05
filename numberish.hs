class Numberish a where
  fromNumber    :: Integer -> a
  toNumber      :: a -> Integer
  defaultNumber :: a

newtype Age = Age Integer deriving (Eq, Show)

instance Numberish Age where
  fromNumber n = Age n
  toNumber (Age n) = n
  defaultNumber = Age 65

newtype Year = Year Integer deriving (Eq, Show)

instance Numberish Year where
  fromNumber n = Year n
  toNumber (Year n) = n
  defaultNumber = Year 1988

sumNumberish :: Numberish a => a -> a -> a
sumNumberish a a' = fromNumber summed
  where integerOfA = toNumber a
        integerOfAPrime = toNumber a'
        summed = integerOfA + integerOfAPrime

data DayOfWeek = Mon | Tue | Weds | Thu | Fri | Sat | Sun

data Date = Date DayOfWeek Int

instance Eq DayOfWeek where
  (==) Mon  Mon  = True
  (==) Tue  Tue  = True
  (==) Weds Weds = True
  (==) Thu  Thu  = True
  (==) Fri  Fri  = True
  (==) Sat  Sat  = True
  (==) Sun  Sun  = True
  (==) _ _       = False

instance Eq Date where
  (==) (Date weekDay monthNum)
       (Date weekDay' monthNum') =
    weekDay == weekDay' && monthNum == monthNum'
