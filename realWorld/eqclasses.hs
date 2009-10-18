data Color = Red | Blue | Green

class BasicEq a where
    isEqual :: a -> a -> Bool
    isEqual x = not . isNotEqual x

    isNotEqual :: a -> a -> Bool
    isNotEqual x = not . isEqual x

instance BasicEq Color where
    isEqual Red Red = True
    isEqual Blue Blue = True
    isEqual Green Green = True
    isEqual _ _ = False