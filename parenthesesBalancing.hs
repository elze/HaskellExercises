isOpen :: Char -> Bool
isOpen '(' = True
isOpen '[' = True
isOpen '{' = True
isOpen _ = False

isClosing :: Char -> Bool
isClosing ')' = True
isClosing ']' = True
isClosing '}' = True
isClosing _ = False


isMatch :: Char -> Char -> Bool
isMatch ')' '(' = True
isMatch ']' '[' = True
isMatch '}' '{' = True
isMatch _ _ = False

charValidator :: Maybe String -> Char -> Maybe String
charValidator (Just []) x
  | isOpen x = (Just [x])
  | otherwise = Nothing
  
charValidator (Just (q:qs)) x
  | isOpen x = (Just (x:q:qs))
  | isClosing x = if (isMatch x q) then (Just qs) else Nothing
  | otherwise = Just(q:qs)  

charValidator Nothing x = Nothing

validBracesHelper s = foldl charValidator (Just []) s

validBraces s
  | validBracesHelper s == Nothing = False
  | otherwise = if null q then True else False 
     where Just q = validBracesHelper s
   