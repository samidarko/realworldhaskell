
addOneToMaybe :: Num a => Maybe a -> Maybe a
addOneToMaybe Nothing = Nothing
addOneToMaybe (Just a) = Just (a+1)

