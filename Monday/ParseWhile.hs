module ParseWhile where

import Practice6

import Data.List

nat :: Parser Int
nat = foldl' (\acc cur -> acc*10 + cur) 0 <$> some digit

-- true v false
bool :: Parser Bool
bool = (token "true" *> pure True)
   <|> (token "false" *> pure False)