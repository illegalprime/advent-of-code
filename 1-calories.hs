import Data.List
import System.Environment


main :: IO ()
main = do
  file <- head <$> getArgs
  text <- readFile file
  print $ query $ elves text
  -- the question, to sum the three highest elves
  where query = sum . take 3 . reverse . sort


-- parse text into total calories of each elf's provisions
elves :: String -> [Integer]
elves text =
  map elf $ split "" $ lines $ text
  where elf = sum . map (read)


-- split a list by a delimiter `a`
split :: Eq a => a -> [a] -> [[a]]
split d = foldr splitter []
  where
    splitter x [] = [[x]]
    splitter x (sublist:lists) =
      if x == d
      then [] : sublist : lists
      else (x : sublist) : lists


