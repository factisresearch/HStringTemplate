import Control.Monad
import System.Exit

main = do foo <- getContents
          let actual_loc = filter (not.null) $ filter isntcomment $
                           map (dropWhile (==' ')) $ lines foo
              loc = length actual_loc
          putStrLn $ show loc ++ " lines of code."

isntcomment ('-':'-':_) = False
isntcomment ('{':'-':_) = False -- pragmas
isntcomment _ = True
