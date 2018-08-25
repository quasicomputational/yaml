import Data.Yaml
import System.Environment

main = do
  [name] <- getArgs
  res <- decodeFileEither name
  print $ (res :: Either ParseException Value)
