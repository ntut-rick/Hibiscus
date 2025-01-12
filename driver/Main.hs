module Main where

import Control.Monad (when)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Functor (void)
import Hibiscus.CodeGen (generate, instructionsToString)
import Hibiscus.CodeGen.Constants (defaultConfig)
import Hibiscus.Parsing.Lexer (runAlex)
import Hibiscus.Parsing.Parser (parseHibiscus)
import Hibiscus.TypeInfer (infer)
import System.Environment (getArgs)


printList :: (Show a) => [a] -> IO ()
printList = mapM_ (putStrLn . show)

main :: IO ()
main = do
  args <- getArgs
  when (null args) $ error "Usage: program <file-path>"
  let inputFilePath = head args

  putStrLn "\n----- Parsing Result ---------------"
  content <- BS.readFile inputFilePath
  case runAlex content parseHibiscus of
    Left err -> putStrLn $ "Parse Error: " ++ err ++ ", perhaps you forgot a ';'?"
    Right parseResult -> do
      printList parseResult
      putStrLn "\n----- Type Infer Result ---------------"
      case infer parseResult of
        Left err -> putStrLn $ "Infer Error: " ++ err
        Right decs -> do
          printList decs
          putStrLn "\n----- Code Generate Result ---------------"
          let code = generate defaultConfig decs
          putStrLn $ show code
          writeFile (inputFilePath ++ ".asm") (instructionsToString code)

-- case typeInfer parseResult of
--   Left err -> putStrLn $ "Check Error: " ++ err
--   Right (env, de) -> do
--     -- print env
--     -- mapM_ print de
--     -- print de
--     let code = generate defaultConfig de
--     print de
--     -- print env
--     -- putStrLn (instructionsToString code)
--     putStrLn $ show code
--     writeFile (inputFilePath ++ ".asm") (instructionsToString code)

-- case typeInfer parseResult of
--   Left err -> putStrLn $ "Check Error: " ++ err
--   Right (env, de) -> do
--     print env
--     mapM_ print de
