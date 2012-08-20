#! /usr/bin/env runhaskell

-- run a spec
-- args: chapter number, exercise number

import System.Environment
import System.Cmd

main = do
  chapter : exercise : _ <- getArgs
  putStrLn $ "Running chapter " ++ chapter ++ ", exercise " ++ exercise
  let file = "spec/ch" ++ chapter ++ "/ex" ++ exercise ++ "spec.hs"
  system $ "runhaskell " ++ file
  return ()
