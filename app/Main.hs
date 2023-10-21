{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

import Core.Language

import Control.Monad
import Options.Applicative


-- Options

type Verbosity = Int

data FCoreOptions = FCoreOptions Verbosity FCoreCommand
    deriving (Show, Eq)

data Input = FileInput FilePath
           | StdInput
           deriving (Show, Eq)

data FCoreCommand = CmdRun    Input 
                  | CmdFormat Input 
                  | CmdParse  Input 
                  deriving (Eq, Show)

pOpts :: Parser FCoreOptions
pOpts =  FCoreOptions 
     <$> length <$> many (flag' () (short 'v' <> help "Verbosity level (repeatable)"))
     <*> ( subparser
            ( command "run" (info (cmd CmdRun)    (progDesc "Run a FCore program"))
           <> command "fmt" (info (cmd CmdFormat) (progDesc "Format a FCore program"))
           <> command "par" (info (cmd CmdParse)  (progDesc "Parse a FCore program"))
            ) 
        <|> cmd CmdRun 
         )
    where cmd ctor = ctor <$> input
          input    =  FileInput <$> argument str (metavar "FILE")
                  <|> pure StdInput


-- Main

main :: IO ()
main = doCommand =<< execParser opts
    where opts = info (pOpts <**> helper)
               ( fullDesc
              <> progDesc "FCore interpreter"
              <> header "fcore - FCore program interpreter" )

doCommand :: FCoreOptions -> IO ()
doCommand = print

