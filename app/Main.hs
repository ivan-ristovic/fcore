{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

import Core.Parser
import Core.Printer
import Core.Template

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
-- run
doCommand (FCoreOptions v (CmdRun StdInput)) = do
    prog <- getContents
    either print putStrLn $ runCoreProgram v prog 
doCommand (FCoreOptions v (CmdRun (FileInput path))) = 
    runCoreFile v path 
-- par
doCommand (FCoreOptions _ (CmdParse (StdInput))) = do
    prog <- getContents
    either print (putStrLn . show) $ parseCoreProgram prog 
doCommand (FCoreOptions _ (CmdParse (FileInput path))) = 
    parseCoreFile path >>= print 
-- fmt
doCommand (FCoreOptions _ (CmdFormat (StdInput))) = do
    prog <- getContents
    either print (putStrLn . pprint) $ parseCoreProgram prog 
doCommand (FCoreOptions _ (CmdFormat (FileInput path))) = 
    parseCoreFile path >>= (putStrLn . pprint)

