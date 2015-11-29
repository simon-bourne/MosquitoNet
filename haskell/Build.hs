#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (lines, unlines, readFile, writeFile, appendFile, concat)
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Control.Applicative ((<$>))
import Data.Text (Text, lines, unlines, pack, unpack, concat)
import Data.Text.IO (readFile, writeFile, appendFile)
import Text.Megaparsec
import qualified Text.Megaparsec.Lexer as Lexer
import Text.Megaparsec.Char (spaceChar)
import Data.Either (isLeft, lefts, rights)
import Control.Monad (void, filterM)
import Data.List (isInfixOf)
import qualified System.Directory as SD (doesFileExist)

type Lexer = Parsec Text

symbol :: String -> Lexer ()
symbol name = void $ Lexer.symbol whiteSpace name

whiteSpace :: Lexer ()
whiteSpace = Lexer.space (void spaceChar) skipLineComment skipBlockComment

skipLineComment :: Lexer ()
skipLineComment = Lexer.skipLineComment "//"

skipBlockComment :: Lexer ()
skipBlockComment = Lexer.skipBlockComment "/*" "*/"

notPragmaOnce :: Text -> Bool
notPragmaOnce = isLeft . runParser pragmaOnce "C++ Header"
  where 
    pragmaOnce :: Parsec Text ()
    pragmaOnce = do
        whiteSpace
        symbol "#"
        symbol "pragma"
        symbol "once"

parseInclude :: Text -> Either Text Text
parseInclude input = either (const $ Left input) Right $ runParser include "C++ Header" input
  where 
    include :: Parsec Text Text
    include = do
        whiteSpace
        symbol "#"
        symbol "include"
        between quote quote (pack <$> many (noneOf "\""))
    quote = char '\"'

findHeader :: [FilePath] -> FilePath -> IO FilePath
findHeader includeDirs file = do
    existingFiles <- liftIO $ filterM SD.doesFileExist fullPaths
    case existingFiles of
        []  -> showError "Couldn't find"
        [f] -> return f
        _   -> showError "Found multiple"
    where
        showError m = error ("Error: " ++ m ++ " \"" ++ file ++ "\"")
        fullPaths = (</> file) <$> includeDirs

readHeader :: [FilePath] -> FilePath -> IO ([FilePath], FilePath, Text)
readHeader includeDirs file = do
    fullPath <- findHeader includeDirs file
    contents <- readFile fullPath
    let parsedContents = parseInclude <$> (filter notPragmaOnce $ lines contents)
    let includes = rights parsedContents
    let header = concat ["// File: ", pack file]
    let code = unlines (header : lefts parsedContents)

    return (unpack <$> includes, fullPath, code)

readAllHeaders :: [FilePath] -> FilePath -> IO [(FilePath, Text)]
readAllHeaders includeDirs file = do
    (includes, fullPath, code) <- readHeader includeDirs file
    dependancies <- mapM (readAllHeaders includeDirs) includes
    return $ foldr (++) [(fullPath, code)] dependancies

buildHeader :: FilePath -> IO ([FilePath], Text)
buildHeader header = do
    headerData <- readAllHeaders ["../cpp/test/include", "../cpp/lib/include"] header
    return $ foldr joinHeaders ([], "") (removeLaterDuplicates [] headerData)
      where
        removeLaterDuplicates _ [] = []
        removeLaterDuplicates seen (h@(path, code) : hs)
            | isInfixOf [path] seen = removeLaterDuplicates seen hs
            | otherwise = h : removeLaterDuplicates (path : seen) hs

        joinHeaders (path, code) (pathList, headerCode) = (path : pathList, concat [code, headerCode])

writeHeader :: FilePath -> Text -> IO ()
writeHeader out source = do
    let includeGuard = "ENHEDRON_MOSQUITONET_H_"
    writeFile out $ unlines [
        "// This file is generated automatically. Do not edit.",
        "",
        concat ["#ifndef ", includeGuard],
        concat ["#define ", includeGuard],
        ""]
    appendFile out source
    appendFile out $ unlines ["", concat ["#endif /* ", includeGuard, " */"], ""]

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles = "../build"} $ do
    let buildDir = "../build"
    let singleHeader = buildDir </> "MosquitoNet.h"
    let inputHeader = "Enhedron/Test.h"

    phony "clean" $ do
        putNormal "Cleaning files in build"
        removeFilesAfter "../build" ["//*"]

    want [singleHeader]
    
    singleHeader %> \out -> do
        dependencies <- liftIO $ do
            (includes, contents) <- buildHeader inputHeader
            writeHeader out contents
            return includes
        need dependencies

