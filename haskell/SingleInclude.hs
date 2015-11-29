{-# LANGUAGE OverloadedStrings #-}

module SingleInclude (writeHeader, buildHeader) where

import Prelude hiding (lines, unlines, readFile, writeFile, appendFile)
import Control.Applicative ((<$>))
import Data.Text (Text, lines, unlines, pack, unpack)
import qualified Data.Text as Text (concat)
import Data.Text.IO (readFile, writeFile, appendFile)
import Text.Megaparsec
import qualified Text.Megaparsec.Lexer as Lexer
import Text.Megaparsec.Char (spaceChar)
import Data.Either (isLeft, lefts, rights)
import Control.Monad (void, filterM)
import Data.List (isInfixOf)
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

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

findHeader :: [FilePath] -> FilePath -> Action FilePath
findHeader includeDirs file = do
    existingFiles <- filterM doesFileExist fullPaths
    case existingFiles of
        []  -> showError "Couldn't find"
        [f] -> return f
        _   -> showError "Found multiple"
    where
        showError m = error ("Error: " ++ m ++ " \"" ++ file ++ "\"")
        fullPaths = (</> file) <$> includeDirs

readHeader :: [FilePath] -> FilePath -> Action ([FilePath], FilePath, Text)
readHeader includeDirs file = do
    fullPath <- findHeader includeDirs file
    contents <- liftIO $ readFile fullPath
    let parsedContents = parseInclude <$> (filter notPragmaOnce $ lines contents)
    let includes = rights parsedContents
    let header = Text.concat ["// File: ", pack file]
    let code = unlines (header : lefts parsedContents)

    return (unpack <$> includes, fullPath, code)

readAllHeaders :: [FilePath] -> FilePath -> Action [(FilePath, Text)]
readAllHeaders includeDirs file = do
    (includes, fullPath, code) <- readHeader includeDirs file
    dependancies <- mapM (readAllHeaders includeDirs) includes
    return $ concat ([(fullPath, code)] : dependancies)

buildHeader :: FilePath -> Action ([FilePath], Text)
buildHeader header = do
    headerData <- readAllHeaders ["../cpp/test/include", "../cpp/lib/include"] header
    return $ foldr joinHeaders ([], "") (removeLaterDuplicates [] headerData)
      where
        removeLaterDuplicates _ [] = []
        removeLaterDuplicates seen (h@(path, code) : hs)
            | isInfixOf [path] seen = removeLaterDuplicates seen hs
            | otherwise = h : removeLaterDuplicates (path : seen) hs

        joinHeaders (path, code) (pathList, headerCode) = (path : pathList, Text.concat [code, headerCode])

writeHeader :: FilePath -> Text -> Action ()
writeHeader out source = do
    let includeGuard = "ENHEDRON_MOSQUITONET_H_"
    liftIO $ do
        writeFile out $ unlines [
            "// This file is generated automatically. Do not edit.",
            "",
            Text.concat ["#ifndef ", includeGuard],
            Text.concat ["#define ", includeGuard],
            ""]
        appendFile out source
        appendFile out $ unlines ["", Text.concat ["#endif /* ", includeGuard, " */"], ""]
