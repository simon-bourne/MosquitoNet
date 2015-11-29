#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import SingleInclude(writeHeader, buildHeader)
import System.FilePath.Find (find, fileType, FileType(RegularFile, Directory), (==?))
import Control.Applicative ((<$>))

buildDir, moduleDir :: FilePath
buildDir = "../build"
moduleDir = "../modules"

dropDirectory :: Int -> FilePath -> FilePath
dropDirectory c
    | c <= 0 = id
    | otherwise = dropDirectory1 . dropDirectory (c - 1)

findAllFiles :: FilePath -> IO [FilePath]
findAllFiles dir = find (fileType ==? Directory) (fileType ==? RegularFile) dir

rules :: FilePath -> FilePath -> IO ()
rules sourceName destName = shakeArgs shakeOptions{shakeFiles = buildDir} $ do
    let enhedron = "Enhedron"
    let singleHeader = buildDir </> destName </> "single-include" </> destName <.> "h"
    let inputHeader = enhedron </> sourceName <.> "h"

    phony "clean" $ do
        putNormal "Cleaning files in build"
        removeFilesAfter "../build" ["//*"]

    want [singleHeader]
    
    singleHeader %> \out -> do
        putNormal ("Building single include for " ++ destName)
        (includes, contents) <- buildHeader inputHeader
        need includes
        writeHeader out contents

    action $ do
        putNormal ("Running rsync")

    {- TODO:
        Actually run rsync.
        Rules to make each exe with cmake + wants for targets, need all C++ files. touch exe at end?
        Rules to run tests - use log files as targets.
        Install haskell-platform on travis and use that in linux builds.
    }

main :: IO ()
main = rules "Test" "MosquitoNet"
