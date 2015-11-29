#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import SingleInclude(writeHeader, buildHeader)

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
        (includes, contents) <- buildHeader inputHeader
        need includes
        writeHeader out contents
