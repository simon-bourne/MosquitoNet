#!/usr/bin/env runhaskell

import Development.Shake
import Development.Shake.FilePath
import SingleInclude(writeHeader, buildHeader)
import System.FilePath.Find (find, fileType, FileType(RegularFile, Directory), (==?))
import Control.Applicative ((<$>))
import System.Directory (canonicalizePath)
import Data.Text (Text)

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

-- TODO: Rename some constants
buildDir, moduleDir, enhedron, cppTestDir :: FilePath
buildDir = "../build"
moduleDir = "../modules"
enhedron = "Enhedron"
cppTestDir = "cpp/test/src"

dropDirectory :: Int -> FilePath -> FilePath
dropDirectory c
    | c <= 0 = id
    | otherwise = dropDirectory1 . dropDirectory (c - 1)

mkDir :: FilePath -> Action ()
mkDir dir = unit $ cmd "mkdir" "-p" dir

copyHeader :: FilePath -> FilePath -> Action ()
copyHeader destDir input = do
    let dest = destDir </> (dropDirectory1 input)
    mkDir $ takeDirectory dest
    copyFile' input dest

cmake :: [FilePath] -> FilePath -> FilePath -> Action ()
cmake additionalDependencies srcRoot out = do
    let cmakeListsFile = "CMakeLists.txt"
    absoluteSrcRoot <- liftIO $ canonicalizePath srcRoot

    need ((srcRoot </> cmakeListsFile) : additionalDependencies)

    let exeDir = takeDirectory out
    mkDir exeDir

    let [compiler, variant, _, _] = lastN 4 $ splitPath out
    let (cxx, cc) = cxxCompiler compiler
    let env = [Cwd exeDir, AddEnv "CMAKE_BUILD_TYPE" variant, AddEnv "CXX" cxx, AddEnv "CC" cc]

    command_ env "cmake" [absoluteSrcRoot]
    command_ env "make" ["-j", "8"]
      where
        cxxCompiler "gcc/" = ("g++", "gcc")
        cxxCompiler "clang-3.6/" = ("clang++-3.6", "clang-3.6")
        cxxCompiler name = error ("Unknown C++ compiler " ++ name)

allFilesIn :: FilePath -> IO [FilePath]
allFilesIn = find (fileType ==? Directory) (fileType ==? RegularFile)

singleHeaderName :: FilePath -> FilePath
singleHeaderName destName = buildDir </> destName  </> "cpp/single-include" </> destName <.> "h"

singleHeaderRules :: FilePath -> [FilePath] -> Text -> Rules ()
singleHeaderRules destName singleHeaderDeps singleHeaderContents = do
    let singleHeader = singleHeaderName destName
    let targetHeaders = singleHeader : ((destDir </>) <$> (dropDirectory 1 <$> singleHeaderDeps))

    want targetHeaders

    targetHeaders &%> \_ -> do
        mapM_ (copyHeader destDir) singleHeaderDeps
        liftIO $ writeHeader singleHeader singleHeaderContents
      where
        destDir = buildDir </> destName

testMatrix :: [FilePath] -> [FilePath] -> [(FilePath, FilePath)] -> [FilePath]
testMatrix compilers variants exeDetails =
    [buildDir </> "test" </> c </> v </> t </> n <.> "log" | c <- compilers, v <- variants, (t, n) <- exeDetails ]

rules :: FilePath -> FilePath -> [FilePath] -> [FilePath] -> Rules ()
rules sourceName destName allModuleFiles allCppSrcFiles = do
    let singleHeader = singleHeaderName destName
    let destCppTestDir = destDir </> cppTestDir
    let allCppSrcTargets = (destCppTestDir </>) <$> (dropDirectory 5 <$> allCppSrcFiles)
    let allModuleTargets = (destDir </>) <$> (dropDirectory 3 <$> allModuleFiles)

    phony "clean" $ do
        putNormal "Cleaning files in build"
        removeFilesAfter "../build" ["//*"]

    let compilers = ["gcc", "clang-3.6"]
    let variants = ["Debug", "Release"]
    let exesDetail = [("single-include", "single-include"), ("multi-include", "test-harness")]

    want (allModuleTargets ++ testMatrix compilers variants exesDetail)

    buildDir </> "exe/*/*/single-include/*" %> \out -> do
        mkDir destCppTestDir
        cmake (singleHeader : allCppSrcTargets) destDir out

    buildDir </> "exe/*/*/multi-include/*" %> \out -> do
        cmake allCppSrcFiles ".." out

    buildDir </> "test/*/*/*/*" <.> "log" %> \out -> do
        let tailPath = lastN 4 $ splitPath out
        let exePath = dropExtension (buildDir </> (foldl (</>) "exe" tailPath))

        need [exePath]
        unit $ cmd (FileStdout out) exePath

    allModuleTargets &%> \_ -> do
        putNormal "Running rsync"
        need allModuleFiles

        unit $ cmd "rsync" "-az" "--delete" "--exclude" "/cpp/" ((moduleDir </> sourceName) ++ "/") destDir

    allCppSrcTargets &%> \_ -> do
        let srcCppTestDir = (".." </> cppTestDir </> sourceName) ++ "/"

        mkDir destCppTestDir
        unit $ cmd "rsync" "-az" "--delete" srcCppTestDir destCppTestDir

      where
        destDir = buildDir </> destName

main :: IO ()
main = do
    allModuleFiles <- allFilesIn moduleDir
    allCppSourceFiles <- allFilesIn (".." </> cppTestDir </> sourceName)
    let inputHeader = enhedron </> sourceName <.> "h"
    (singleHeaderIncludes, singleHeaderContents) <- buildHeader inputHeader

    shakeArgs shakeOptions{shakeFiles = buildDir} $ do
        rules sourceName destName allModuleFiles allCppSourceFiles
        singleHeaderRules destName singleHeaderIncludes singleHeaderContents

      where
        sourceName = "Test"
        destName = "MosquitoNet"
