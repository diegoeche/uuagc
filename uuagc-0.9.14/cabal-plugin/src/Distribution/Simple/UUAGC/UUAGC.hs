module Distribution.Simple.UUAGC.UUAGC(uuagcUserHook,
                                       uuagc
                                      ) where
import Distribution.Simple.BuildPaths (autogenModulesDir)
import Debug.Trace
import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils
import Distribution.Simple.Setup
import Distribution.PackageDescription
import Distribution.Simple.UUAGC.AbsSyn( AGFileOption(..)
                                         , AGFileOptions
                                         , UUAGCOption(..)
                                         , UUAGCOptions
                                         , defaultUUAGCOptions
                                         , fromUUAGCOtoArgs
                                         , fromUUAGCOstoArgs
                                         , lookupFileOptions
                                         )
import Distribution.Simple.UUAGC.Parser
import System.Process( CreateProcess(..), createProcess, CmdSpec(..)
                     , StdStream(..), runProcess, waitForProcess
                     , proc)

import System.Directory
import System.FilePath(pathSeparators,
                       (</>),
                       takeFileName,
                       normalise,
                       dropFileName,
                       addExtension,
                       dropExtension)

import System.Exit (ExitCode(..))
import System.IO( openFile, IOMode(..), hFileSize,
                  hSetFileSize, hClose, hGetContents,
                  Handle(..), stderr, hPutStr)

import Control.Exception (throwIO)
import Control.Monad (liftM, when, guard)

-- | 'uuagc' returns the name of the uuagc compiler
uuagcn = "uuagc"

-- | 'defUUAGCOptions' returns the default names of the uuagc options
defUUAGCOptions = "uuagc_options"

uuagcUserHook :: UserHooks
uuagcUserHook = simpleUserHooks {hookedPreProcessors = ("ag", uuagc):knownSuffixHandlers,
                                 buildHook = uuagcBuildHook}

originalPreBuild  = preBuild simpleUserHooks
originalBuildHook = buildHook simpleUserHooks

processContent :: Handle -> IO [String]
processContent = liftM words . hGetContents

putErrorInfo :: Handle -> IO ()
putErrorInfo h = hGetContents h >>= hPutStr stderr

addSearch :: String -> [String] -> [String]
addSearch sp fl = let sf = [head pathSeparators]
                      path = if sp == ""
                             then '.' : sf
                             else sp ++ sf
                  in [normalise (sp ++ f) | f  <- fl]

throwFailure :: IO ()
throwFailure = throwIO $ ExitFailure 1

-- The tmp build directory really depends on the type of project.
-- In the case executables it uses the name of the generated file for
-- the output directory.
withBuildTmpDir
  :: PackageDescription
     -> LocalBuildInfo
     -> (FilePath -> IO ())
     -> IO ()
withBuildTmpDir pkgDescr lbi f = do
            withLib pkgDescr () $ \ _ -> do
                    f $ buildDir lbi
            withExe pkgDescr $ \ theExe -> do
                    f $ buildDir lbi </> exeName theExe </> exeName theExe ++ "-tmp"

-- Creates the output file given the main preprocessed file and the buildtmp folder
tmpFile :: FilePath -> FilePath -> FilePath
tmpFile buildTmp = (buildTmp </>)
                   . (`addExtension` "hs")
                   . dropExtension
                   . takeFileName

-- | 'updateAGFile' search into the uuagc options file for a list of all
-- AG Files and theirs file dependencies in order to see if the latters
-- are more updated that the formers, and if this is the case to
-- update the AG File
updateAGFile pkgDescr lbi (f, sp) = do
  (_, Just ppOutput, Just ppError, ph) <- newProcess
  ec <- waitForProcess ph
  case ec of
    ExitSuccess ->
      do fls <- processContent ppOutput
         let flsC = addSearch sp fls
         flsmt <- mapM getModificationTime flsC
         let maxModified = maximum $ flsmt
             removeTmpFile f = do
                 print f
                 exists <- doesFileExist f
                 print exists
                 when exists $ do
                     fmt <- getModificationTime f
                     when (maxModified > fmt) $ removeFile f
         withBuildTmpDir pkgDescr lbi $ removeTmpFile . (`tmpFile` f)
    (ExitFailure exc) ->
      do putErrorInfo ppOutput
         putErrorInfo ppError
         throwFailure
  where newProcess = createProcess $ (proc uuagcn ["--genfiledeps"
                                                   ,"--=" ++ intercalate ":" [sp]
                                                   ,f ])
                                                  { std_in  = Inherit
                                                  , std_out = CreatePipe
                                                  , std_err = CreatePipe
                                                  }

uuagcBuildHook
  :: PackageDescription
     -> LocalBuildInfo
     -> UserHooks
     -> BuildFlags
     -> IO ()
uuagcBuildHook pd lbi uh bf = do
  uuagcOpts <- parserAG defUUAGCOptions
  let agfls  = getAGFileList uuagcOpts
      agflSP = map (\f -> (f, dropFileName f)) agfls
  mapM_ (updateAGFile pd lbi) agflSP
  originalBuildHook pd lbi uh bf

getAGFileList :: AGFileOptions -> [FilePath]
getAGFileList = map (normalise . filename)

uuagc :: BuildInfo
        -> LocalBuildInfo
        -> PreProcessor
uuagc build local  =
   PreProcessor {
     platformIndependent = True,
     runPreProcessor = mkSimplePreProcessor $ \ inFile outFile verbosity ->
                       do info verbosity $ concat [inFile, " has been preprocessed into ", outFile]
                          print $ "processing: " ++ inFile
                          opts <- parserAG defUUAGCOptions
                          let search  = dropFileName inFile
                              options = fromUUAGCOstoArgs (lookupFileOptions inFile opts)
                                        ++ ["-P" ++ search, "--output=" ++ outFile, inFile]
                          (_,_,_,ph) <- createProcess (proc uuagcn options)
                          eCode <- waitForProcess ph
                          case eCode of
                            ExitSuccess   -> return ()
                            ExitFailure _ -> throwFailure
                }

