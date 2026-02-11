{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- | CLI entry point for the ObjC binding code generator.
--
-- Usage:
--
-- @
-- objc-codegen --framework Foundation --output-dir generated/
-- objc-codegen -f AppKit -f WebKit -f AVFoundation -o generated/
-- objc-codegen --framework AppKit --output-dir generated/ --target-framework AppKit
-- @
--
-- Multiple @-f@ flags can be provided to import several frameworks in one
-- clang invocation.  Each is guarded by @__has_include@ so frameworks
-- without an Objective-C umbrella header are silently skipped.
--
-- When @--target-framework@ is specified, the target and all its transitive
-- dependencies are generated flat under @--output-dir@.
module Main (main) where

import Control.Concurrent (forkIO)
import Control.Exception (evaluate)
import Control.Monad (unless, when)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath ((</>), (<.>), takeDirectory)
import System.IO (hClose, hFlush, hPutStr, hPutStrLn, stderr)
import System.Process (CreateProcess(..), StdStream(..), proc, createProcess,
                       readProcess, waitForProcess)

import ObjC.CodeGen.ClangAST (parseClangAST, ParsedAST(..))
import ObjC.CodeGen.Hierarchy (buildHierarchy)
import ObjC.CodeGen.IR (ClassHierarchy(..), StructDef(structFramework), EnumDef(enumFramework))
import ObjC.CodeGen.Generate
  ( generateFrameworkPackages
  , GeneratedPackage(..)
  , GeneratedModule(..)
  , SkippedBinding(..)
  )

-- ---------------------------------------------------------------------------
-- CLI options
-- ---------------------------------------------------------------------------

data Options = Options
  { optFrameworks      :: [String]
  , optHeader          :: Maybe FilePath
  , optOutputDir       :: FilePath
  , optTargetFramework :: Maybe String
  } deriving (Show)

optionsParser :: Parser Options
optionsParser = Options
  <$> many (strOption
        ( long "framework"
       <> short 'f'
       <> metavar "NAME"
       <> help "Framework(s) to import (repeatable, e.g. -f AppKit -f WebKit)"
        ))
  <*> optional (strOption
        ( long "header"
       <> short 'H'
       <> metavar "PATH"
       <> help "Path to an Objective-C header file"
        ))
  <*> strOption
        ( long "output-dir"
       <> short 'o'
       <> metavar "DIR"
       <> value "generated"
       <> help "Parent directory for generated packages (default: generated/)"
        )
  <*> optional (strOption
        ( long "target-framework"
       <> short 't'
       <> metavar "NAME"
       <> help "Generate this framework and all its transitive dependencies (default: all found in AST)"
        ))

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  opts <- execParser $ info (optionsParser <**> helper)
    ( fullDesc
   <> progDesc "Generate type-safe Haskell bindings from Objective-C framework headers"
   <> header "objc-codegen - ObjC -> Haskell binding generator"
    )

  -- Build the clang input header
  headerContent <- case (optFrameworks opts, optHeader opts) of
    (fws@(_:_), _) -> do
      let mkImport fw = "#if __has_include(<" ++ fw ++ "/" ++ fw ++ ".h>)\n"
                     ++ "#import <" ++ fw ++ "/" ++ fw ++ ".h>\n"
                     ++ "#endif\n"
      putStrLn $ "Importing " ++ show (length fws) ++ " framework(s)..."
      pure (concatMap mkImport fws)
    (_, Just hdr) -> readFile hdr
    ([], Nothing) -> do
      putStrLn "Error: specify --framework or --header"
      exitFailure

  -- Run clang to dump the AST as JSON.
  -- We read clang's stdout directly as a lazy ByteString to avoid the
  -- catastrophic memory overhead of Haskell's String (~24 bytes/char).
  -- For large frameworks like AppKit the AST JSON can exceed 1 GB;
  -- a String representation would require 24+ GB of heap.
  putStrLn "Running clang to dump AST..."
  sdkPath <- strip <$> readProcess "xcrun" ["--show-sdk-path"] ""
  let clangArgs =
        [ "-Xclang", "-ast-dump=json"
        , "-fsyntax-only"
        , "-fretain-comments-from-system-headers"
        , "-ferror-limit=0"
        , "-w"  -- suppress warnings to reduce stderr noise
        , "-x", "objective-c"
        , "-isysroot", sdkPath
        , "-c", "-"
        ]
  astBytes <- runClangToLBS ("clang" : clangArgs) headerContent
  let !astLen = BSL.length astBytes
  putStrLn $ "AST JSON size: " ++ show astLen ++ " bytes"

  -- Parse the AST
  putStrLn "Parsing AST..."
  case parseClangAST astBytes of
    Left err -> do
      putStrLn $ "Parse error: " ++ err
      exitFailure
    Right parsed -> do
      putStrLn $ "  Interfaces: " ++ show (length (parsedInterfaces parsed))
      putStrLn $ "  Categories: " ++ show (length (parsedCategories parsed))
      putStrLn $ "  Protocols:  " ++ show (length (parsedProtocols parsed))
      putStrLn $ "  Structs:    " ++ show (length (parsedStructDefs parsed))
      putStrLn $ "  Enums:      " ++ show (length (parsedEnumDefs parsed))
      putStrLn $ "  Typedefs:   " ++ show (Map.size (parsedTypedefs parsed))

      -- Build hierarchy
      putStrLn "Building class hierarchy..."
      let hierarchy0 = buildHierarchy parsed
      putStrLn $ "  Classes in hierarchy: "
        ++ show (Map.size (hierarchyClasses hierarchy0))
      putStrLn $ "  Topological order: "
        ++ show (length (hierarchyTopoOrder hierarchy0))

      -- Assign frameworkless classes to "Foundation" by default.
      -- Classes like NSObject whose @interface is in /usr/include/objc/
      -- rather than inside a .framework bundle are logically Foundation
      -- classes. Using a fixed "Foundation" (rather than the --framework
      -- value) prevents duplicate class declarations across frameworks.
      let hasFrameworks = not (null (optFrameworks opts))
          hierarchy = if not hasFrameworks then hierarchy0 else
              let known = Map.keysSet (hierarchyFrameworks hierarchy0)
                  allCls = Map.keysSet (hierarchyClasses hierarchy0)
                  missing = Set.difference allCls known
                  extraFw = Map.fromSet (const "Foundation") missing
                  -- Also assign frameworkless structs to the default framework
                  updatedStructs = Map.map (\sd ->
                    case structFramework sd of
                      Just _  -> sd
                      Nothing -> sd { structFramework = Just "Foundation" }
                    ) (hierarchyStructs hierarchy0)
                  -- Also assign frameworkless enums to the default framework
                  updatedEnums = Map.map (\ed ->
                    case enumFramework ed of
                      Just _  -> ed
                      Nothing -> ed { enumFramework = Just "Foundation" }
                    ) (hierarchyEnums hierarchy0)
              in hierarchy0
                { hierarchyFrameworks =
                    Map.union (hierarchyFrameworks hierarchy0) extraFw
                , hierarchyStructs = updatedStructs
                , hierarchyEnums   = updatedEnums
                }

      putStrLn $ "  Structs in hierarchy: "
        ++ show (Map.size (hierarchyStructs hierarchy))
      putStrLn $ "  Enums in hierarchy: "
        ++ show (Map.size (hierarchyEnums hierarchy))

      -- Debug: protocol conformance stats
      let protoConforms = hierarchyProtoConforms hierarchy
          allProtos = hierarchyAllProtos hierarchy
          classesWithProtos = Map.filter (not . Set.null) allProtos
      putStrLn $ "  Protocols defined: "
        ++ show (Map.size (hierarchyProtocols hierarchy))
      putStrLn $ "  Classes with direct protocol conformance: "
        ++ show (Map.size (Map.filter (not . Set.null) protoConforms))
      putStrLn $ "  Classes with any protocol conformance: "
        ++ show (Map.size classesWithProtos)
      -- Show a sample
      case take 3 (Map.toList classesWithProtos) of
        [] -> putStrLn "  (no classes with protocols found)"
        samples -> mapM_ (\(cls, protos) ->
          putStrLn $ "    " ++ T.unpack cls ++ " -> "
            ++ show (Set.toList protos)) samples

      let fwSet = Set.fromList (Map.elems (hierarchyFrameworks hierarchy))
          -- Also include frameworks from structs
          structFwSet = Set.fromList
            [ fw | sd <- Map.elems (hierarchyStructs hierarchy)
            , Just fw <- [structFramework sd] ]
          allFwSet = Set.union fwSet structFwSet
      putStrLn $ "  Frameworks found: " ++ show (Set.size allFwSet)
        ++ " " ++ show (Set.toList allFwSet)

      -- Generate per-framework packages
      let outDir = optOutputDir opts
          targetFw = fmap T.pack (optTargetFramework opts)
          packages = generateFrameworkPackages hierarchy targetFw

      putStrLn $ "\nGenerating " ++ show (length packages)
        ++ " framework package(s)..."

      -- Write all output packages (target + transitive dependencies)
      mapM_ (writePackage outDir) packages

      -- Print summary
      putStrLn "\nSummary:"
      mapM_ (\pkg -> do
        putStrLn $ "  " ++ T.unpack (gpkgName pkg)
          ++ " (" ++ T.unpack (gpkgFramework pkg) ++ ")"
        putStrLn $ "    Modules: " ++ show (length (gpkgModules pkg))
        unless (Set.null (gpkgDeps pkg)) $
          putStrLn $ "    Deps: "
            ++ T.unpack (T.intercalate ", " (Set.toList (gpkgDeps pkg)))
        ) packages

      -- Report skipped bindings
      let allSkipped = concatMap gpkgSkipped packages
      unless (null allSkipped) $ do
        hPutStrLn stderr $ "\nSkipped " ++ show (length allSkipped)
          ++ " binding(s) due to unsupported types:"
        mapM_ (\s -> hPutStrLn stderr $ "  "
          ++ T.unpack (skipFramework s) ++ " "
          ++ T.unpack (skipClass s) ++ "."
          ++ T.unpack (skipSelector s) ++ ": "
          ++ T.unpack (skipReason s)) allSkipped

      putStrLn "\nDone."

-- ---------------------------------------------------------------------------
-- Writing packages to disk
-- ---------------------------------------------------------------------------

-- | Write a generated package to disk under the output directory.
--
-- Removes the old @src/@ directory first to avoid stale @.hs@ files from
-- prior codegen runs being auto-discovered by hpack/GHC.
writePackage :: FilePath -> GeneratedPackage -> IO ()
writePackage outDir pkg = do
  let pkgDir = outDir </> T.unpack (gpkgName pkg)
      srcDir = pkgDir </> "src"

  -- Remove stale source tree from prior runs
  srcExists <- doesDirectoryExist srcDir
  when srcExists $ removeDirectoryRecursive srcDir

  -- Write package.yaml
  createDirectoryIfMissing True pkgDir
  TIO.writeFile (pkgDir </> "package.yaml") (gpkgPackageYaml pkg)
  putStrLn $ "  Wrote: " ++ pkgDir </> "package.yaml"

  -- Write re-export module (if present)
  mapM_ (writeModule pkgDir) (gpkgReExport pkg)

  -- Write class modules
  mapM_ (writeModule pkgDir) (gpkgModules pkg)

-- | Write a generated module to disk.
writeModule :: FilePath -> GeneratedModule -> IO ()
writeModule pkgDir gm = do
  let modPath = T.unpack (T.replace "." "/" (genModuleName gm)) <.> "hs"
      fullPath = pkgDir </> "src" </> modPath
      dir = takeDirectory fullPath
  createDirectoryIfMissing True dir
  TIO.writeFile fullPath (genModuleContent gm)
  putStrLn $ "  Wrote: " ++ fullPath

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Run @xcrun clang ...@ feeding @input@ on stdin, returning stdout as a
-- lazy 'BSL.ByteString'.
--
-- This avoids the massive heap overhead of 'readProcess' which returns
-- 'String' (~24 bytes per character).  For AppKit's ~1 GB AST JSON, the
-- difference is 1 GB (ByteString) vs 24+ GB (String).
runClangToLBS :: [String] -> String -> IO BSL.ByteString
runClangToLBS args input = do
  let cp = (proc "xcrun" args)
        { std_in  = CreatePipe
        , std_out = CreatePipe
        , std_err = Inherit
        }
  (Just hIn, Just hOut, _, ph) <- createProcess cp
  -- Write stdin on a separate thread to avoid deadlocking when the pipe
  -- buffer fills (clang writes stdout while we're still writing stdin).
  _ <- forkIO $ do
    hPutStr hIn input
    hFlush hIn
    hClose hIn
  -- Read stdout as a lazy ByteString — chunks are pulled on demand.
  bs <- BSL.hGetContents hOut
  -- Force the entire ByteString so the process can finish.
  !_ <- evaluate (BSL.length bs)
  ex <- waitForProcess ph
  case ex of
    ExitSuccess   -> return bs
    ExitFailure c -> do
      -- When importing many frameworks, some may have benign conflicts
      -- (e.g. Kerberos vs GSS typedef collisions).  Clang still emits a
      -- valid AST for the declarations it managed to parse, so we return
      -- what we have rather than aborting.
      hPutStrLn stderr $ "clang exited with code " ++ show c
        ++ " (continuing with partial AST)"
      return bs

-- | Strip whitespace from both ends of a string.
strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
  where isSpace c = c == ' ' || c == '\n' || c == '\r' || c == '\t'
