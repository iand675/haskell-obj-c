{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLCompileOptions@.
module ObjC.Metal.MTLCompileOptions
  ( MTLCompileOptions
  , IsMTLCompileOptions(..)
  , preprocessorMacros
  , setPreprocessorMacros
  , fastMathEnabled
  , setFastMathEnabled
  , mathMode
  , setMathMode
  , mathFloatingPointFunctions
  , setMathFloatingPointFunctions
  , languageVersion
  , setLanguageVersion
  , libraryType
  , setLibraryType
  , installName
  , setInstallName
  , libraries
  , setLibraries
  , preserveInvariance
  , setPreserveInvariance
  , optimizationLevel
  , setOptimizationLevel
  , compileSymbolVisibility
  , setCompileSymbolVisibility
  , allowReferencingUndefinedSymbols
  , setAllowReferencingUndefinedSymbols
  , maxTotalThreadsPerThreadgroup
  , setMaxTotalThreadsPerThreadgroup
  , enableLogging
  , setEnableLogging
  , preprocessorMacrosSelector
  , setPreprocessorMacrosSelector
  , fastMathEnabledSelector
  , setFastMathEnabledSelector
  , mathModeSelector
  , setMathModeSelector
  , mathFloatingPointFunctionsSelector
  , setMathFloatingPointFunctionsSelector
  , languageVersionSelector
  , setLanguageVersionSelector
  , libraryTypeSelector
  , setLibraryTypeSelector
  , installNameSelector
  , setInstallNameSelector
  , librariesSelector
  , setLibrariesSelector
  , preserveInvarianceSelector
  , setPreserveInvarianceSelector
  , optimizationLevelSelector
  , setOptimizationLevelSelector
  , compileSymbolVisibilitySelector
  , setCompileSymbolVisibilitySelector
  , allowReferencingUndefinedSymbolsSelector
  , setAllowReferencingUndefinedSymbolsSelector
  , maxTotalThreadsPerThreadgroupSelector
  , setMaxTotalThreadsPerThreadgroupSelector
  , enableLoggingSelector
  , setEnableLoggingSelector

  -- * Enum types
  , MTLCompileSymbolVisibility(MTLCompileSymbolVisibility)
  , pattern MTLCompileSymbolVisibilityDefault
  , pattern MTLCompileSymbolVisibilityHidden
  , MTLLanguageVersion(MTLLanguageVersion)
  , pattern MTLLanguageVersion1_0
  , pattern MTLLanguageVersion1_1
  , pattern MTLLanguageVersion1_2
  , pattern MTLLanguageVersion2_0
  , pattern MTLLanguageVersion2_1
  , pattern MTLLanguageVersion2_2
  , pattern MTLLanguageVersion2_3
  , pattern MTLLanguageVersion2_4
  , pattern MTLLanguageVersion3_0
  , pattern MTLLanguageVersion3_1
  , pattern MTLLanguageVersion3_2
  , pattern MTLLanguageVersion4_0
  , MTLLibraryOptimizationLevel(MTLLibraryOptimizationLevel)
  , pattern MTLLibraryOptimizationLevelDefault
  , pattern MTLLibraryOptimizationLevelSize
  , MTLLibraryType(MTLLibraryType)
  , pattern MTLLibraryTypeExecutable
  , pattern MTLLibraryTypeDynamic
  , MTLMathFloatingPointFunctions(MTLMathFloatingPointFunctions)
  , pattern MTLMathFloatingPointFunctionsFast
  , pattern MTLMathFloatingPointFunctionsPrecise
  , MTLMathMode(MTLMathMode)
  , pattern MTLMathModeSafe
  , pattern MTLMathModeRelaxed
  , pattern MTLMathModeFast

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | preprocessorNames
--
-- List of preprocessor macros to consider to when compiling this program. Specified as key value pairs, using a NSDictionary. The keys must be NSString objects and values can be either NSString or NSNumber objects.
--
-- The default value is nil.
--
-- ObjC selector: @- preprocessorMacros@
preprocessorMacros :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> IO (Id NSDictionary)
preprocessorMacros mtlCompileOptions  =
    sendMsg mtlCompileOptions (mkSelector "preprocessorMacros") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | preprocessorNames
--
-- List of preprocessor macros to consider to when compiling this program. Specified as key value pairs, using a NSDictionary. The keys must be NSString objects and values can be either NSString or NSNumber objects.
--
-- The default value is nil.
--
-- ObjC selector: @- setPreprocessorMacros:@
setPreprocessorMacros :: (IsMTLCompileOptions mtlCompileOptions, IsNSDictionary value) => mtlCompileOptions -> value -> IO ()
setPreprocessorMacros mtlCompileOptions  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtlCompileOptions (mkSelector "setPreprocessorMacros:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | fastMathEnabled
--
-- If YES, enables the compiler to perform optimizations for floating-point arithmetic that may violate the IEEE 754 standard. It also enables the high precision variant of math functions for single precision floating-point scalar and vector types. fastMathEnabled defaults to YES.
--
-- ObjC selector: @- fastMathEnabled@
fastMathEnabled :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> IO Bool
fastMathEnabled mtlCompileOptions  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlCompileOptions (mkSelector "fastMathEnabled") retCULong []

-- | fastMathEnabled
--
-- If YES, enables the compiler to perform optimizations for floating-point arithmetic that may violate the IEEE 754 standard. It also enables the high precision variant of math functions for single precision floating-point scalar and vector types. fastMathEnabled defaults to YES.
--
-- ObjC selector: @- setFastMathEnabled:@
setFastMathEnabled :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> Bool -> IO ()
setFastMathEnabled mtlCompileOptions  value =
    sendMsg mtlCompileOptions (mkSelector "setFastMathEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | mathMode
--
-- Sets the floating-point arithmetic optimizations. Default depends on the language standard version.
--
-- ObjC selector: @- mathMode@
mathMode :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> IO MTLMathMode
mathMode mtlCompileOptions  =
    fmap (coerce :: CLong -> MTLMathMode) $ sendMsg mtlCompileOptions (mkSelector "mathMode") retCLong []

-- | mathMode
--
-- Sets the floating-point arithmetic optimizations. Default depends on the language standard version.
--
-- ObjC selector: @- setMathMode:@
setMathMode :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> MTLMathMode -> IO ()
setMathMode mtlCompileOptions  value =
    sendMsg mtlCompileOptions (mkSelector "setMathMode:") retVoid [argCLong (coerce value)]

-- | mathFloatingPointFunctions
--
-- Sets the default math functions for single precision floating-point. Default is @MTLMathFloatingPointFunctionsFast@.
--
-- ObjC selector: @- mathFloatingPointFunctions@
mathFloatingPointFunctions :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> IO MTLMathFloatingPointFunctions
mathFloatingPointFunctions mtlCompileOptions  =
    fmap (coerce :: CLong -> MTLMathFloatingPointFunctions) $ sendMsg mtlCompileOptions (mkSelector "mathFloatingPointFunctions") retCLong []

-- | mathFloatingPointFunctions
--
-- Sets the default math functions for single precision floating-point. Default is @MTLMathFloatingPointFunctionsFast@.
--
-- ObjC selector: @- setMathFloatingPointFunctions:@
setMathFloatingPointFunctions :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> MTLMathFloatingPointFunctions -> IO ()
setMathFloatingPointFunctions mtlCompileOptions  value =
    sendMsg mtlCompileOptions (mkSelector "setMathFloatingPointFunctions:") retVoid [argCLong (coerce value)]

-- | languageVersion
--
-- set the metal language version used to interpret the source.
--
-- ObjC selector: @- languageVersion@
languageVersion :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> IO MTLLanguageVersion
languageVersion mtlCompileOptions  =
    fmap (coerce :: CULong -> MTLLanguageVersion) $ sendMsg mtlCompileOptions (mkSelector "languageVersion") retCULong []

-- | languageVersion
--
-- set the metal language version used to interpret the source.
--
-- ObjC selector: @- setLanguageVersion:@
setLanguageVersion :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> MTLLanguageVersion -> IO ()
setLanguageVersion mtlCompileOptions  value =
    sendMsg mtlCompileOptions (mkSelector "setLanguageVersion:") retVoid [argCULong (coerce value)]

-- | type
--
-- Which type the library should be compiled as. The default value is MTLLibraryTypeExecutable.
--
-- MTLLibraryTypeExecutable is suitable to build a library of "kernel", "vertex" and "fragment" qualified functions. MTLLibraryType is suitable when the compilation result will instead be used to instantiate a MTLDynamicLibrary. MTLDynamicLibrary contains no qualified functions, but it's unqualified functions and variables can be used as an external dependency for compiling other libraries.
--
-- ObjC selector: @- libraryType@
libraryType :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> IO MTLLibraryType
libraryType mtlCompileOptions  =
    fmap (coerce :: CLong -> MTLLibraryType) $ sendMsg mtlCompileOptions (mkSelector "libraryType") retCLong []

-- | type
--
-- Which type the library should be compiled as. The default value is MTLLibraryTypeExecutable.
--
-- MTLLibraryTypeExecutable is suitable to build a library of "kernel", "vertex" and "fragment" qualified functions. MTLLibraryType is suitable when the compilation result will instead be used to instantiate a MTLDynamicLibrary. MTLDynamicLibrary contains no qualified functions, but it's unqualified functions and variables can be used as an external dependency for compiling other libraries.
--
-- ObjC selector: @- setLibraryType:@
setLibraryType :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> MTLLibraryType -> IO ()
setLibraryType mtlCompileOptions  value =
    sendMsg mtlCompileOptions (mkSelector "setLibraryType:") retVoid [argCLong (coerce value)]

-- | installName
--
-- The install name of this dynamic library.
--
-- The install name is used when a pipeline state is created that depends, directly or indirectly, on a dynamic library. The installName is embedded into any other MTLLibrary that links against the compilation result. This property should be set such that the dynamic library can be found in the file system at the time a pipeline state is created. Specify one of: - an absolute path to a file from which the dynamic library can be loaded, or - a path relative to \@executable_path, where \@executable_path is substituted with the directory name from which the MTLLibrary containing the MTLFunction entrypoint used to create the pipeline state is loaded, or - a path relative to \@loader_path, where \@loader_path is substituted with the directory name from which the MTLLibrary with the reference to this installName embedded is loaded. The first is appropriate for MTLDynamicLibrary written to the file-system using its serializeToURL:error: method on the current device. The others are appropriate when the MTLDynamicLibrary is installed as part of a bundle or app, where the absolute path is not known. This property is ignored when the type property is not set to MTLLibraryTypeDynamic. This propery should not be null if the property type is set to MTLLibraryTypeDynamic: the compilation will fail in that scenario.
--
-- ObjC selector: @- installName@
installName :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> IO (Id NSString)
installName mtlCompileOptions  =
    sendMsg mtlCompileOptions (mkSelector "installName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | installName
--
-- The install name of this dynamic library.
--
-- The install name is used when a pipeline state is created that depends, directly or indirectly, on a dynamic library. The installName is embedded into any other MTLLibrary that links against the compilation result. This property should be set such that the dynamic library can be found in the file system at the time a pipeline state is created. Specify one of: - an absolute path to a file from which the dynamic library can be loaded, or - a path relative to \@executable_path, where \@executable_path is substituted with the directory name from which the MTLLibrary containing the MTLFunction entrypoint used to create the pipeline state is loaded, or - a path relative to \@loader_path, where \@loader_path is substituted with the directory name from which the MTLLibrary with the reference to this installName embedded is loaded. The first is appropriate for MTLDynamicLibrary written to the file-system using its serializeToURL:error: method on the current device. The others are appropriate when the MTLDynamicLibrary is installed as part of a bundle or app, where the absolute path is not known. This property is ignored when the type property is not set to MTLLibraryTypeDynamic. This propery should not be null if the property type is set to MTLLibraryTypeDynamic: the compilation will fail in that scenario.
--
-- ObjC selector: @- setInstallName:@
setInstallName :: (IsMTLCompileOptions mtlCompileOptions, IsNSString value) => mtlCompileOptions -> value -> IO ()
setInstallName mtlCompileOptions  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtlCompileOptions (mkSelector "setInstallName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | libraries
--
-- A set of MTLDynamicLibrary instances to link against. The installName of the provided MTLDynamicLibrary is embedded into the compilation result. When a function from the resulting MTLLibrary is used (either as an MTLFunction, or as an to create a pipeline state, the embedded install names are used to automatically load the MTLDynamicLibrary instances. This property can be null if no libraries should be automatically loaded, either because the MTLLibrary has no external dependencies, or because you will use preloadedLibraries to specify the libraries to use at pipeline creation time.
--
-- ObjC selector: @- libraries@
libraries :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> IO (Id NSArray)
libraries mtlCompileOptions  =
    sendMsg mtlCompileOptions (mkSelector "libraries") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | libraries
--
-- A set of MTLDynamicLibrary instances to link against. The installName of the provided MTLDynamicLibrary is embedded into the compilation result. When a function from the resulting MTLLibrary is used (either as an MTLFunction, or as an to create a pipeline state, the embedded install names are used to automatically load the MTLDynamicLibrary instances. This property can be null if no libraries should be automatically loaded, either because the MTLLibrary has no external dependencies, or because you will use preloadedLibraries to specify the libraries to use at pipeline creation time.
--
-- ObjC selector: @- setLibraries:@
setLibraries :: (IsMTLCompileOptions mtlCompileOptions, IsNSArray value) => mtlCompileOptions -> value -> IO ()
setLibraries mtlCompileOptions  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtlCompileOptions (mkSelector "setLibraries:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | preserveInvariance
--
-- If YES,  set the compiler to compile shaders to preserve invariance.  The default is false.
--
-- ObjC selector: @- preserveInvariance@
preserveInvariance :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> IO Bool
preserveInvariance mtlCompileOptions  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlCompileOptions (mkSelector "preserveInvariance") retCULong []

-- | preserveInvariance
--
-- If YES,  set the compiler to compile shaders to preserve invariance.  The default is false.
--
-- ObjC selector: @- setPreserveInvariance:@
setPreserveInvariance :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> Bool -> IO ()
setPreserveInvariance mtlCompileOptions  value =
    sendMsg mtlCompileOptions (mkSelector "setPreserveInvariance:") retVoid [argCULong (if value then 1 else 0)]

-- | optimizationLevel
--
-- Sets the compiler optimization level.
--
-- ObjC selector: @- optimizationLevel@
optimizationLevel :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> IO MTLLibraryOptimizationLevel
optimizationLevel mtlCompileOptions  =
    fmap (coerce :: CLong -> MTLLibraryOptimizationLevel) $ sendMsg mtlCompileOptions (mkSelector "optimizationLevel") retCLong []

-- | optimizationLevel
--
-- Sets the compiler optimization level.
--
-- ObjC selector: @- setOptimizationLevel:@
setOptimizationLevel :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> MTLLibraryOptimizationLevel -> IO ()
setOptimizationLevel mtlCompileOptions  value =
    sendMsg mtlCompileOptions (mkSelector "setOptimizationLevel:") retVoid [argCLong (coerce value)]

-- | Adds a compiler command to force the default visibility of symbols to be hidden
--
-- ObjC selector: @- compileSymbolVisibility@
compileSymbolVisibility :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> IO MTLCompileSymbolVisibility
compileSymbolVisibility mtlCompileOptions  =
    fmap (coerce :: CLong -> MTLCompileSymbolVisibility) $ sendMsg mtlCompileOptions (mkSelector "compileSymbolVisibility") retCLong []

-- | Adds a compiler command to force the default visibility of symbols to be hidden
--
-- ObjC selector: @- setCompileSymbolVisibility:@
setCompileSymbolVisibility :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> MTLCompileSymbolVisibility -> IO ()
setCompileSymbolVisibility mtlCompileOptions  value =
    sendMsg mtlCompileOptions (mkSelector "setCompileSymbolVisibility:") retVoid [argCLong (coerce value)]

-- | allowReferencingUndefinedSymbols
--
-- Adds a compiler command to allow the reference of undefined symbols
--
-- ObjC selector: @- allowReferencingUndefinedSymbols@
allowReferencingUndefinedSymbols :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> IO Bool
allowReferencingUndefinedSymbols mtlCompileOptions  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlCompileOptions (mkSelector "allowReferencingUndefinedSymbols") retCULong []

-- | allowReferencingUndefinedSymbols
--
-- Adds a compiler command to allow the reference of undefined symbols
--
-- ObjC selector: @- setAllowReferencingUndefinedSymbols:@
setAllowReferencingUndefinedSymbols :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> Bool -> IO ()
setAllowReferencingUndefinedSymbols mtlCompileOptions  value =
    sendMsg mtlCompileOptions (mkSelector "setAllowReferencingUndefinedSymbols:") retVoid [argCULong (if value then 1 else 0)]

-- | maxTotalThreadsPerThreadgroup
--
-- Adds a compiler command to specify the total threads per threadgroup
--
-- ObjC selector: @- maxTotalThreadsPerThreadgroup@
maxTotalThreadsPerThreadgroup :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> IO CULong
maxTotalThreadsPerThreadgroup mtlCompileOptions  =
    sendMsg mtlCompileOptions (mkSelector "maxTotalThreadsPerThreadgroup") retCULong []

-- | maxTotalThreadsPerThreadgroup
--
-- Adds a compiler command to specify the total threads per threadgroup
--
-- ObjC selector: @- setMaxTotalThreadsPerThreadgroup:@
setMaxTotalThreadsPerThreadgroup :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> CULong -> IO ()
setMaxTotalThreadsPerThreadgroup mtlCompileOptions  value =
    sendMsg mtlCompileOptions (mkSelector "setMaxTotalThreadsPerThreadgroup:") retVoid [argCULong value]

-- | enableLogging
--
-- If YES,  set the compiler to enable any logging in the shader. The default is false.
--
-- ObjC selector: @- enableLogging@
enableLogging :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> IO Bool
enableLogging mtlCompileOptions  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlCompileOptions (mkSelector "enableLogging") retCULong []

-- | enableLogging
--
-- If YES,  set the compiler to enable any logging in the shader. The default is false.
--
-- ObjC selector: @- setEnableLogging:@
setEnableLogging :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> Bool -> IO ()
setEnableLogging mtlCompileOptions  value =
    sendMsg mtlCompileOptions (mkSelector "setEnableLogging:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @preprocessorMacros@
preprocessorMacrosSelector :: Selector
preprocessorMacrosSelector = mkSelector "preprocessorMacros"

-- | @Selector@ for @setPreprocessorMacros:@
setPreprocessorMacrosSelector :: Selector
setPreprocessorMacrosSelector = mkSelector "setPreprocessorMacros:"

-- | @Selector@ for @fastMathEnabled@
fastMathEnabledSelector :: Selector
fastMathEnabledSelector = mkSelector "fastMathEnabled"

-- | @Selector@ for @setFastMathEnabled:@
setFastMathEnabledSelector :: Selector
setFastMathEnabledSelector = mkSelector "setFastMathEnabled:"

-- | @Selector@ for @mathMode@
mathModeSelector :: Selector
mathModeSelector = mkSelector "mathMode"

-- | @Selector@ for @setMathMode:@
setMathModeSelector :: Selector
setMathModeSelector = mkSelector "setMathMode:"

-- | @Selector@ for @mathFloatingPointFunctions@
mathFloatingPointFunctionsSelector :: Selector
mathFloatingPointFunctionsSelector = mkSelector "mathFloatingPointFunctions"

-- | @Selector@ for @setMathFloatingPointFunctions:@
setMathFloatingPointFunctionsSelector :: Selector
setMathFloatingPointFunctionsSelector = mkSelector "setMathFloatingPointFunctions:"

-- | @Selector@ for @languageVersion@
languageVersionSelector :: Selector
languageVersionSelector = mkSelector "languageVersion"

-- | @Selector@ for @setLanguageVersion:@
setLanguageVersionSelector :: Selector
setLanguageVersionSelector = mkSelector "setLanguageVersion:"

-- | @Selector@ for @libraryType@
libraryTypeSelector :: Selector
libraryTypeSelector = mkSelector "libraryType"

-- | @Selector@ for @setLibraryType:@
setLibraryTypeSelector :: Selector
setLibraryTypeSelector = mkSelector "setLibraryType:"

-- | @Selector@ for @installName@
installNameSelector :: Selector
installNameSelector = mkSelector "installName"

-- | @Selector@ for @setInstallName:@
setInstallNameSelector :: Selector
setInstallNameSelector = mkSelector "setInstallName:"

-- | @Selector@ for @libraries@
librariesSelector :: Selector
librariesSelector = mkSelector "libraries"

-- | @Selector@ for @setLibraries:@
setLibrariesSelector :: Selector
setLibrariesSelector = mkSelector "setLibraries:"

-- | @Selector@ for @preserveInvariance@
preserveInvarianceSelector :: Selector
preserveInvarianceSelector = mkSelector "preserveInvariance"

-- | @Selector@ for @setPreserveInvariance:@
setPreserveInvarianceSelector :: Selector
setPreserveInvarianceSelector = mkSelector "setPreserveInvariance:"

-- | @Selector@ for @optimizationLevel@
optimizationLevelSelector :: Selector
optimizationLevelSelector = mkSelector "optimizationLevel"

-- | @Selector@ for @setOptimizationLevel:@
setOptimizationLevelSelector :: Selector
setOptimizationLevelSelector = mkSelector "setOptimizationLevel:"

-- | @Selector@ for @compileSymbolVisibility@
compileSymbolVisibilitySelector :: Selector
compileSymbolVisibilitySelector = mkSelector "compileSymbolVisibility"

-- | @Selector@ for @setCompileSymbolVisibility:@
setCompileSymbolVisibilitySelector :: Selector
setCompileSymbolVisibilitySelector = mkSelector "setCompileSymbolVisibility:"

-- | @Selector@ for @allowReferencingUndefinedSymbols@
allowReferencingUndefinedSymbolsSelector :: Selector
allowReferencingUndefinedSymbolsSelector = mkSelector "allowReferencingUndefinedSymbols"

-- | @Selector@ for @setAllowReferencingUndefinedSymbols:@
setAllowReferencingUndefinedSymbolsSelector :: Selector
setAllowReferencingUndefinedSymbolsSelector = mkSelector "setAllowReferencingUndefinedSymbols:"

-- | @Selector@ for @maxTotalThreadsPerThreadgroup@
maxTotalThreadsPerThreadgroupSelector :: Selector
maxTotalThreadsPerThreadgroupSelector = mkSelector "maxTotalThreadsPerThreadgroup"

-- | @Selector@ for @setMaxTotalThreadsPerThreadgroup:@
setMaxTotalThreadsPerThreadgroupSelector :: Selector
setMaxTotalThreadsPerThreadgroupSelector = mkSelector "setMaxTotalThreadsPerThreadgroup:"

-- | @Selector@ for @enableLogging@
enableLoggingSelector :: Selector
enableLoggingSelector = mkSelector "enableLogging"

-- | @Selector@ for @setEnableLogging:@
setEnableLoggingSelector :: Selector
setEnableLoggingSelector = mkSelector "setEnableLogging:"

