{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , allowReferencingUndefinedSymbolsSelector
  , compileSymbolVisibilitySelector
  , enableLoggingSelector
  , fastMathEnabledSelector
  , installNameSelector
  , languageVersionSelector
  , librariesSelector
  , libraryTypeSelector
  , mathFloatingPointFunctionsSelector
  , mathModeSelector
  , maxTotalThreadsPerThreadgroupSelector
  , optimizationLevelSelector
  , preprocessorMacrosSelector
  , preserveInvarianceSelector
  , setAllowReferencingUndefinedSymbolsSelector
  , setCompileSymbolVisibilitySelector
  , setEnableLoggingSelector
  , setFastMathEnabledSelector
  , setInstallNameSelector
  , setLanguageVersionSelector
  , setLibrariesSelector
  , setLibraryTypeSelector
  , setMathFloatingPointFunctionsSelector
  , setMathModeSelector
  , setMaxTotalThreadsPerThreadgroupSelector
  , setOptimizationLevelSelector
  , setPreprocessorMacrosSelector
  , setPreserveInvarianceSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
preprocessorMacros mtlCompileOptions =
  sendMessage mtlCompileOptions preprocessorMacrosSelector

-- | preprocessorNames
--
-- List of preprocessor macros to consider to when compiling this program. Specified as key value pairs, using a NSDictionary. The keys must be NSString objects and values can be either NSString or NSNumber objects.
--
-- The default value is nil.
--
-- ObjC selector: @- setPreprocessorMacros:@
setPreprocessorMacros :: (IsMTLCompileOptions mtlCompileOptions, IsNSDictionary value) => mtlCompileOptions -> value -> IO ()
setPreprocessorMacros mtlCompileOptions value =
  sendMessage mtlCompileOptions setPreprocessorMacrosSelector (toNSDictionary value)

-- | fastMathEnabled
--
-- If YES, enables the compiler to perform optimizations for floating-point arithmetic that may violate the IEEE 754 standard. It also enables the high precision variant of math functions for single precision floating-point scalar and vector types. fastMathEnabled defaults to YES.
--
-- ObjC selector: @- fastMathEnabled@
fastMathEnabled :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> IO Bool
fastMathEnabled mtlCompileOptions =
  sendMessage mtlCompileOptions fastMathEnabledSelector

-- | fastMathEnabled
--
-- If YES, enables the compiler to perform optimizations for floating-point arithmetic that may violate the IEEE 754 standard. It also enables the high precision variant of math functions for single precision floating-point scalar and vector types. fastMathEnabled defaults to YES.
--
-- ObjC selector: @- setFastMathEnabled:@
setFastMathEnabled :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> Bool -> IO ()
setFastMathEnabled mtlCompileOptions value =
  sendMessage mtlCompileOptions setFastMathEnabledSelector value

-- | mathMode
--
-- Sets the floating-point arithmetic optimizations. Default depends on the language standard version.
--
-- ObjC selector: @- mathMode@
mathMode :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> IO MTLMathMode
mathMode mtlCompileOptions =
  sendMessage mtlCompileOptions mathModeSelector

-- | mathMode
--
-- Sets the floating-point arithmetic optimizations. Default depends on the language standard version.
--
-- ObjC selector: @- setMathMode:@
setMathMode :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> MTLMathMode -> IO ()
setMathMode mtlCompileOptions value =
  sendMessage mtlCompileOptions setMathModeSelector value

-- | mathFloatingPointFunctions
--
-- Sets the default math functions for single precision floating-point. Default is @MTLMathFloatingPointFunctionsFast@.
--
-- ObjC selector: @- mathFloatingPointFunctions@
mathFloatingPointFunctions :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> IO MTLMathFloatingPointFunctions
mathFloatingPointFunctions mtlCompileOptions =
  sendMessage mtlCompileOptions mathFloatingPointFunctionsSelector

-- | mathFloatingPointFunctions
--
-- Sets the default math functions for single precision floating-point. Default is @MTLMathFloatingPointFunctionsFast@.
--
-- ObjC selector: @- setMathFloatingPointFunctions:@
setMathFloatingPointFunctions :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> MTLMathFloatingPointFunctions -> IO ()
setMathFloatingPointFunctions mtlCompileOptions value =
  sendMessage mtlCompileOptions setMathFloatingPointFunctionsSelector value

-- | languageVersion
--
-- set the metal language version used to interpret the source.
--
-- ObjC selector: @- languageVersion@
languageVersion :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> IO MTLLanguageVersion
languageVersion mtlCompileOptions =
  sendMessage mtlCompileOptions languageVersionSelector

-- | languageVersion
--
-- set the metal language version used to interpret the source.
--
-- ObjC selector: @- setLanguageVersion:@
setLanguageVersion :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> MTLLanguageVersion -> IO ()
setLanguageVersion mtlCompileOptions value =
  sendMessage mtlCompileOptions setLanguageVersionSelector value

-- | type
--
-- Which type the library should be compiled as. The default value is MTLLibraryTypeExecutable.
--
-- MTLLibraryTypeExecutable is suitable to build a library of "kernel", "vertex" and "fragment" qualified functions. MTLLibraryType is suitable when the compilation result will instead be used to instantiate a MTLDynamicLibrary. MTLDynamicLibrary contains no qualified functions, but it's unqualified functions and variables can be used as an external dependency for compiling other libraries.
--
-- ObjC selector: @- libraryType@
libraryType :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> IO MTLLibraryType
libraryType mtlCompileOptions =
  sendMessage mtlCompileOptions libraryTypeSelector

-- | type
--
-- Which type the library should be compiled as. The default value is MTLLibraryTypeExecutable.
--
-- MTLLibraryTypeExecutable is suitable to build a library of "kernel", "vertex" and "fragment" qualified functions. MTLLibraryType is suitable when the compilation result will instead be used to instantiate a MTLDynamicLibrary. MTLDynamicLibrary contains no qualified functions, but it's unqualified functions and variables can be used as an external dependency for compiling other libraries.
--
-- ObjC selector: @- setLibraryType:@
setLibraryType :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> MTLLibraryType -> IO ()
setLibraryType mtlCompileOptions value =
  sendMessage mtlCompileOptions setLibraryTypeSelector value

-- | installName
--
-- The install name of this dynamic library.
--
-- The install name is used when a pipeline state is created that depends, directly or indirectly, on a dynamic library. The installName is embedded into any other MTLLibrary that links against the compilation result. This property should be set such that the dynamic library can be found in the file system at the time a pipeline state is created. Specify one of: - an absolute path to a file from which the dynamic library can be loaded, or - a path relative to \@executable_path, where \@executable_path is substituted with the directory name from which the MTLLibrary containing the MTLFunction entrypoint used to create the pipeline state is loaded, or - a path relative to \@loader_path, where \@loader_path is substituted with the directory name from which the MTLLibrary with the reference to this installName embedded is loaded. The first is appropriate for MTLDynamicLibrary written to the file-system using its serializeToURL:error: method on the current device. The others are appropriate when the MTLDynamicLibrary is installed as part of a bundle or app, where the absolute path is not known. This property is ignored when the type property is not set to MTLLibraryTypeDynamic. This propery should not be null if the property type is set to MTLLibraryTypeDynamic: the compilation will fail in that scenario.
--
-- ObjC selector: @- installName@
installName :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> IO (Id NSString)
installName mtlCompileOptions =
  sendMessage mtlCompileOptions installNameSelector

-- | installName
--
-- The install name of this dynamic library.
--
-- The install name is used when a pipeline state is created that depends, directly or indirectly, on a dynamic library. The installName is embedded into any other MTLLibrary that links against the compilation result. This property should be set such that the dynamic library can be found in the file system at the time a pipeline state is created. Specify one of: - an absolute path to a file from which the dynamic library can be loaded, or - a path relative to \@executable_path, where \@executable_path is substituted with the directory name from which the MTLLibrary containing the MTLFunction entrypoint used to create the pipeline state is loaded, or - a path relative to \@loader_path, where \@loader_path is substituted with the directory name from which the MTLLibrary with the reference to this installName embedded is loaded. The first is appropriate for MTLDynamicLibrary written to the file-system using its serializeToURL:error: method on the current device. The others are appropriate when the MTLDynamicLibrary is installed as part of a bundle or app, where the absolute path is not known. This property is ignored when the type property is not set to MTLLibraryTypeDynamic. This propery should not be null if the property type is set to MTLLibraryTypeDynamic: the compilation will fail in that scenario.
--
-- ObjC selector: @- setInstallName:@
setInstallName :: (IsMTLCompileOptions mtlCompileOptions, IsNSString value) => mtlCompileOptions -> value -> IO ()
setInstallName mtlCompileOptions value =
  sendMessage mtlCompileOptions setInstallNameSelector (toNSString value)

-- | libraries
--
-- A set of MTLDynamicLibrary instances to link against. The installName of the provided MTLDynamicLibrary is embedded into the compilation result. When a function from the resulting MTLLibrary is used (either as an MTLFunction, or as an to create a pipeline state, the embedded install names are used to automatically load the MTLDynamicLibrary instances. This property can be null if no libraries should be automatically loaded, either because the MTLLibrary has no external dependencies, or because you will use preloadedLibraries to specify the libraries to use at pipeline creation time.
--
-- ObjC selector: @- libraries@
libraries :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> IO (Id NSArray)
libraries mtlCompileOptions =
  sendMessage mtlCompileOptions librariesSelector

-- | libraries
--
-- A set of MTLDynamicLibrary instances to link against. The installName of the provided MTLDynamicLibrary is embedded into the compilation result. When a function from the resulting MTLLibrary is used (either as an MTLFunction, or as an to create a pipeline state, the embedded install names are used to automatically load the MTLDynamicLibrary instances. This property can be null if no libraries should be automatically loaded, either because the MTLLibrary has no external dependencies, or because you will use preloadedLibraries to specify the libraries to use at pipeline creation time.
--
-- ObjC selector: @- setLibraries:@
setLibraries :: (IsMTLCompileOptions mtlCompileOptions, IsNSArray value) => mtlCompileOptions -> value -> IO ()
setLibraries mtlCompileOptions value =
  sendMessage mtlCompileOptions setLibrariesSelector (toNSArray value)

-- | preserveInvariance
--
-- If YES,  set the compiler to compile shaders to preserve invariance.  The default is false.
--
-- ObjC selector: @- preserveInvariance@
preserveInvariance :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> IO Bool
preserveInvariance mtlCompileOptions =
  sendMessage mtlCompileOptions preserveInvarianceSelector

-- | preserveInvariance
--
-- If YES,  set the compiler to compile shaders to preserve invariance.  The default is false.
--
-- ObjC selector: @- setPreserveInvariance:@
setPreserveInvariance :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> Bool -> IO ()
setPreserveInvariance mtlCompileOptions value =
  sendMessage mtlCompileOptions setPreserveInvarianceSelector value

-- | optimizationLevel
--
-- Sets the compiler optimization level.
--
-- ObjC selector: @- optimizationLevel@
optimizationLevel :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> IO MTLLibraryOptimizationLevel
optimizationLevel mtlCompileOptions =
  sendMessage mtlCompileOptions optimizationLevelSelector

-- | optimizationLevel
--
-- Sets the compiler optimization level.
--
-- ObjC selector: @- setOptimizationLevel:@
setOptimizationLevel :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> MTLLibraryOptimizationLevel -> IO ()
setOptimizationLevel mtlCompileOptions value =
  sendMessage mtlCompileOptions setOptimizationLevelSelector value

-- | Adds a compiler command to force the default visibility of symbols to be hidden
--
-- ObjC selector: @- compileSymbolVisibility@
compileSymbolVisibility :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> IO MTLCompileSymbolVisibility
compileSymbolVisibility mtlCompileOptions =
  sendMessage mtlCompileOptions compileSymbolVisibilitySelector

-- | Adds a compiler command to force the default visibility of symbols to be hidden
--
-- ObjC selector: @- setCompileSymbolVisibility:@
setCompileSymbolVisibility :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> MTLCompileSymbolVisibility -> IO ()
setCompileSymbolVisibility mtlCompileOptions value =
  sendMessage mtlCompileOptions setCompileSymbolVisibilitySelector value

-- | allowReferencingUndefinedSymbols
--
-- Adds a compiler command to allow the reference of undefined symbols
--
-- ObjC selector: @- allowReferencingUndefinedSymbols@
allowReferencingUndefinedSymbols :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> IO Bool
allowReferencingUndefinedSymbols mtlCompileOptions =
  sendMessage mtlCompileOptions allowReferencingUndefinedSymbolsSelector

-- | allowReferencingUndefinedSymbols
--
-- Adds a compiler command to allow the reference of undefined symbols
--
-- ObjC selector: @- setAllowReferencingUndefinedSymbols:@
setAllowReferencingUndefinedSymbols :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> Bool -> IO ()
setAllowReferencingUndefinedSymbols mtlCompileOptions value =
  sendMessage mtlCompileOptions setAllowReferencingUndefinedSymbolsSelector value

-- | maxTotalThreadsPerThreadgroup
--
-- Adds a compiler command to specify the total threads per threadgroup
--
-- ObjC selector: @- maxTotalThreadsPerThreadgroup@
maxTotalThreadsPerThreadgroup :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> IO CULong
maxTotalThreadsPerThreadgroup mtlCompileOptions =
  sendMessage mtlCompileOptions maxTotalThreadsPerThreadgroupSelector

-- | maxTotalThreadsPerThreadgroup
--
-- Adds a compiler command to specify the total threads per threadgroup
--
-- ObjC selector: @- setMaxTotalThreadsPerThreadgroup:@
setMaxTotalThreadsPerThreadgroup :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> CULong -> IO ()
setMaxTotalThreadsPerThreadgroup mtlCompileOptions value =
  sendMessage mtlCompileOptions setMaxTotalThreadsPerThreadgroupSelector value

-- | enableLogging
--
-- If YES,  set the compiler to enable any logging in the shader. The default is false.
--
-- ObjC selector: @- enableLogging@
enableLogging :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> IO Bool
enableLogging mtlCompileOptions =
  sendMessage mtlCompileOptions enableLoggingSelector

-- | enableLogging
--
-- If YES,  set the compiler to enable any logging in the shader. The default is false.
--
-- ObjC selector: @- setEnableLogging:@
setEnableLogging :: IsMTLCompileOptions mtlCompileOptions => mtlCompileOptions -> Bool -> IO ()
setEnableLogging mtlCompileOptions value =
  sendMessage mtlCompileOptions setEnableLoggingSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @preprocessorMacros@
preprocessorMacrosSelector :: Selector '[] (Id NSDictionary)
preprocessorMacrosSelector = mkSelector "preprocessorMacros"

-- | @Selector@ for @setPreprocessorMacros:@
setPreprocessorMacrosSelector :: Selector '[Id NSDictionary] ()
setPreprocessorMacrosSelector = mkSelector "setPreprocessorMacros:"

-- | @Selector@ for @fastMathEnabled@
fastMathEnabledSelector :: Selector '[] Bool
fastMathEnabledSelector = mkSelector "fastMathEnabled"

-- | @Selector@ for @setFastMathEnabled:@
setFastMathEnabledSelector :: Selector '[Bool] ()
setFastMathEnabledSelector = mkSelector "setFastMathEnabled:"

-- | @Selector@ for @mathMode@
mathModeSelector :: Selector '[] MTLMathMode
mathModeSelector = mkSelector "mathMode"

-- | @Selector@ for @setMathMode:@
setMathModeSelector :: Selector '[MTLMathMode] ()
setMathModeSelector = mkSelector "setMathMode:"

-- | @Selector@ for @mathFloatingPointFunctions@
mathFloatingPointFunctionsSelector :: Selector '[] MTLMathFloatingPointFunctions
mathFloatingPointFunctionsSelector = mkSelector "mathFloatingPointFunctions"

-- | @Selector@ for @setMathFloatingPointFunctions:@
setMathFloatingPointFunctionsSelector :: Selector '[MTLMathFloatingPointFunctions] ()
setMathFloatingPointFunctionsSelector = mkSelector "setMathFloatingPointFunctions:"

-- | @Selector@ for @languageVersion@
languageVersionSelector :: Selector '[] MTLLanguageVersion
languageVersionSelector = mkSelector "languageVersion"

-- | @Selector@ for @setLanguageVersion:@
setLanguageVersionSelector :: Selector '[MTLLanguageVersion] ()
setLanguageVersionSelector = mkSelector "setLanguageVersion:"

-- | @Selector@ for @libraryType@
libraryTypeSelector :: Selector '[] MTLLibraryType
libraryTypeSelector = mkSelector "libraryType"

-- | @Selector@ for @setLibraryType:@
setLibraryTypeSelector :: Selector '[MTLLibraryType] ()
setLibraryTypeSelector = mkSelector "setLibraryType:"

-- | @Selector@ for @installName@
installNameSelector :: Selector '[] (Id NSString)
installNameSelector = mkSelector "installName"

-- | @Selector@ for @setInstallName:@
setInstallNameSelector :: Selector '[Id NSString] ()
setInstallNameSelector = mkSelector "setInstallName:"

-- | @Selector@ for @libraries@
librariesSelector :: Selector '[] (Id NSArray)
librariesSelector = mkSelector "libraries"

-- | @Selector@ for @setLibraries:@
setLibrariesSelector :: Selector '[Id NSArray] ()
setLibrariesSelector = mkSelector "setLibraries:"

-- | @Selector@ for @preserveInvariance@
preserveInvarianceSelector :: Selector '[] Bool
preserveInvarianceSelector = mkSelector "preserveInvariance"

-- | @Selector@ for @setPreserveInvariance:@
setPreserveInvarianceSelector :: Selector '[Bool] ()
setPreserveInvarianceSelector = mkSelector "setPreserveInvariance:"

-- | @Selector@ for @optimizationLevel@
optimizationLevelSelector :: Selector '[] MTLLibraryOptimizationLevel
optimizationLevelSelector = mkSelector "optimizationLevel"

-- | @Selector@ for @setOptimizationLevel:@
setOptimizationLevelSelector :: Selector '[MTLLibraryOptimizationLevel] ()
setOptimizationLevelSelector = mkSelector "setOptimizationLevel:"

-- | @Selector@ for @compileSymbolVisibility@
compileSymbolVisibilitySelector :: Selector '[] MTLCompileSymbolVisibility
compileSymbolVisibilitySelector = mkSelector "compileSymbolVisibility"

-- | @Selector@ for @setCompileSymbolVisibility:@
setCompileSymbolVisibilitySelector :: Selector '[MTLCompileSymbolVisibility] ()
setCompileSymbolVisibilitySelector = mkSelector "setCompileSymbolVisibility:"

-- | @Selector@ for @allowReferencingUndefinedSymbols@
allowReferencingUndefinedSymbolsSelector :: Selector '[] Bool
allowReferencingUndefinedSymbolsSelector = mkSelector "allowReferencingUndefinedSymbols"

-- | @Selector@ for @setAllowReferencingUndefinedSymbols:@
setAllowReferencingUndefinedSymbolsSelector :: Selector '[Bool] ()
setAllowReferencingUndefinedSymbolsSelector = mkSelector "setAllowReferencingUndefinedSymbols:"

-- | @Selector@ for @maxTotalThreadsPerThreadgroup@
maxTotalThreadsPerThreadgroupSelector :: Selector '[] CULong
maxTotalThreadsPerThreadgroupSelector = mkSelector "maxTotalThreadsPerThreadgroup"

-- | @Selector@ for @setMaxTotalThreadsPerThreadgroup:@
setMaxTotalThreadsPerThreadgroupSelector :: Selector '[CULong] ()
setMaxTotalThreadsPerThreadgroupSelector = mkSelector "setMaxTotalThreadsPerThreadgroup:"

-- | @Selector@ for @enableLogging@
enableLoggingSelector :: Selector '[] Bool
enableLoggingSelector = mkSelector "enableLogging"

-- | @Selector@ for @setEnableLogging:@
setEnableLoggingSelector :: Selector '[Bool] ()
setEnableLoggingSelector = mkSelector "setEnableLogging:"

