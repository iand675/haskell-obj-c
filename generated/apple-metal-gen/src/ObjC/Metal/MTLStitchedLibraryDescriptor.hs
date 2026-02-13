{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTLStitchedLibraryDescriptor
--
-- A container for the graphs and functions needed to create the stitched functions described by the graphs.
--
-- Generated bindings for @MTLStitchedLibraryDescriptor@.
module ObjC.Metal.MTLStitchedLibraryDescriptor
  ( MTLStitchedLibraryDescriptor
  , IsMTLStitchedLibraryDescriptor(..)
  , functionGraphs
  , setFunctionGraphs
  , functions
  , setFunctions
  , binaryArchives
  , setBinaryArchives
  , options
  , setOptions
  , binaryArchivesSelector
  , functionGraphsSelector
  , functionsSelector
  , optionsSelector
  , setBinaryArchivesSelector
  , setFunctionGraphsSelector
  , setFunctionsSelector
  , setOptionsSelector

  -- * Enum types
  , MTLStitchedLibraryOptions(MTLStitchedLibraryOptions)
  , pattern MTLStitchedLibraryOptionNone
  , pattern MTLStitchedLibraryOptionFailOnBinaryArchiveMiss
  , pattern MTLStitchedLibraryOptionStoreLibraryInMetalPipelinesScript

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

-- | @- functionGraphs@
functionGraphs :: IsMTLStitchedLibraryDescriptor mtlStitchedLibraryDescriptor => mtlStitchedLibraryDescriptor -> IO (Id NSArray)
functionGraphs mtlStitchedLibraryDescriptor =
  sendMessage mtlStitchedLibraryDescriptor functionGraphsSelector

-- | @- setFunctionGraphs:@
setFunctionGraphs :: (IsMTLStitchedLibraryDescriptor mtlStitchedLibraryDescriptor, IsNSArray value) => mtlStitchedLibraryDescriptor -> value -> IO ()
setFunctionGraphs mtlStitchedLibraryDescriptor value =
  sendMessage mtlStitchedLibraryDescriptor setFunctionGraphsSelector (toNSArray value)

-- | @- functions@
functions :: IsMTLStitchedLibraryDescriptor mtlStitchedLibraryDescriptor => mtlStitchedLibraryDescriptor -> IO (Id NSArray)
functions mtlStitchedLibraryDescriptor =
  sendMessage mtlStitchedLibraryDescriptor functionsSelector

-- | @- setFunctions:@
setFunctions :: (IsMTLStitchedLibraryDescriptor mtlStitchedLibraryDescriptor, IsNSArray value) => mtlStitchedLibraryDescriptor -> value -> IO ()
setFunctions mtlStitchedLibraryDescriptor value =
  sendMessage mtlStitchedLibraryDescriptor setFunctionsSelector (toNSArray value)

-- | binaryArchives
--
-- The array of archives to be searched.
--
-- Binary archives to be searched for precompiled stitched libraries during the compilation of this library.
--
-- ObjC selector: @- binaryArchives@
binaryArchives :: IsMTLStitchedLibraryDescriptor mtlStitchedLibraryDescriptor => mtlStitchedLibraryDescriptor -> IO (Id NSArray)
binaryArchives mtlStitchedLibraryDescriptor =
  sendMessage mtlStitchedLibraryDescriptor binaryArchivesSelector

-- | binaryArchives
--
-- The array of archives to be searched.
--
-- Binary archives to be searched for precompiled stitched libraries during the compilation of this library.
--
-- ObjC selector: @- setBinaryArchives:@
setBinaryArchives :: (IsMTLStitchedLibraryDescriptor mtlStitchedLibraryDescriptor, IsNSArray value) => mtlStitchedLibraryDescriptor -> value -> IO ()
setBinaryArchives mtlStitchedLibraryDescriptor value =
  sendMessage mtlStitchedLibraryDescriptor setBinaryArchivesSelector (toNSArray value)

-- | options
--
-- The options to use for this new MTLLibrary.
--
-- ObjC selector: @- options@
options :: IsMTLStitchedLibraryDescriptor mtlStitchedLibraryDescriptor => mtlStitchedLibraryDescriptor -> IO MTLStitchedLibraryOptions
options mtlStitchedLibraryDescriptor =
  sendMessage mtlStitchedLibraryDescriptor optionsSelector

-- | options
--
-- The options to use for this new MTLLibrary.
--
-- ObjC selector: @- setOptions:@
setOptions :: IsMTLStitchedLibraryDescriptor mtlStitchedLibraryDescriptor => mtlStitchedLibraryDescriptor -> MTLStitchedLibraryOptions -> IO ()
setOptions mtlStitchedLibraryDescriptor value =
  sendMessage mtlStitchedLibraryDescriptor setOptionsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @functionGraphs@
functionGraphsSelector :: Selector '[] (Id NSArray)
functionGraphsSelector = mkSelector "functionGraphs"

-- | @Selector@ for @setFunctionGraphs:@
setFunctionGraphsSelector :: Selector '[Id NSArray] ()
setFunctionGraphsSelector = mkSelector "setFunctionGraphs:"

-- | @Selector@ for @functions@
functionsSelector :: Selector '[] (Id NSArray)
functionsSelector = mkSelector "functions"

-- | @Selector@ for @setFunctions:@
setFunctionsSelector :: Selector '[Id NSArray] ()
setFunctionsSelector = mkSelector "setFunctions:"

-- | @Selector@ for @binaryArchives@
binaryArchivesSelector :: Selector '[] (Id NSArray)
binaryArchivesSelector = mkSelector "binaryArchives"

-- | @Selector@ for @setBinaryArchives:@
setBinaryArchivesSelector :: Selector '[Id NSArray] ()
setBinaryArchivesSelector = mkSelector "setBinaryArchives:"

-- | @Selector@ for @options@
optionsSelector :: Selector '[] MTLStitchedLibraryOptions
optionsSelector = mkSelector "options"

-- | @Selector@ for @setOptions:@
setOptionsSelector :: Selector '[MTLStitchedLibraryOptions] ()
setOptionsSelector = mkSelector "setOptions:"

