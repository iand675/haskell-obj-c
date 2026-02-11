{-# LANGUAGE PatternSynonyms #-}
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
  , functionGraphsSelector
  , setFunctionGraphsSelector
  , functionsSelector
  , setFunctionsSelector
  , binaryArchivesSelector
  , setBinaryArchivesSelector
  , optionsSelector
  , setOptionsSelector

  -- * Enum types
  , MTLStitchedLibraryOptions(MTLStitchedLibraryOptions)
  , pattern MTLStitchedLibraryOptionNone
  , pattern MTLStitchedLibraryOptionFailOnBinaryArchiveMiss
  , pattern MTLStitchedLibraryOptionStoreLibraryInMetalPipelinesScript

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

-- | @- functionGraphs@
functionGraphs :: IsMTLStitchedLibraryDescriptor mtlStitchedLibraryDescriptor => mtlStitchedLibraryDescriptor -> IO (Id NSArray)
functionGraphs mtlStitchedLibraryDescriptor  =
    sendMsg mtlStitchedLibraryDescriptor (mkSelector "functionGraphs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFunctionGraphs:@
setFunctionGraphs :: (IsMTLStitchedLibraryDescriptor mtlStitchedLibraryDescriptor, IsNSArray value) => mtlStitchedLibraryDescriptor -> value -> IO ()
setFunctionGraphs mtlStitchedLibraryDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtlStitchedLibraryDescriptor (mkSelector "setFunctionGraphs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- functions@
functions :: IsMTLStitchedLibraryDescriptor mtlStitchedLibraryDescriptor => mtlStitchedLibraryDescriptor -> IO (Id NSArray)
functions mtlStitchedLibraryDescriptor  =
    sendMsg mtlStitchedLibraryDescriptor (mkSelector "functions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFunctions:@
setFunctions :: (IsMTLStitchedLibraryDescriptor mtlStitchedLibraryDescriptor, IsNSArray value) => mtlStitchedLibraryDescriptor -> value -> IO ()
setFunctions mtlStitchedLibraryDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtlStitchedLibraryDescriptor (mkSelector "setFunctions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | binaryArchives
--
-- The array of archives to be searched.
--
-- Binary archives to be searched for precompiled stitched libraries during the compilation of this library.
--
-- ObjC selector: @- binaryArchives@
binaryArchives :: IsMTLStitchedLibraryDescriptor mtlStitchedLibraryDescriptor => mtlStitchedLibraryDescriptor -> IO (Id NSArray)
binaryArchives mtlStitchedLibraryDescriptor  =
    sendMsg mtlStitchedLibraryDescriptor (mkSelector "binaryArchives") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | binaryArchives
--
-- The array of archives to be searched.
--
-- Binary archives to be searched for precompiled stitched libraries during the compilation of this library.
--
-- ObjC selector: @- setBinaryArchives:@
setBinaryArchives :: (IsMTLStitchedLibraryDescriptor mtlStitchedLibraryDescriptor, IsNSArray value) => mtlStitchedLibraryDescriptor -> value -> IO ()
setBinaryArchives mtlStitchedLibraryDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtlStitchedLibraryDescriptor (mkSelector "setBinaryArchives:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | options
--
-- The options to use for this new MTLLibrary.
--
-- ObjC selector: @- options@
options :: IsMTLStitchedLibraryDescriptor mtlStitchedLibraryDescriptor => mtlStitchedLibraryDescriptor -> IO MTLStitchedLibraryOptions
options mtlStitchedLibraryDescriptor  =
    fmap (coerce :: CULong -> MTLStitchedLibraryOptions) $ sendMsg mtlStitchedLibraryDescriptor (mkSelector "options") retCULong []

-- | options
--
-- The options to use for this new MTLLibrary.
--
-- ObjC selector: @- setOptions:@
setOptions :: IsMTLStitchedLibraryDescriptor mtlStitchedLibraryDescriptor => mtlStitchedLibraryDescriptor -> MTLStitchedLibraryOptions -> IO ()
setOptions mtlStitchedLibraryDescriptor  value =
    sendMsg mtlStitchedLibraryDescriptor (mkSelector "setOptions:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @functionGraphs@
functionGraphsSelector :: Selector
functionGraphsSelector = mkSelector "functionGraphs"

-- | @Selector@ for @setFunctionGraphs:@
setFunctionGraphsSelector :: Selector
setFunctionGraphsSelector = mkSelector "setFunctionGraphs:"

-- | @Selector@ for @functions@
functionsSelector :: Selector
functionsSelector = mkSelector "functions"

-- | @Selector@ for @setFunctions:@
setFunctionsSelector :: Selector
setFunctionsSelector = mkSelector "setFunctions:"

-- | @Selector@ for @binaryArchives@
binaryArchivesSelector :: Selector
binaryArchivesSelector = mkSelector "binaryArchives"

-- | @Selector@ for @setBinaryArchives:@
setBinaryArchivesSelector :: Selector
setBinaryArchivesSelector = mkSelector "setBinaryArchives:"

-- | @Selector@ for @options@
optionsSelector :: Selector
optionsSelector = mkSelector "options"

-- | @Selector@ for @setOptions:@
setOptionsSelector :: Selector
setOptionsSelector = mkSelector "setOptions:"

