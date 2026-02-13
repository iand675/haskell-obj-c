{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRRVCCleanModeClusterModeOptionStruct@.
module ObjC.Matter.MTRRVCCleanModeClusterModeOptionStruct
  ( MTRRVCCleanModeClusterModeOptionStruct
  , IsMTRRVCCleanModeClusterModeOptionStruct(..)
  , label
  , setLabel
  , mode
  , setMode
  , modeTags
  , setModeTags
  , labelSelector
  , modeSelector
  , modeTagsSelector
  , setLabelSelector
  , setModeSelector
  , setModeTagsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- label@
label :: IsMTRRVCCleanModeClusterModeOptionStruct mtrrvcCleanModeClusterModeOptionStruct => mtrrvcCleanModeClusterModeOptionStruct -> IO (Id NSString)
label mtrrvcCleanModeClusterModeOptionStruct =
  sendMessage mtrrvcCleanModeClusterModeOptionStruct labelSelector

-- | @- setLabel:@
setLabel :: (IsMTRRVCCleanModeClusterModeOptionStruct mtrrvcCleanModeClusterModeOptionStruct, IsNSString value) => mtrrvcCleanModeClusterModeOptionStruct -> value -> IO ()
setLabel mtrrvcCleanModeClusterModeOptionStruct value =
  sendMessage mtrrvcCleanModeClusterModeOptionStruct setLabelSelector (toNSString value)

-- | @- mode@
mode :: IsMTRRVCCleanModeClusterModeOptionStruct mtrrvcCleanModeClusterModeOptionStruct => mtrrvcCleanModeClusterModeOptionStruct -> IO (Id NSNumber)
mode mtrrvcCleanModeClusterModeOptionStruct =
  sendMessage mtrrvcCleanModeClusterModeOptionStruct modeSelector

-- | @- setMode:@
setMode :: (IsMTRRVCCleanModeClusterModeOptionStruct mtrrvcCleanModeClusterModeOptionStruct, IsNSNumber value) => mtrrvcCleanModeClusterModeOptionStruct -> value -> IO ()
setMode mtrrvcCleanModeClusterModeOptionStruct value =
  sendMessage mtrrvcCleanModeClusterModeOptionStruct setModeSelector (toNSNumber value)

-- | @- modeTags@
modeTags :: IsMTRRVCCleanModeClusterModeOptionStruct mtrrvcCleanModeClusterModeOptionStruct => mtrrvcCleanModeClusterModeOptionStruct -> IO (Id NSArray)
modeTags mtrrvcCleanModeClusterModeOptionStruct =
  sendMessage mtrrvcCleanModeClusterModeOptionStruct modeTagsSelector

-- | @- setModeTags:@
setModeTags :: (IsMTRRVCCleanModeClusterModeOptionStruct mtrrvcCleanModeClusterModeOptionStruct, IsNSArray value) => mtrrvcCleanModeClusterModeOptionStruct -> value -> IO ()
setModeTags mtrrvcCleanModeClusterModeOptionStruct value =
  sendMessage mtrrvcCleanModeClusterModeOptionStruct setModeTagsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @mode@
modeSelector :: Selector '[] (Id NSNumber)
modeSelector = mkSelector "mode"

-- | @Selector@ for @setMode:@
setModeSelector :: Selector '[Id NSNumber] ()
setModeSelector = mkSelector "setMode:"

-- | @Selector@ for @modeTags@
modeTagsSelector :: Selector '[] (Id NSArray)
modeTagsSelector = mkSelector "modeTags"

-- | @Selector@ for @setModeTags:@
setModeTagsSelector :: Selector '[Id NSArray] ()
setModeTagsSelector = mkSelector "setModeTags:"

