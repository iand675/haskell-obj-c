{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRRVCRunModeClusterModeOptionStruct@.
module ObjC.Matter.MTRRVCRunModeClusterModeOptionStruct
  ( MTRRVCRunModeClusterModeOptionStruct
  , IsMTRRVCRunModeClusterModeOptionStruct(..)
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
label :: IsMTRRVCRunModeClusterModeOptionStruct mtrrvcRunModeClusterModeOptionStruct => mtrrvcRunModeClusterModeOptionStruct -> IO (Id NSString)
label mtrrvcRunModeClusterModeOptionStruct =
  sendMessage mtrrvcRunModeClusterModeOptionStruct labelSelector

-- | @- setLabel:@
setLabel :: (IsMTRRVCRunModeClusterModeOptionStruct mtrrvcRunModeClusterModeOptionStruct, IsNSString value) => mtrrvcRunModeClusterModeOptionStruct -> value -> IO ()
setLabel mtrrvcRunModeClusterModeOptionStruct value =
  sendMessage mtrrvcRunModeClusterModeOptionStruct setLabelSelector (toNSString value)

-- | @- mode@
mode :: IsMTRRVCRunModeClusterModeOptionStruct mtrrvcRunModeClusterModeOptionStruct => mtrrvcRunModeClusterModeOptionStruct -> IO (Id NSNumber)
mode mtrrvcRunModeClusterModeOptionStruct =
  sendMessage mtrrvcRunModeClusterModeOptionStruct modeSelector

-- | @- setMode:@
setMode :: (IsMTRRVCRunModeClusterModeOptionStruct mtrrvcRunModeClusterModeOptionStruct, IsNSNumber value) => mtrrvcRunModeClusterModeOptionStruct -> value -> IO ()
setMode mtrrvcRunModeClusterModeOptionStruct value =
  sendMessage mtrrvcRunModeClusterModeOptionStruct setModeSelector (toNSNumber value)

-- | @- modeTags@
modeTags :: IsMTRRVCRunModeClusterModeOptionStruct mtrrvcRunModeClusterModeOptionStruct => mtrrvcRunModeClusterModeOptionStruct -> IO (Id NSArray)
modeTags mtrrvcRunModeClusterModeOptionStruct =
  sendMessage mtrrvcRunModeClusterModeOptionStruct modeTagsSelector

-- | @- setModeTags:@
setModeTags :: (IsMTRRVCRunModeClusterModeOptionStruct mtrrvcRunModeClusterModeOptionStruct, IsNSArray value) => mtrrvcRunModeClusterModeOptionStruct -> value -> IO ()
setModeTags mtrrvcRunModeClusterModeOptionStruct value =
  sendMessage mtrrvcRunModeClusterModeOptionStruct setModeTagsSelector (toNSArray value)

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

