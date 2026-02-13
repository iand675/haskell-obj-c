{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMicrowaveOvenModeClusterModeOptionStruct@.
module ObjC.Matter.MTRMicrowaveOvenModeClusterModeOptionStruct
  ( MTRMicrowaveOvenModeClusterModeOptionStruct
  , IsMTRMicrowaveOvenModeClusterModeOptionStruct(..)
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
label :: IsMTRMicrowaveOvenModeClusterModeOptionStruct mtrMicrowaveOvenModeClusterModeOptionStruct => mtrMicrowaveOvenModeClusterModeOptionStruct -> IO (Id NSString)
label mtrMicrowaveOvenModeClusterModeOptionStruct =
  sendMessage mtrMicrowaveOvenModeClusterModeOptionStruct labelSelector

-- | @- setLabel:@
setLabel :: (IsMTRMicrowaveOvenModeClusterModeOptionStruct mtrMicrowaveOvenModeClusterModeOptionStruct, IsNSString value) => mtrMicrowaveOvenModeClusterModeOptionStruct -> value -> IO ()
setLabel mtrMicrowaveOvenModeClusterModeOptionStruct value =
  sendMessage mtrMicrowaveOvenModeClusterModeOptionStruct setLabelSelector (toNSString value)

-- | @- mode@
mode :: IsMTRMicrowaveOvenModeClusterModeOptionStruct mtrMicrowaveOvenModeClusterModeOptionStruct => mtrMicrowaveOvenModeClusterModeOptionStruct -> IO (Id NSNumber)
mode mtrMicrowaveOvenModeClusterModeOptionStruct =
  sendMessage mtrMicrowaveOvenModeClusterModeOptionStruct modeSelector

-- | @- setMode:@
setMode :: (IsMTRMicrowaveOvenModeClusterModeOptionStruct mtrMicrowaveOvenModeClusterModeOptionStruct, IsNSNumber value) => mtrMicrowaveOvenModeClusterModeOptionStruct -> value -> IO ()
setMode mtrMicrowaveOvenModeClusterModeOptionStruct value =
  sendMessage mtrMicrowaveOvenModeClusterModeOptionStruct setModeSelector (toNSNumber value)

-- | @- modeTags@
modeTags :: IsMTRMicrowaveOvenModeClusterModeOptionStruct mtrMicrowaveOvenModeClusterModeOptionStruct => mtrMicrowaveOvenModeClusterModeOptionStruct -> IO (Id NSArray)
modeTags mtrMicrowaveOvenModeClusterModeOptionStruct =
  sendMessage mtrMicrowaveOvenModeClusterModeOptionStruct modeTagsSelector

-- | @- setModeTags:@
setModeTags :: (IsMTRMicrowaveOvenModeClusterModeOptionStruct mtrMicrowaveOvenModeClusterModeOptionStruct, IsNSArray value) => mtrMicrowaveOvenModeClusterModeOptionStruct -> value -> IO ()
setModeTags mtrMicrowaveOvenModeClusterModeOptionStruct value =
  sendMessage mtrMicrowaveOvenModeClusterModeOptionStruct setModeTagsSelector (toNSArray value)

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

