{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROvenModeClusterModeOptionStruct@.
module ObjC.Matter.MTROvenModeClusterModeOptionStruct
  ( MTROvenModeClusterModeOptionStruct
  , IsMTROvenModeClusterModeOptionStruct(..)
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
label :: IsMTROvenModeClusterModeOptionStruct mtrOvenModeClusterModeOptionStruct => mtrOvenModeClusterModeOptionStruct -> IO (Id NSString)
label mtrOvenModeClusterModeOptionStruct =
  sendMessage mtrOvenModeClusterModeOptionStruct labelSelector

-- | @- setLabel:@
setLabel :: (IsMTROvenModeClusterModeOptionStruct mtrOvenModeClusterModeOptionStruct, IsNSString value) => mtrOvenModeClusterModeOptionStruct -> value -> IO ()
setLabel mtrOvenModeClusterModeOptionStruct value =
  sendMessage mtrOvenModeClusterModeOptionStruct setLabelSelector (toNSString value)

-- | @- mode@
mode :: IsMTROvenModeClusterModeOptionStruct mtrOvenModeClusterModeOptionStruct => mtrOvenModeClusterModeOptionStruct -> IO (Id NSNumber)
mode mtrOvenModeClusterModeOptionStruct =
  sendMessage mtrOvenModeClusterModeOptionStruct modeSelector

-- | @- setMode:@
setMode :: (IsMTROvenModeClusterModeOptionStruct mtrOvenModeClusterModeOptionStruct, IsNSNumber value) => mtrOvenModeClusterModeOptionStruct -> value -> IO ()
setMode mtrOvenModeClusterModeOptionStruct value =
  sendMessage mtrOvenModeClusterModeOptionStruct setModeSelector (toNSNumber value)

-- | @- modeTags@
modeTags :: IsMTROvenModeClusterModeOptionStruct mtrOvenModeClusterModeOptionStruct => mtrOvenModeClusterModeOptionStruct -> IO (Id NSArray)
modeTags mtrOvenModeClusterModeOptionStruct =
  sendMessage mtrOvenModeClusterModeOptionStruct modeTagsSelector

-- | @- setModeTags:@
setModeTags :: (IsMTROvenModeClusterModeOptionStruct mtrOvenModeClusterModeOptionStruct, IsNSArray value) => mtrOvenModeClusterModeOptionStruct -> value -> IO ()
setModeTags mtrOvenModeClusterModeOptionStruct value =
  sendMessage mtrOvenModeClusterModeOptionStruct setModeTagsSelector (toNSArray value)

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

