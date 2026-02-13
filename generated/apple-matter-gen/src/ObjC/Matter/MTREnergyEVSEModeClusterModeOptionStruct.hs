{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREnergyEVSEModeClusterModeOptionStruct@.
module ObjC.Matter.MTREnergyEVSEModeClusterModeOptionStruct
  ( MTREnergyEVSEModeClusterModeOptionStruct
  , IsMTREnergyEVSEModeClusterModeOptionStruct(..)
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
label :: IsMTREnergyEVSEModeClusterModeOptionStruct mtrEnergyEVSEModeClusterModeOptionStruct => mtrEnergyEVSEModeClusterModeOptionStruct -> IO (Id NSString)
label mtrEnergyEVSEModeClusterModeOptionStruct =
  sendMessage mtrEnergyEVSEModeClusterModeOptionStruct labelSelector

-- | @- setLabel:@
setLabel :: (IsMTREnergyEVSEModeClusterModeOptionStruct mtrEnergyEVSEModeClusterModeOptionStruct, IsNSString value) => mtrEnergyEVSEModeClusterModeOptionStruct -> value -> IO ()
setLabel mtrEnergyEVSEModeClusterModeOptionStruct value =
  sendMessage mtrEnergyEVSEModeClusterModeOptionStruct setLabelSelector (toNSString value)

-- | @- mode@
mode :: IsMTREnergyEVSEModeClusterModeOptionStruct mtrEnergyEVSEModeClusterModeOptionStruct => mtrEnergyEVSEModeClusterModeOptionStruct -> IO (Id NSNumber)
mode mtrEnergyEVSEModeClusterModeOptionStruct =
  sendMessage mtrEnergyEVSEModeClusterModeOptionStruct modeSelector

-- | @- setMode:@
setMode :: (IsMTREnergyEVSEModeClusterModeOptionStruct mtrEnergyEVSEModeClusterModeOptionStruct, IsNSNumber value) => mtrEnergyEVSEModeClusterModeOptionStruct -> value -> IO ()
setMode mtrEnergyEVSEModeClusterModeOptionStruct value =
  sendMessage mtrEnergyEVSEModeClusterModeOptionStruct setModeSelector (toNSNumber value)

-- | @- modeTags@
modeTags :: IsMTREnergyEVSEModeClusterModeOptionStruct mtrEnergyEVSEModeClusterModeOptionStruct => mtrEnergyEVSEModeClusterModeOptionStruct -> IO (Id NSArray)
modeTags mtrEnergyEVSEModeClusterModeOptionStruct =
  sendMessage mtrEnergyEVSEModeClusterModeOptionStruct modeTagsSelector

-- | @- setModeTags:@
setModeTags :: (IsMTREnergyEVSEModeClusterModeOptionStruct mtrEnergyEVSEModeClusterModeOptionStruct, IsNSArray value) => mtrEnergyEVSEModeClusterModeOptionStruct -> value -> IO ()
setModeTags mtrEnergyEVSEModeClusterModeOptionStruct value =
  sendMessage mtrEnergyEVSEModeClusterModeOptionStruct setModeTagsSelector (toNSArray value)

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

