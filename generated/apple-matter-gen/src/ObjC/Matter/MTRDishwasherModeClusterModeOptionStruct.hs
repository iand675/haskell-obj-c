{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDishwasherModeClusterModeOptionStruct@.
module ObjC.Matter.MTRDishwasherModeClusterModeOptionStruct
  ( MTRDishwasherModeClusterModeOptionStruct
  , IsMTRDishwasherModeClusterModeOptionStruct(..)
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
label :: IsMTRDishwasherModeClusterModeOptionStruct mtrDishwasherModeClusterModeOptionStruct => mtrDishwasherModeClusterModeOptionStruct -> IO (Id NSString)
label mtrDishwasherModeClusterModeOptionStruct =
  sendMessage mtrDishwasherModeClusterModeOptionStruct labelSelector

-- | @- setLabel:@
setLabel :: (IsMTRDishwasherModeClusterModeOptionStruct mtrDishwasherModeClusterModeOptionStruct, IsNSString value) => mtrDishwasherModeClusterModeOptionStruct -> value -> IO ()
setLabel mtrDishwasherModeClusterModeOptionStruct value =
  sendMessage mtrDishwasherModeClusterModeOptionStruct setLabelSelector (toNSString value)

-- | @- mode@
mode :: IsMTRDishwasherModeClusterModeOptionStruct mtrDishwasherModeClusterModeOptionStruct => mtrDishwasherModeClusterModeOptionStruct -> IO (Id NSNumber)
mode mtrDishwasherModeClusterModeOptionStruct =
  sendMessage mtrDishwasherModeClusterModeOptionStruct modeSelector

-- | @- setMode:@
setMode :: (IsMTRDishwasherModeClusterModeOptionStruct mtrDishwasherModeClusterModeOptionStruct, IsNSNumber value) => mtrDishwasherModeClusterModeOptionStruct -> value -> IO ()
setMode mtrDishwasherModeClusterModeOptionStruct value =
  sendMessage mtrDishwasherModeClusterModeOptionStruct setModeSelector (toNSNumber value)

-- | @- modeTags@
modeTags :: IsMTRDishwasherModeClusterModeOptionStruct mtrDishwasherModeClusterModeOptionStruct => mtrDishwasherModeClusterModeOptionStruct -> IO (Id NSArray)
modeTags mtrDishwasherModeClusterModeOptionStruct =
  sendMessage mtrDishwasherModeClusterModeOptionStruct modeTagsSelector

-- | @- setModeTags:@
setModeTags :: (IsMTRDishwasherModeClusterModeOptionStruct mtrDishwasherModeClusterModeOptionStruct, IsNSArray value) => mtrDishwasherModeClusterModeOptionStruct -> value -> IO ()
setModeTags mtrDishwasherModeClusterModeOptionStruct value =
  sendMessage mtrDishwasherModeClusterModeOptionStruct setModeTagsSelector (toNSArray value)

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

