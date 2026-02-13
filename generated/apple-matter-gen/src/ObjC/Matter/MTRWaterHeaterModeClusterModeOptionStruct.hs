{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWaterHeaterModeClusterModeOptionStruct@.
module ObjC.Matter.MTRWaterHeaterModeClusterModeOptionStruct
  ( MTRWaterHeaterModeClusterModeOptionStruct
  , IsMTRWaterHeaterModeClusterModeOptionStruct(..)
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
label :: IsMTRWaterHeaterModeClusterModeOptionStruct mtrWaterHeaterModeClusterModeOptionStruct => mtrWaterHeaterModeClusterModeOptionStruct -> IO (Id NSString)
label mtrWaterHeaterModeClusterModeOptionStruct =
  sendMessage mtrWaterHeaterModeClusterModeOptionStruct labelSelector

-- | @- setLabel:@
setLabel :: (IsMTRWaterHeaterModeClusterModeOptionStruct mtrWaterHeaterModeClusterModeOptionStruct, IsNSString value) => mtrWaterHeaterModeClusterModeOptionStruct -> value -> IO ()
setLabel mtrWaterHeaterModeClusterModeOptionStruct value =
  sendMessage mtrWaterHeaterModeClusterModeOptionStruct setLabelSelector (toNSString value)

-- | @- mode@
mode :: IsMTRWaterHeaterModeClusterModeOptionStruct mtrWaterHeaterModeClusterModeOptionStruct => mtrWaterHeaterModeClusterModeOptionStruct -> IO (Id NSNumber)
mode mtrWaterHeaterModeClusterModeOptionStruct =
  sendMessage mtrWaterHeaterModeClusterModeOptionStruct modeSelector

-- | @- setMode:@
setMode :: (IsMTRWaterHeaterModeClusterModeOptionStruct mtrWaterHeaterModeClusterModeOptionStruct, IsNSNumber value) => mtrWaterHeaterModeClusterModeOptionStruct -> value -> IO ()
setMode mtrWaterHeaterModeClusterModeOptionStruct value =
  sendMessage mtrWaterHeaterModeClusterModeOptionStruct setModeSelector (toNSNumber value)

-- | @- modeTags@
modeTags :: IsMTRWaterHeaterModeClusterModeOptionStruct mtrWaterHeaterModeClusterModeOptionStruct => mtrWaterHeaterModeClusterModeOptionStruct -> IO (Id NSArray)
modeTags mtrWaterHeaterModeClusterModeOptionStruct =
  sendMessage mtrWaterHeaterModeClusterModeOptionStruct modeTagsSelector

-- | @- setModeTags:@
setModeTags :: (IsMTRWaterHeaterModeClusterModeOptionStruct mtrWaterHeaterModeClusterModeOptionStruct, IsNSArray value) => mtrWaterHeaterModeClusterModeOptionStruct -> value -> IO ()
setModeTags mtrWaterHeaterModeClusterModeOptionStruct value =
  sendMessage mtrWaterHeaterModeClusterModeOptionStruct setModeTagsSelector (toNSArray value)

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

