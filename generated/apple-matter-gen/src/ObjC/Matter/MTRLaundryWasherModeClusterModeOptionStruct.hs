{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRLaundryWasherModeClusterModeOptionStruct@.
module ObjC.Matter.MTRLaundryWasherModeClusterModeOptionStruct
  ( MTRLaundryWasherModeClusterModeOptionStruct
  , IsMTRLaundryWasherModeClusterModeOptionStruct(..)
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
label :: IsMTRLaundryWasherModeClusterModeOptionStruct mtrLaundryWasherModeClusterModeOptionStruct => mtrLaundryWasherModeClusterModeOptionStruct -> IO (Id NSString)
label mtrLaundryWasherModeClusterModeOptionStruct =
  sendMessage mtrLaundryWasherModeClusterModeOptionStruct labelSelector

-- | @- setLabel:@
setLabel :: (IsMTRLaundryWasherModeClusterModeOptionStruct mtrLaundryWasherModeClusterModeOptionStruct, IsNSString value) => mtrLaundryWasherModeClusterModeOptionStruct -> value -> IO ()
setLabel mtrLaundryWasherModeClusterModeOptionStruct value =
  sendMessage mtrLaundryWasherModeClusterModeOptionStruct setLabelSelector (toNSString value)

-- | @- mode@
mode :: IsMTRLaundryWasherModeClusterModeOptionStruct mtrLaundryWasherModeClusterModeOptionStruct => mtrLaundryWasherModeClusterModeOptionStruct -> IO (Id NSNumber)
mode mtrLaundryWasherModeClusterModeOptionStruct =
  sendMessage mtrLaundryWasherModeClusterModeOptionStruct modeSelector

-- | @- setMode:@
setMode :: (IsMTRLaundryWasherModeClusterModeOptionStruct mtrLaundryWasherModeClusterModeOptionStruct, IsNSNumber value) => mtrLaundryWasherModeClusterModeOptionStruct -> value -> IO ()
setMode mtrLaundryWasherModeClusterModeOptionStruct value =
  sendMessage mtrLaundryWasherModeClusterModeOptionStruct setModeSelector (toNSNumber value)

-- | @- modeTags@
modeTags :: IsMTRLaundryWasherModeClusterModeOptionStruct mtrLaundryWasherModeClusterModeOptionStruct => mtrLaundryWasherModeClusterModeOptionStruct -> IO (Id NSArray)
modeTags mtrLaundryWasherModeClusterModeOptionStruct =
  sendMessage mtrLaundryWasherModeClusterModeOptionStruct modeTagsSelector

-- | @- setModeTags:@
setModeTags :: (IsMTRLaundryWasherModeClusterModeOptionStruct mtrLaundryWasherModeClusterModeOptionStruct, IsNSArray value) => mtrLaundryWasherModeClusterModeOptionStruct -> value -> IO ()
setModeTags mtrLaundryWasherModeClusterModeOptionStruct value =
  sendMessage mtrLaundryWasherModeClusterModeOptionStruct setModeTagsSelector (toNSArray value)

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

