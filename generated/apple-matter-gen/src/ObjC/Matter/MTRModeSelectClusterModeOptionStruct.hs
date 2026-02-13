{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRModeSelectClusterModeOptionStruct@.
module ObjC.Matter.MTRModeSelectClusterModeOptionStruct
  ( MTRModeSelectClusterModeOptionStruct
  , IsMTRModeSelectClusterModeOptionStruct(..)
  , label
  , setLabel
  , mode
  , setMode
  , semanticTags
  , setSemanticTags
  , labelSelector
  , modeSelector
  , semanticTagsSelector
  , setLabelSelector
  , setModeSelector
  , setSemanticTagsSelector


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
label :: IsMTRModeSelectClusterModeOptionStruct mtrModeSelectClusterModeOptionStruct => mtrModeSelectClusterModeOptionStruct -> IO (Id NSString)
label mtrModeSelectClusterModeOptionStruct =
  sendMessage mtrModeSelectClusterModeOptionStruct labelSelector

-- | @- setLabel:@
setLabel :: (IsMTRModeSelectClusterModeOptionStruct mtrModeSelectClusterModeOptionStruct, IsNSString value) => mtrModeSelectClusterModeOptionStruct -> value -> IO ()
setLabel mtrModeSelectClusterModeOptionStruct value =
  sendMessage mtrModeSelectClusterModeOptionStruct setLabelSelector (toNSString value)

-- | @- mode@
mode :: IsMTRModeSelectClusterModeOptionStruct mtrModeSelectClusterModeOptionStruct => mtrModeSelectClusterModeOptionStruct -> IO (Id NSNumber)
mode mtrModeSelectClusterModeOptionStruct =
  sendMessage mtrModeSelectClusterModeOptionStruct modeSelector

-- | @- setMode:@
setMode :: (IsMTRModeSelectClusterModeOptionStruct mtrModeSelectClusterModeOptionStruct, IsNSNumber value) => mtrModeSelectClusterModeOptionStruct -> value -> IO ()
setMode mtrModeSelectClusterModeOptionStruct value =
  sendMessage mtrModeSelectClusterModeOptionStruct setModeSelector (toNSNumber value)

-- | @- semanticTags@
semanticTags :: IsMTRModeSelectClusterModeOptionStruct mtrModeSelectClusterModeOptionStruct => mtrModeSelectClusterModeOptionStruct -> IO (Id NSArray)
semanticTags mtrModeSelectClusterModeOptionStruct =
  sendMessage mtrModeSelectClusterModeOptionStruct semanticTagsSelector

-- | @- setSemanticTags:@
setSemanticTags :: (IsMTRModeSelectClusterModeOptionStruct mtrModeSelectClusterModeOptionStruct, IsNSArray value) => mtrModeSelectClusterModeOptionStruct -> value -> IO ()
setSemanticTags mtrModeSelectClusterModeOptionStruct value =
  sendMessage mtrModeSelectClusterModeOptionStruct setSemanticTagsSelector (toNSArray value)

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

-- | @Selector@ for @semanticTags@
semanticTagsSelector :: Selector '[] (Id NSArray)
semanticTagsSelector = mkSelector "semanticTags"

-- | @Selector@ for @setSemanticTags:@
setSemanticTagsSelector :: Selector '[Id NSArray] ()
setSemanticTagsSelector = mkSelector "setSemanticTags:"

