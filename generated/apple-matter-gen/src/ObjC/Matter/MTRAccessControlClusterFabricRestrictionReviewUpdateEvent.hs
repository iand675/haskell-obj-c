{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAccessControlClusterFabricRestrictionReviewUpdateEvent@.
module ObjC.Matter.MTRAccessControlClusterFabricRestrictionReviewUpdateEvent
  ( MTRAccessControlClusterFabricRestrictionReviewUpdateEvent
  , IsMTRAccessControlClusterFabricRestrictionReviewUpdateEvent(..)
  , token
  , setToken
  , instruction
  , setInstruction
  , arlRequestFlowUrl
  , setArlRequestFlowUrl
  , fabricIndex
  , setFabricIndex
  , arlRequestFlowUrlSelector
  , fabricIndexSelector
  , instructionSelector
  , setArlRequestFlowUrlSelector
  , setFabricIndexSelector
  , setInstructionSelector
  , setTokenSelector
  , tokenSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- token@
token :: IsMTRAccessControlClusterFabricRestrictionReviewUpdateEvent mtrAccessControlClusterFabricRestrictionReviewUpdateEvent => mtrAccessControlClusterFabricRestrictionReviewUpdateEvent -> IO (Id NSNumber)
token mtrAccessControlClusterFabricRestrictionReviewUpdateEvent =
  sendMessage mtrAccessControlClusterFabricRestrictionReviewUpdateEvent tokenSelector

-- | @- setToken:@
setToken :: (IsMTRAccessControlClusterFabricRestrictionReviewUpdateEvent mtrAccessControlClusterFabricRestrictionReviewUpdateEvent, IsNSNumber value) => mtrAccessControlClusterFabricRestrictionReviewUpdateEvent -> value -> IO ()
setToken mtrAccessControlClusterFabricRestrictionReviewUpdateEvent value =
  sendMessage mtrAccessControlClusterFabricRestrictionReviewUpdateEvent setTokenSelector (toNSNumber value)

-- | @- instruction@
instruction :: IsMTRAccessControlClusterFabricRestrictionReviewUpdateEvent mtrAccessControlClusterFabricRestrictionReviewUpdateEvent => mtrAccessControlClusterFabricRestrictionReviewUpdateEvent -> IO (Id NSString)
instruction mtrAccessControlClusterFabricRestrictionReviewUpdateEvent =
  sendMessage mtrAccessControlClusterFabricRestrictionReviewUpdateEvent instructionSelector

-- | @- setInstruction:@
setInstruction :: (IsMTRAccessControlClusterFabricRestrictionReviewUpdateEvent mtrAccessControlClusterFabricRestrictionReviewUpdateEvent, IsNSString value) => mtrAccessControlClusterFabricRestrictionReviewUpdateEvent -> value -> IO ()
setInstruction mtrAccessControlClusterFabricRestrictionReviewUpdateEvent value =
  sendMessage mtrAccessControlClusterFabricRestrictionReviewUpdateEvent setInstructionSelector (toNSString value)

-- | @- arlRequestFlowUrl@
arlRequestFlowUrl :: IsMTRAccessControlClusterFabricRestrictionReviewUpdateEvent mtrAccessControlClusterFabricRestrictionReviewUpdateEvent => mtrAccessControlClusterFabricRestrictionReviewUpdateEvent -> IO (Id NSString)
arlRequestFlowUrl mtrAccessControlClusterFabricRestrictionReviewUpdateEvent =
  sendMessage mtrAccessControlClusterFabricRestrictionReviewUpdateEvent arlRequestFlowUrlSelector

-- | @- setArlRequestFlowUrl:@
setArlRequestFlowUrl :: (IsMTRAccessControlClusterFabricRestrictionReviewUpdateEvent mtrAccessControlClusterFabricRestrictionReviewUpdateEvent, IsNSString value) => mtrAccessControlClusterFabricRestrictionReviewUpdateEvent -> value -> IO ()
setArlRequestFlowUrl mtrAccessControlClusterFabricRestrictionReviewUpdateEvent value =
  sendMessage mtrAccessControlClusterFabricRestrictionReviewUpdateEvent setArlRequestFlowUrlSelector (toNSString value)

-- | @- fabricIndex@
fabricIndex :: IsMTRAccessControlClusterFabricRestrictionReviewUpdateEvent mtrAccessControlClusterFabricRestrictionReviewUpdateEvent => mtrAccessControlClusterFabricRestrictionReviewUpdateEvent -> IO (Id NSNumber)
fabricIndex mtrAccessControlClusterFabricRestrictionReviewUpdateEvent =
  sendMessage mtrAccessControlClusterFabricRestrictionReviewUpdateEvent fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRAccessControlClusterFabricRestrictionReviewUpdateEvent mtrAccessControlClusterFabricRestrictionReviewUpdateEvent, IsNSNumber value) => mtrAccessControlClusterFabricRestrictionReviewUpdateEvent -> value -> IO ()
setFabricIndex mtrAccessControlClusterFabricRestrictionReviewUpdateEvent value =
  sendMessage mtrAccessControlClusterFabricRestrictionReviewUpdateEvent setFabricIndexSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @token@
tokenSelector :: Selector '[] (Id NSNumber)
tokenSelector = mkSelector "token"

-- | @Selector@ for @setToken:@
setTokenSelector :: Selector '[Id NSNumber] ()
setTokenSelector = mkSelector "setToken:"

-- | @Selector@ for @instruction@
instructionSelector :: Selector '[] (Id NSString)
instructionSelector = mkSelector "instruction"

-- | @Selector@ for @setInstruction:@
setInstructionSelector :: Selector '[Id NSString] ()
setInstructionSelector = mkSelector "setInstruction:"

-- | @Selector@ for @arlRequestFlowUrl@
arlRequestFlowUrlSelector :: Selector '[] (Id NSString)
arlRequestFlowUrlSelector = mkSelector "arlRequestFlowUrl"

-- | @Selector@ for @setArlRequestFlowUrl:@
setArlRequestFlowUrlSelector :: Selector '[Id NSString] ()
setArlRequestFlowUrlSelector = mkSelector "setArlRequestFlowUrl:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

