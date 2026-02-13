{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CXCallAction@.
module ObjC.CallKit.CXCallAction
  ( CXCallAction
  , IsCXCallAction(..)
  , initWithCallUUID
  , initWithCoder
  , init_
  , callUUID
  , callUUIDSelector
  , initSelector
  , initWithCallUUIDSelector
  , initWithCoderSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CallKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithCallUUID:@
initWithCallUUID :: (IsCXCallAction cxCallAction, IsNSUUID callUUID) => cxCallAction -> callUUID -> IO (Id CXCallAction)
initWithCallUUID cxCallAction callUUID =
  sendOwnedMessage cxCallAction initWithCallUUIDSelector (toNSUUID callUUID)

-- | @- initWithCoder:@
initWithCoder :: (IsCXCallAction cxCallAction, IsNSCoder aDecoder) => cxCallAction -> aDecoder -> IO (Id CXCallAction)
initWithCoder cxCallAction aDecoder =
  sendOwnedMessage cxCallAction initWithCoderSelector (toNSCoder aDecoder)

-- | @- init@
init_ :: IsCXCallAction cxCallAction => cxCallAction -> IO (Id CXCallAction)
init_ cxCallAction =
  sendOwnedMessage cxCallAction initSelector

-- | @- callUUID@
callUUID :: IsCXCallAction cxCallAction => cxCallAction -> IO (Id NSUUID)
callUUID cxCallAction =
  sendMessage cxCallAction callUUIDSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCallUUID:@
initWithCallUUIDSelector :: Selector '[Id NSUUID] (Id CXCallAction)
initWithCallUUIDSelector = mkSelector "initWithCallUUID:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id CXCallAction)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CXCallAction)
initSelector = mkSelector "init"

-- | @Selector@ for @callUUID@
callUUIDSelector :: Selector '[] (Id NSUUID)
callUUIDSelector = mkSelector "callUUID"

