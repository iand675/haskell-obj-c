{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GKTurnBasedEventHandler@.
module ObjC.GameKit.GKTurnBasedEventHandler
  ( GKTurnBasedEventHandler
  , IsGKTurnBasedEventHandler(..)
  , sharedTurnBasedEventHandler
  , delegate
  , setDelegate
  , sharedTurnBasedEventHandlerSelector
  , delegateSelector
  , setDelegateSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ sharedTurnBasedEventHandler@
sharedTurnBasedEventHandler :: IO (Id GKTurnBasedEventHandler)
sharedTurnBasedEventHandler  =
  do
    cls' <- getRequiredClass "GKTurnBasedEventHandler"
    sendClassMsg cls' (mkSelector "sharedTurnBasedEventHandler") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- delegate@
delegate :: IsGKTurnBasedEventHandler gkTurnBasedEventHandler => gkTurnBasedEventHandler -> IO (Id NSObject)
delegate gkTurnBasedEventHandler  =
    sendMsg gkTurnBasedEventHandler (mkSelector "delegate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDelegate:@
setDelegate :: (IsGKTurnBasedEventHandler gkTurnBasedEventHandler, IsNSObject value) => gkTurnBasedEventHandler -> value -> IO ()
setDelegate gkTurnBasedEventHandler  value =
  withObjCPtr value $ \raw_value ->
      sendMsg gkTurnBasedEventHandler (mkSelector "setDelegate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedTurnBasedEventHandler@
sharedTurnBasedEventHandlerSelector :: Selector
sharedTurnBasedEventHandlerSelector = mkSelector "sharedTurnBasedEventHandler"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

