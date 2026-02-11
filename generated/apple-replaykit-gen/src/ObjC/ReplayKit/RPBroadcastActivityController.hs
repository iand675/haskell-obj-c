{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | RPBroadcastActivityController
--
-- Controller object that allows clients to present the macOS broadcast picker and returns the RPBroadcastController object that controls broadcast functionality.
--
-- Generated bindings for @RPBroadcastActivityController@.
module ObjC.ReplayKit.RPBroadcastActivityController
  ( RPBroadcastActivityController
  , IsRPBroadcastActivityController(..)
  , delegate
  , setDelegate
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

import ObjC.ReplayKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Delegate that is notified when the activity view controller is complete.
--
-- ObjC selector: @- delegate@
delegate :: IsRPBroadcastActivityController rpBroadcastActivityController => rpBroadcastActivityController -> IO RawId
delegate rpBroadcastActivityController  =
    fmap (RawId . castPtr) $ sendMsg rpBroadcastActivityController (mkSelector "delegate") (retPtr retVoid) []

-- | Delegate that is notified when the activity view controller is complete.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsRPBroadcastActivityController rpBroadcastActivityController => rpBroadcastActivityController -> RawId -> IO ()
setDelegate rpBroadcastActivityController  value =
    sendMsg rpBroadcastActivityController (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

