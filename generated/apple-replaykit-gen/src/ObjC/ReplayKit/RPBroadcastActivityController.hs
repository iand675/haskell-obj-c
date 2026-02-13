{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ReplayKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Delegate that is notified when the activity view controller is complete.
--
-- ObjC selector: @- delegate@
delegate :: IsRPBroadcastActivityController rpBroadcastActivityController => rpBroadcastActivityController -> IO RawId
delegate rpBroadcastActivityController =
  sendMessage rpBroadcastActivityController delegateSelector

-- | Delegate that is notified when the activity view controller is complete.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsRPBroadcastActivityController rpBroadcastActivityController => rpBroadcastActivityController -> RawId -> IO ()
setDelegate rpBroadcastActivityController value =
  sendMessage rpBroadcastActivityController setDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

