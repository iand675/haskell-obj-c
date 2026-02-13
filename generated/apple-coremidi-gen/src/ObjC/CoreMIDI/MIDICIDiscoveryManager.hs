{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MIDICIDiscoveryManager@.
module ObjC.CoreMIDI.MIDICIDiscoveryManager
  ( MIDICIDiscoveryManager
  , IsMIDICIDiscoveryManager(..)
  , sharedInstance
  , discoverWithHandler
  , discoverWithHandlerSelector
  , sharedInstanceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMIDI.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ sharedInstance@
sharedInstance :: IO (Id MIDICIDiscoveryManager)
sharedInstance  =
  do
    cls' <- getRequiredClass "MIDICIDiscoveryManager"
    sendClassMessage cls' sharedInstanceSelector

-- | @- discoverWithHandler:@
discoverWithHandler :: IsMIDICIDiscoveryManager midiciDiscoveryManager => midiciDiscoveryManager -> Ptr () -> IO ()
discoverWithHandler midiciDiscoveryManager completedHandler =
  sendMessage midiciDiscoveryManager discoverWithHandlerSelector completedHandler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedInstance@
sharedInstanceSelector :: Selector '[] (Id MIDICIDiscoveryManager)
sharedInstanceSelector = mkSelector "sharedInstance"

-- | @Selector@ for @discoverWithHandler:@
discoverWithHandlerSelector :: Selector '[Ptr ()] ()
discoverWithHandlerSelector = mkSelector "discoverWithHandler:"

