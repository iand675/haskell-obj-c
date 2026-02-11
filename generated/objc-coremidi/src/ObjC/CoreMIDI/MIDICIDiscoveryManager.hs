{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MIDICIDiscoveryManager@.
module ObjC.CoreMIDI.MIDICIDiscoveryManager
  ( MIDICIDiscoveryManager
  , IsMIDICIDiscoveryManager(..)
  , sharedInstance
  , discoverWithHandler
  , sharedInstanceSelector
  , discoverWithHandlerSelector


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

import ObjC.CoreMIDI.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ sharedInstance@
sharedInstance :: IO (Id MIDICIDiscoveryManager)
sharedInstance  =
  do
    cls' <- getRequiredClass "MIDICIDiscoveryManager"
    sendClassMsg cls' (mkSelector "sharedInstance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- discoverWithHandler:@
discoverWithHandler :: IsMIDICIDiscoveryManager midiciDiscoveryManager => midiciDiscoveryManager -> Ptr () -> IO ()
discoverWithHandler midiciDiscoveryManager  completedHandler =
  sendMsg midiciDiscoveryManager (mkSelector "discoverWithHandler:") retVoid [argPtr (castPtr completedHandler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedInstance@
sharedInstanceSelector :: Selector
sharedInstanceSelector = mkSelector "sharedInstance"

-- | @Selector@ for @discoverWithHandler:@
discoverWithHandlerSelector :: Selector
discoverWithHandlerSelector = mkSelector "discoverWithHandler:"

