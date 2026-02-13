{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Manages a session between the extension and host.
--
-- Generated bindings for @DDDiscoverySession@.
module ObjC.DeviceDiscoveryExtension.DDDiscoverySession
  ( DDDiscoverySession
  , IsDDDiscoverySession(..)
  , reportEvent
  , reportEventSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.DeviceDiscoveryExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Reports an event to the host.
--
-- ObjC selector: @- reportEvent:@
reportEvent :: (IsDDDiscoverySession ddDiscoverySession, IsDDDeviceEvent inEvent) => ddDiscoverySession -> inEvent -> IO ()
reportEvent ddDiscoverySession inEvent =
  sendMessage ddDiscoverySession reportEventSelector (toDDDeviceEvent inEvent)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reportEvent:@
reportEventSelector :: Selector '[Id DDDeviceEvent] ()
reportEventSelector = mkSelector "reportEvent:"

