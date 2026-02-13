{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @EASession@.
module ObjC.ExternalAccessory.EASession
  ( EASession
  , IsEASession(..)
  , initWithAccessory_forProtocol
  , initWithAccessory_forProtocolSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ExternalAccessory.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithAccessory:forProtocol:@
initWithAccessory_forProtocol :: (IsEASession eaSession, IsEAAccessory accessory, IsNSString protocolString) => eaSession -> accessory -> protocolString -> IO (Id EASession)
initWithAccessory_forProtocol eaSession accessory protocolString =
  sendOwnedMessage eaSession initWithAccessory_forProtocolSelector (toEAAccessory accessory) (toNSString protocolString)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAccessory:forProtocol:@
initWithAccessory_forProtocolSelector :: Selector '[Id EAAccessory, Id NSString] (Id EASession)
initWithAccessory_forProtocolSelector = mkSelector "initWithAccessory:forProtocol:"

