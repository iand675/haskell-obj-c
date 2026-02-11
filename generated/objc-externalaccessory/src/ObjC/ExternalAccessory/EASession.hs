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

import ObjC.ExternalAccessory.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithAccessory:forProtocol:@
initWithAccessory_forProtocol :: (IsEASession eaSession, IsEAAccessory accessory, IsNSString protocolString) => eaSession -> accessory -> protocolString -> IO (Id EASession)
initWithAccessory_forProtocol eaSession  accessory protocolString =
withObjCPtr accessory $ \raw_accessory ->
  withObjCPtr protocolString $ \raw_protocolString ->
      sendMsg eaSession (mkSelector "initWithAccessory:forProtocol:") (retPtr retVoid) [argPtr (castPtr raw_accessory :: Ptr ()), argPtr (castPtr raw_protocolString :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAccessory:forProtocol:@
initWithAccessory_forProtocolSelector :: Selector
initWithAccessory_forProtocolSelector = mkSelector "initWithAccessory:forProtocol:"

