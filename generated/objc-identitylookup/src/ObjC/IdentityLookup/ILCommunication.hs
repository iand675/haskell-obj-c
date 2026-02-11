{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An incident of communication via some medium.
--
-- Generated bindings for @ILCommunication@.
module ObjC.IdentityLookup.ILCommunication
  ( ILCommunication
  , IsILCommunication(..)
  , isEqualToCommunication
  , init_
  , sender
  , dateReceived
  , isEqualToCommunicationSelector
  , initSelector
  , senderSelector
  , dateReceivedSelector


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

import ObjC.IdentityLookup.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- isEqualToCommunication:@
isEqualToCommunication :: (IsILCommunication ilCommunication, IsILCommunication communication) => ilCommunication -> communication -> IO Bool
isEqualToCommunication ilCommunication  communication =
withObjCPtr communication $ \raw_communication ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ilCommunication (mkSelector "isEqualToCommunication:") retCULong [argPtr (castPtr raw_communication :: Ptr ())]

-- | @- init@
init_ :: IsILCommunication ilCommunication => ilCommunication -> IO (Id ILCommunication)
init_ ilCommunication  =
  sendMsg ilCommunication (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The phone number or e-mail address of the sender.  The value will be nil if the sender is unknown.
--
-- ObjC selector: @- sender@
sender :: IsILCommunication ilCommunication => ilCommunication -> IO (Id NSString)
sender ilCommunication  =
  sendMsg ilCommunication (mkSelector "sender") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- dateReceived@
dateReceived :: IsILCommunication ilCommunication => ilCommunication -> IO (Id NSDate)
dateReceived ilCommunication  =
  sendMsg ilCommunication (mkSelector "dateReceived") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isEqualToCommunication:@
isEqualToCommunicationSelector :: Selector
isEqualToCommunicationSelector = mkSelector "isEqualToCommunication:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @sender@
senderSelector :: Selector
senderSelector = mkSelector "sender"

-- | @Selector@ for @dateReceived@
dateReceivedSelector :: Selector
dateReceivedSelector = mkSelector "dateReceived"

