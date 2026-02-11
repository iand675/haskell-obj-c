{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ILCallCommunication@.
module ObjC.IdentityLookup.ILCallCommunication
  ( ILCallCommunication
  , IsILCallCommunication(..)
  , isEqualToCallCommunication
  , init_
  , isEqualToCallCommunicationSelector
  , initSelector


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

-- | @- isEqualToCallCommunication:@
isEqualToCallCommunication :: (IsILCallCommunication ilCallCommunication, IsILCallCommunication communication) => ilCallCommunication -> communication -> IO Bool
isEqualToCallCommunication ilCallCommunication  communication =
withObjCPtr communication $ \raw_communication ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ilCallCommunication (mkSelector "isEqualToCallCommunication:") retCULong [argPtr (castPtr raw_communication :: Ptr ())]

-- | @- init@
init_ :: IsILCallCommunication ilCallCommunication => ilCallCommunication -> IO (Id ILCallCommunication)
init_ ilCallCommunication  =
  sendMsg ilCallCommunication (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isEqualToCallCommunication:@
isEqualToCallCommunicationSelector :: Selector
isEqualToCallCommunicationSelector = mkSelector "isEqualToCallCommunication:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

