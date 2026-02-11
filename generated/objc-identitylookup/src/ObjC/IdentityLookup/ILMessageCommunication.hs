{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ILMessageCommunication@.
module ObjC.IdentityLookup.ILMessageCommunication
  ( ILMessageCommunication
  , IsILMessageCommunication(..)
  , isEqualToMessageCommunication
  , init_
  , messageBody
  , isEqualToMessageCommunicationSelector
  , initSelector
  , messageBodySelector


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

-- | @- isEqualToMessageCommunication:@
isEqualToMessageCommunication :: (IsILMessageCommunication ilMessageCommunication, IsILMessageCommunication communication) => ilMessageCommunication -> communication -> IO Bool
isEqualToMessageCommunication ilMessageCommunication  communication =
withObjCPtr communication $ \raw_communication ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ilMessageCommunication (mkSelector "isEqualToMessageCommunication:") retCULong [argPtr (castPtr raw_communication :: Ptr ())]

-- | @- init@
init_ :: IsILMessageCommunication ilMessageCommunication => ilMessageCommunication -> IO (Id ILMessageCommunication)
init_ ilMessageCommunication  =
  sendMsg ilMessageCommunication (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- messageBody@
messageBody :: IsILMessageCommunication ilMessageCommunication => ilMessageCommunication -> IO (Id NSString)
messageBody ilMessageCommunication  =
  sendMsg ilMessageCommunication (mkSelector "messageBody") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isEqualToMessageCommunication:@
isEqualToMessageCommunicationSelector :: Selector
isEqualToMessageCommunicationSelector = mkSelector "isEqualToMessageCommunication:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @messageBody@
messageBodySelector :: Selector
messageBodySelector = mkSelector "messageBody"

