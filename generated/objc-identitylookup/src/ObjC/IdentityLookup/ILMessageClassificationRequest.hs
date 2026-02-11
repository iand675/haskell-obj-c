{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ILMessageClassificationRequest@.
module ObjC.IdentityLookup.ILMessageClassificationRequest
  ( ILMessageClassificationRequest
  , IsILMessageClassificationRequest(..)
  , init_
  , messageCommunications
  , initSelector
  , messageCommunicationsSelector


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

-- | @- init@
init_ :: IsILMessageClassificationRequest ilMessageClassificationRequest => ilMessageClassificationRequest -> IO (Id ILMessageClassificationRequest)
init_ ilMessageClassificationRequest  =
  sendMsg ilMessageClassificationRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- messageCommunications@
messageCommunications :: IsILMessageClassificationRequest ilMessageClassificationRequest => ilMessageClassificationRequest -> IO (Id NSArray)
messageCommunications ilMessageClassificationRequest  =
  sendMsg ilMessageClassificationRequest (mkSelector "messageCommunications") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @messageCommunications@
messageCommunicationsSelector :: Selector
messageCommunicationsSelector = mkSelector "messageCommunications"

