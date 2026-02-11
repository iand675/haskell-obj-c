{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ILCallClassificationRequest@.
module ObjC.IdentityLookup.ILCallClassificationRequest
  ( ILCallClassificationRequest
  , IsILCallClassificationRequest(..)
  , init_
  , callCommunications
  , initSelector
  , callCommunicationsSelector


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
init_ :: IsILCallClassificationRequest ilCallClassificationRequest => ilCallClassificationRequest -> IO (Id ILCallClassificationRequest)
init_ ilCallClassificationRequest  =
  sendMsg ilCallClassificationRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- callCommunications@
callCommunications :: IsILCallClassificationRequest ilCallClassificationRequest => ilCallClassificationRequest -> IO (Id NSArray)
callCommunications ilCallClassificationRequest  =
  sendMsg ilCallClassificationRequest (mkSelector "callCommunications") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @callCommunications@
callCommunicationsSelector :: Selector
callCommunicationsSelector = mkSelector "callCommunications"

