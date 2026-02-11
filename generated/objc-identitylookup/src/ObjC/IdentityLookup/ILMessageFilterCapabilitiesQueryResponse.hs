{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A response to an ILMessageFilterCapabilitiesQueryRequest.
--
-- Generated bindings for @ILMessageFilterCapabilitiesQueryResponse@.
module ObjC.IdentityLookup.ILMessageFilterCapabilitiesQueryResponse
  ( ILMessageFilterCapabilitiesQueryResponse
  , IsILMessageFilterCapabilitiesQueryResponse(..)
  , transactionalSubActions
  , setTransactionalSubActions
  , promotionalSubActions
  , setPromotionalSubActions
  , transactionalSubActionsSelector
  , setTransactionalSubActionsSelector
  , promotionalSubActionsSelector
  , setPromotionalSubActionsSelector


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

-- | Array of type ILMessageFilterSubAction under Transactional
--
-- ObjC selector: @- transactionalSubActions@
transactionalSubActions :: IsILMessageFilterCapabilitiesQueryResponse ilMessageFilterCapabilitiesQueryResponse => ilMessageFilterCapabilitiesQueryResponse -> IO (Id NSArray)
transactionalSubActions ilMessageFilterCapabilitiesQueryResponse  =
  sendMsg ilMessageFilterCapabilitiesQueryResponse (mkSelector "transactionalSubActions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Array of type ILMessageFilterSubAction under Transactional
--
-- ObjC selector: @- setTransactionalSubActions:@
setTransactionalSubActions :: (IsILMessageFilterCapabilitiesQueryResponse ilMessageFilterCapabilitiesQueryResponse, IsNSArray value) => ilMessageFilterCapabilitiesQueryResponse -> value -> IO ()
setTransactionalSubActions ilMessageFilterCapabilitiesQueryResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg ilMessageFilterCapabilitiesQueryResponse (mkSelector "setTransactionalSubActions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Array of type ILMessageFilterSubAction under Promotional
--
-- ObjC selector: @- promotionalSubActions@
promotionalSubActions :: IsILMessageFilterCapabilitiesQueryResponse ilMessageFilterCapabilitiesQueryResponse => ilMessageFilterCapabilitiesQueryResponse -> IO (Id NSArray)
promotionalSubActions ilMessageFilterCapabilitiesQueryResponse  =
  sendMsg ilMessageFilterCapabilitiesQueryResponse (mkSelector "promotionalSubActions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Array of type ILMessageFilterSubAction under Promotional
--
-- ObjC selector: @- setPromotionalSubActions:@
setPromotionalSubActions :: (IsILMessageFilterCapabilitiesQueryResponse ilMessageFilterCapabilitiesQueryResponse, IsNSArray value) => ilMessageFilterCapabilitiesQueryResponse -> value -> IO ()
setPromotionalSubActions ilMessageFilterCapabilitiesQueryResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg ilMessageFilterCapabilitiesQueryResponse (mkSelector "setPromotionalSubActions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @transactionalSubActions@
transactionalSubActionsSelector :: Selector
transactionalSubActionsSelector = mkSelector "transactionalSubActions"

-- | @Selector@ for @setTransactionalSubActions:@
setTransactionalSubActionsSelector :: Selector
setTransactionalSubActionsSelector = mkSelector "setTransactionalSubActions:"

-- | @Selector@ for @promotionalSubActions@
promotionalSubActionsSelector :: Selector
promotionalSubActionsSelector = mkSelector "promotionalSubActions"

-- | @Selector@ for @setPromotionalSubActions:@
setPromotionalSubActionsSelector :: Selector
setPromotionalSubActionsSelector = mkSelector "setPromotionalSubActions:"

