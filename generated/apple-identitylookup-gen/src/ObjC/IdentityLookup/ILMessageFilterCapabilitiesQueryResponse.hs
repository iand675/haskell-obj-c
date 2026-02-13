{-# LANGUAGE DataKinds #-}
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
  , promotionalSubActionsSelector
  , setPromotionalSubActionsSelector
  , setTransactionalSubActionsSelector
  , transactionalSubActionsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.IdentityLookup.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Array of type ILMessageFilterSubAction under Transactional
--
-- ObjC selector: @- transactionalSubActions@
transactionalSubActions :: IsILMessageFilterCapabilitiesQueryResponse ilMessageFilterCapabilitiesQueryResponse => ilMessageFilterCapabilitiesQueryResponse -> IO (Id NSArray)
transactionalSubActions ilMessageFilterCapabilitiesQueryResponse =
  sendMessage ilMessageFilterCapabilitiesQueryResponse transactionalSubActionsSelector

-- | Array of type ILMessageFilterSubAction under Transactional
--
-- ObjC selector: @- setTransactionalSubActions:@
setTransactionalSubActions :: (IsILMessageFilterCapabilitiesQueryResponse ilMessageFilterCapabilitiesQueryResponse, IsNSArray value) => ilMessageFilterCapabilitiesQueryResponse -> value -> IO ()
setTransactionalSubActions ilMessageFilterCapabilitiesQueryResponse value =
  sendMessage ilMessageFilterCapabilitiesQueryResponse setTransactionalSubActionsSelector (toNSArray value)

-- | Array of type ILMessageFilterSubAction under Promotional
--
-- ObjC selector: @- promotionalSubActions@
promotionalSubActions :: IsILMessageFilterCapabilitiesQueryResponse ilMessageFilterCapabilitiesQueryResponse => ilMessageFilterCapabilitiesQueryResponse -> IO (Id NSArray)
promotionalSubActions ilMessageFilterCapabilitiesQueryResponse =
  sendMessage ilMessageFilterCapabilitiesQueryResponse promotionalSubActionsSelector

-- | Array of type ILMessageFilterSubAction under Promotional
--
-- ObjC selector: @- setPromotionalSubActions:@
setPromotionalSubActions :: (IsILMessageFilterCapabilitiesQueryResponse ilMessageFilterCapabilitiesQueryResponse, IsNSArray value) => ilMessageFilterCapabilitiesQueryResponse -> value -> IO ()
setPromotionalSubActions ilMessageFilterCapabilitiesQueryResponse value =
  sendMessage ilMessageFilterCapabilitiesQueryResponse setPromotionalSubActionsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @transactionalSubActions@
transactionalSubActionsSelector :: Selector '[] (Id NSArray)
transactionalSubActionsSelector = mkSelector "transactionalSubActions"

-- | @Selector@ for @setTransactionalSubActions:@
setTransactionalSubActionsSelector :: Selector '[Id NSArray] ()
setTransactionalSubActionsSelector = mkSelector "setTransactionalSubActions:"

-- | @Selector@ for @promotionalSubActions@
promotionalSubActionsSelector :: Selector '[] (Id NSArray)
promotionalSubActionsSelector = mkSelector "promotionalSubActions"

-- | @Selector@ for @setPromotionalSubActions:@
setPromotionalSubActionsSelector :: Selector '[Id NSArray] ()
setPromotionalSubActionsSelector = mkSelector "setPromotionalSubActions:"

