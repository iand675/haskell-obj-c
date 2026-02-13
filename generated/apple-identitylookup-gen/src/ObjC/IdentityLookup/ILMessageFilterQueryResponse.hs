{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A response to an ILMessageFilterQueryRequest.
--
-- Generated bindings for @ILMessageFilterQueryResponse@.
module ObjC.IdentityLookup.ILMessageFilterQueryResponse
  ( ILMessageFilterQueryResponse
  , IsILMessageFilterQueryResponse(..)
  , action
  , setAction
  , subAction
  , setSubAction
  , actionSelector
  , setActionSelector
  , setSubActionSelector
  , subActionSelector

  -- * Enum types
  , ILMessageFilterAction(ILMessageFilterAction)
  , pattern ILMessageFilterActionNone
  , pattern ILMessageFilterActionAllow
  , pattern ILMessageFilterActionJunk
  , pattern ILMessageFilterActionFilter
  , pattern ILMessageFilterActionPromotion
  , pattern ILMessageFilterActionTransaction
  , ILMessageFilterSubAction(ILMessageFilterSubAction)
  , pattern ILMessageFilterSubActionNone
  , pattern ILMessageFilterSubActionTransactionalOthers
  , pattern ILMessageFilterSubActionTransactionalFinance
  , pattern ILMessageFilterSubActionTransactionalOrders
  , pattern ILMessageFilterSubActionTransactionalReminders
  , pattern ILMessageFilterSubActionTransactionalHealth
  , pattern ILMessageFilterSubActionTransactionalWeather
  , pattern ILMessageFilterSubActionTransactionalCarrier
  , pattern ILMessageFilterSubActionTransactionalRewards
  , pattern ILMessageFilterSubActionTransactionalPublicServices
  , pattern ILMessageFilterSubActionPromotionalOthers
  , pattern ILMessageFilterSubActionPromotionalOffers
  , pattern ILMessageFilterSubActionPromotionalCoupons

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.IdentityLookup.Internal.Classes
import ObjC.IdentityLookup.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Action to take for the received message.
--
-- ObjC selector: @- action@
action :: IsILMessageFilterQueryResponse ilMessageFilterQueryResponse => ilMessageFilterQueryResponse -> IO ILMessageFilterAction
action ilMessageFilterQueryResponse =
  sendMessage ilMessageFilterQueryResponse actionSelector

-- | Action to take for the received message.
--
-- ObjC selector: @- setAction:@
setAction :: IsILMessageFilterQueryResponse ilMessageFilterQueryResponse => ilMessageFilterQueryResponse -> ILMessageFilterAction -> IO ()
setAction ilMessageFilterQueryResponse value =
  sendMessage ilMessageFilterQueryResponse setActionSelector value

-- | SubAction to take for the received message.
--
-- ObjC selector: @- subAction@
subAction :: IsILMessageFilterQueryResponse ilMessageFilterQueryResponse => ilMessageFilterQueryResponse -> IO ILMessageFilterSubAction
subAction ilMessageFilterQueryResponse =
  sendMessage ilMessageFilterQueryResponse subActionSelector

-- | SubAction to take for the received message.
--
-- ObjC selector: @- setSubAction:@
setSubAction :: IsILMessageFilterQueryResponse ilMessageFilterQueryResponse => ilMessageFilterQueryResponse -> ILMessageFilterSubAction -> IO ()
setSubAction ilMessageFilterQueryResponse value =
  sendMessage ilMessageFilterQueryResponse setSubActionSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @action@
actionSelector :: Selector '[] ILMessageFilterAction
actionSelector = mkSelector "action"

-- | @Selector@ for @setAction:@
setActionSelector :: Selector '[ILMessageFilterAction] ()
setActionSelector = mkSelector "setAction:"

-- | @Selector@ for @subAction@
subActionSelector :: Selector '[] ILMessageFilterSubAction
subActionSelector = mkSelector "subAction"

-- | @Selector@ for @setSubAction:@
setSubActionSelector :: Selector '[ILMessageFilterSubAction] ()
setSubActionSelector = mkSelector "setSubAction:"

