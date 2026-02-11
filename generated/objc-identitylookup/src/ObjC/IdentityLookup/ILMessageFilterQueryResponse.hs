{-# LANGUAGE PatternSynonyms #-}
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
  , subActionSelector
  , setSubActionSelector

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
import ObjC.IdentityLookup.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Action to take for the received message.
--
-- ObjC selector: @- action@
action :: IsILMessageFilterQueryResponse ilMessageFilterQueryResponse => ilMessageFilterQueryResponse -> IO ILMessageFilterAction
action ilMessageFilterQueryResponse  =
  fmap (coerce :: CLong -> ILMessageFilterAction) $ sendMsg ilMessageFilterQueryResponse (mkSelector "action") retCLong []

-- | Action to take for the received message.
--
-- ObjC selector: @- setAction:@
setAction :: IsILMessageFilterQueryResponse ilMessageFilterQueryResponse => ilMessageFilterQueryResponse -> ILMessageFilterAction -> IO ()
setAction ilMessageFilterQueryResponse  value =
  sendMsg ilMessageFilterQueryResponse (mkSelector "setAction:") retVoid [argCLong (coerce value)]

-- | SubAction to take for the received message.
--
-- ObjC selector: @- subAction@
subAction :: IsILMessageFilterQueryResponse ilMessageFilterQueryResponse => ilMessageFilterQueryResponse -> IO ILMessageFilterSubAction
subAction ilMessageFilterQueryResponse  =
  fmap (coerce :: CLong -> ILMessageFilterSubAction) $ sendMsg ilMessageFilterQueryResponse (mkSelector "subAction") retCLong []

-- | SubAction to take for the received message.
--
-- ObjC selector: @- setSubAction:@
setSubAction :: IsILMessageFilterQueryResponse ilMessageFilterQueryResponse => ilMessageFilterQueryResponse -> ILMessageFilterSubAction -> IO ()
setSubAction ilMessageFilterQueryResponse  value =
  sendMsg ilMessageFilterQueryResponse (mkSelector "setSubAction:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @action@
actionSelector :: Selector
actionSelector = mkSelector "action"

-- | @Selector@ for @setAction:@
setActionSelector :: Selector
setActionSelector = mkSelector "setAction:"

-- | @Selector@ for @subAction@
subActionSelector :: Selector
subActionSelector = mkSelector "subAction"

-- | @Selector@ for @setSubAction:@
setSubActionSelector :: Selector
setSubActionSelector = mkSelector "setSubAction:"

