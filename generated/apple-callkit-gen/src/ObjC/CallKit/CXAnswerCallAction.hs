{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CXAnswerCallAction@.
module ObjC.CallKit.CXAnswerCallAction
  ( CXAnswerCallAction
  , IsCXAnswerCallAction(..)
  , fulfillWithDateConnected
  , fulfillWithDateConnectedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CallKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Normally, providers can just call -[CXAction fulfill] to indicate action fulfillment. Use this method to note a specific date that the call connected. A call is considered connected when both caller and callee can start communicating.
--
-- ObjC selector: @- fulfillWithDateConnected:@
fulfillWithDateConnected :: (IsCXAnswerCallAction cxAnswerCallAction, IsNSDate dateConnected) => cxAnswerCallAction -> dateConnected -> IO ()
fulfillWithDateConnected cxAnswerCallAction dateConnected =
  sendMessage cxAnswerCallAction fulfillWithDateConnectedSelector (toNSDate dateConnected)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fulfillWithDateConnected:@
fulfillWithDateConnectedSelector :: Selector '[Id NSDate] ()
fulfillWithDateConnectedSelector = mkSelector "fulfillWithDateConnected:"

