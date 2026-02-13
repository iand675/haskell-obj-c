{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CXEndCallAction@.
module ObjC.CallKit.CXEndCallAction
  ( CXEndCallAction
  , IsCXEndCallAction(..)
  , fulfillWithDateEnded
  , fulfillWithDateEndedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CallKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Normally, providers can just call -[CXAction fulfill] to indicate action fulfillment. Use this method to note a specific date that the call ended.
--
-- ObjC selector: @- fulfillWithDateEnded:@
fulfillWithDateEnded :: (IsCXEndCallAction cxEndCallAction, IsNSDate dateEnded) => cxEndCallAction -> dateEnded -> IO ()
fulfillWithDateEnded cxEndCallAction dateEnded =
  sendMessage cxEndCallAction fulfillWithDateEndedSelector (toNSDate dateEnded)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fulfillWithDateEnded:@
fulfillWithDateEndedSelector :: Selector '[Id NSDate] ()
fulfillWithDateEndedSelector = mkSelector "fulfillWithDateEnded:"

