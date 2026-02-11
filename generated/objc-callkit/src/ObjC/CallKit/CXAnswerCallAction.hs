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

import ObjC.CallKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Normally, providers can just call -[CXAction fulfill] to indicate action fulfillment. Use this method to note a specific date that the call connected. A call is considered connected when both caller and callee can start communicating.
--
-- ObjC selector: @- fulfillWithDateConnected:@
fulfillWithDateConnected :: (IsCXAnswerCallAction cxAnswerCallAction, IsNSDate dateConnected) => cxAnswerCallAction -> dateConnected -> IO ()
fulfillWithDateConnected cxAnswerCallAction  dateConnected =
withObjCPtr dateConnected $ \raw_dateConnected ->
    sendMsg cxAnswerCallAction (mkSelector "fulfillWithDateConnected:") retVoid [argPtr (castPtr raw_dateConnected :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fulfillWithDateConnected:@
fulfillWithDateConnectedSelector :: Selector
fulfillWithDateConnectedSelector = mkSelector "fulfillWithDateConnected:"

