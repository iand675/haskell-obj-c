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

-- | Normally, providers can just call -[CXAction fulfill] to indicate action fulfillment. Use this method to note a specific date that the call ended.
--
-- ObjC selector: @- fulfillWithDateEnded:@
fulfillWithDateEnded :: (IsCXEndCallAction cxEndCallAction, IsNSDate dateEnded) => cxEndCallAction -> dateEnded -> IO ()
fulfillWithDateEnded cxEndCallAction  dateEnded =
withObjCPtr dateEnded $ \raw_dateEnded ->
    sendMsg cxEndCallAction (mkSelector "fulfillWithDateEnded:") retVoid [argPtr (castPtr raw_dateEnded :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fulfillWithDateEnded:@
fulfillWithDateEndedSelector :: Selector
fulfillWithDateEndedSelector = mkSelector "fulfillWithDateEnded:"

