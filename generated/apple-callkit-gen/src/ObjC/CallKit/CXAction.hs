{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CXAction@.
module ObjC.CallKit.CXAction
  ( CXAction
  , IsCXAction(..)
  , init_
  , initWithCoder
  , fulfill
  , fail_
  , uuid
  , complete
  , timeoutDate
  , completeSelector
  , failSelector
  , fulfillSelector
  , initSelector
  , initWithCoderSelector
  , timeoutDateSelector
  , uuidSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CallKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCXAction cxAction => cxAction -> IO (Id CXAction)
init_ cxAction =
  sendOwnedMessage cxAction initSelector

-- | @- initWithCoder:@
initWithCoder :: (IsCXAction cxAction, IsNSCoder aDecoder) => cxAction -> aDecoder -> IO (Id CXAction)
initWithCoder cxAction aDecoder =
  sendOwnedMessage cxAction initWithCoderSelector (toNSCoder aDecoder)

-- | Report successful execution of the receiver.
--
-- ObjC selector: @- fulfill@
fulfill :: IsCXAction cxAction => cxAction -> IO ()
fulfill cxAction =
  sendMessage cxAction fulfillSelector

-- | Report failed execution of the receiver.
--
-- ObjC selector: @- fail@
fail_ :: IsCXAction cxAction => cxAction -> IO ()
fail_ cxAction =
  sendMessage cxAction failSelector

-- | Unique ID
--
-- ObjC selector: @- UUID@
uuid :: IsCXAction cxAction => cxAction -> IO (Id NSUUID)
uuid cxAction =
  sendMessage cxAction uuidSelector

-- | Whether all actions are either fulfilled or failed
--
-- ObjC selector: @- complete@
complete :: IsCXAction cxAction => cxAction -> IO Bool
complete cxAction =
  sendMessage cxAction completeSelector

-- | @- timeoutDate@
timeoutDate :: IsCXAction cxAction => cxAction -> IO (Id NSDate)
timeoutDate cxAction =
  sendMessage cxAction timeoutDateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CXAction)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id CXAction)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @fulfill@
fulfillSelector :: Selector '[] ()
fulfillSelector = mkSelector "fulfill"

-- | @Selector@ for @fail@
failSelector :: Selector '[] ()
failSelector = mkSelector "fail"

-- | @Selector@ for @UUID@
uuidSelector :: Selector '[] (Id NSUUID)
uuidSelector = mkSelector "UUID"

-- | @Selector@ for @complete@
completeSelector :: Selector '[] Bool
completeSelector = mkSelector "complete"

-- | @Selector@ for @timeoutDate@
timeoutDateSelector :: Selector '[] (Id NSDate)
timeoutDateSelector = mkSelector "timeoutDate"

