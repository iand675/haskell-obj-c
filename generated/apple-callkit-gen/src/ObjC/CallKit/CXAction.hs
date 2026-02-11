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
  , initSelector
  , initWithCoderSelector
  , fulfillSelector
  , failSelector
  , uuidSelector
  , completeSelector
  , timeoutDateSelector


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

-- | @- init@
init_ :: IsCXAction cxAction => cxAction -> IO (Id CXAction)
init_ cxAction  =
    sendMsg cxAction (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsCXAction cxAction, IsNSCoder aDecoder) => cxAction -> aDecoder -> IO (Id CXAction)
initWithCoder cxAction  aDecoder =
  withObjCPtr aDecoder $ \raw_aDecoder ->
      sendMsg cxAction (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ())] >>= ownedObject . castPtr

-- | Report successful execution of the receiver.
--
-- ObjC selector: @- fulfill@
fulfill :: IsCXAction cxAction => cxAction -> IO ()
fulfill cxAction  =
    sendMsg cxAction (mkSelector "fulfill") retVoid []

-- | Report failed execution of the receiver.
--
-- ObjC selector: @- fail@
fail_ :: IsCXAction cxAction => cxAction -> IO ()
fail_ cxAction  =
    sendMsg cxAction (mkSelector "fail") retVoid []

-- | Unique ID
--
-- ObjC selector: @- UUID@
uuid :: IsCXAction cxAction => cxAction -> IO (Id NSUUID)
uuid cxAction  =
    sendMsg cxAction (mkSelector "UUID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Whether all actions are either fulfilled or failed
--
-- ObjC selector: @- complete@
complete :: IsCXAction cxAction => cxAction -> IO Bool
complete cxAction  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cxAction (mkSelector "complete") retCULong []

-- | @- timeoutDate@
timeoutDate :: IsCXAction cxAction => cxAction -> IO (Id NSDate)
timeoutDate cxAction  =
    sendMsg cxAction (mkSelector "timeoutDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @fulfill@
fulfillSelector :: Selector
fulfillSelector = mkSelector "fulfill"

-- | @Selector@ for @fail@
failSelector :: Selector
failSelector = mkSelector "fail"

-- | @Selector@ for @UUID@
uuidSelector :: Selector
uuidSelector = mkSelector "UUID"

-- | @Selector@ for @complete@
completeSelector :: Selector
completeSelector = mkSelector "complete"

-- | @Selector@ for @timeoutDate@
timeoutDateSelector :: Selector
timeoutDateSelector = mkSelector "timeoutDate"

