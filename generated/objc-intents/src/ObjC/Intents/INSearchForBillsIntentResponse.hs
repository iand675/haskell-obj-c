{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSearchForBillsIntentResponse@.
module ObjC.Intents.INSearchForBillsIntentResponse
  ( INSearchForBillsIntentResponse
  , IsINSearchForBillsIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , bills
  , setBills
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector
  , billsSelector
  , setBillsSelector

  -- * Enum types
  , INSearchForBillsIntentResponseCode(INSearchForBillsIntentResponseCode)
  , pattern INSearchForBillsIntentResponseCodeUnspecified
  , pattern INSearchForBillsIntentResponseCodeReady
  , pattern INSearchForBillsIntentResponseCodeInProgress
  , pattern INSearchForBillsIntentResponseCodeSuccess
  , pattern INSearchForBillsIntentResponseCodeFailure
  , pattern INSearchForBillsIntentResponseCodeFailureRequiringAppLaunch
  , pattern INSearchForBillsIntentResponseCodeFailureCredentialsUnverified
  , pattern INSearchForBillsIntentResponseCodeFailureBillNotFound

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

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINSearchForBillsIntentResponse inSearchForBillsIntentResponse => inSearchForBillsIntentResponse -> IO RawId
init_ inSearchForBillsIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inSearchForBillsIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSearchForBillsIntentResponse inSearchForBillsIntentResponse, IsNSUserActivity userActivity) => inSearchForBillsIntentResponse -> INSearchForBillsIntentResponseCode -> userActivity -> IO (Id INSearchForBillsIntentResponse)
initWithCode_userActivity inSearchForBillsIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inSearchForBillsIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINSearchForBillsIntentResponse inSearchForBillsIntentResponse => inSearchForBillsIntentResponse -> IO INSearchForBillsIntentResponseCode
code inSearchForBillsIntentResponse  =
  fmap (coerce :: CLong -> INSearchForBillsIntentResponseCode) $ sendMsg inSearchForBillsIntentResponse (mkSelector "code") retCLong []

-- | @- bills@
bills :: IsINSearchForBillsIntentResponse inSearchForBillsIntentResponse => inSearchForBillsIntentResponse -> IO (Id NSArray)
bills inSearchForBillsIntentResponse  =
  sendMsg inSearchForBillsIntentResponse (mkSelector "bills") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBills:@
setBills :: (IsINSearchForBillsIntentResponse inSearchForBillsIntentResponse, IsNSArray value) => inSearchForBillsIntentResponse -> value -> IO ()
setBills inSearchForBillsIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inSearchForBillsIntentResponse (mkSelector "setBills:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector
codeSelector = mkSelector "code"

-- | @Selector@ for @bills@
billsSelector :: Selector
billsSelector = mkSelector "bills"

-- | @Selector@ for @setBills:@
setBillsSelector :: Selector
setBillsSelector = mkSelector "setBills:"

