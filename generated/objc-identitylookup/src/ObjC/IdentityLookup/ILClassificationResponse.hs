{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A response to an ILClassificationRequest.
--
-- Generated bindings for @ILClassificationResponse@.
module ObjC.IdentityLookup.ILClassificationResponse
  ( ILClassificationResponse
  , IsILClassificationResponse(..)
  , initWithClassificationAction
  , init_
  , action
  , userInfo
  , setUserInfo
  , initWithClassificationActionSelector
  , initSelector
  , actionSelector
  , userInfoSelector
  , setUserInfoSelector

  -- * Enum types
  , ILClassificationAction(ILClassificationAction)
  , pattern ILClassificationActionNone
  , pattern ILClassificationActionReportNotJunk
  , pattern ILClassificationActionReportJunk
  , pattern ILClassificationActionReportJunkAndBlockSender

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

-- | @- initWithClassificationAction:@
initWithClassificationAction :: IsILClassificationResponse ilClassificationResponse => ilClassificationResponse -> ILClassificationAction -> IO (Id ILClassificationResponse)
initWithClassificationAction ilClassificationResponse  action =
  sendMsg ilClassificationResponse (mkSelector "initWithClassificationAction:") (retPtr retVoid) [argCLong (coerce action)] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsILClassificationResponse ilClassificationResponse => ilClassificationResponse -> IO (Id ILClassificationResponse)
init_ ilClassificationResponse  =
  sendMsg ilClassificationResponse (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- action@
action :: IsILClassificationResponse ilClassificationResponse => ilClassificationResponse -> IO ILClassificationAction
action ilClassificationResponse  =
  fmap (coerce :: CLong -> ILClassificationAction) $ sendMsg ilClassificationResponse (mkSelector "action") retCLong []

-- | @- userInfo@
userInfo :: IsILClassificationResponse ilClassificationResponse => ilClassificationResponse -> IO (Id NSDictionary)
userInfo ilClassificationResponse  =
  sendMsg ilClassificationResponse (mkSelector "userInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserInfo:@
setUserInfo :: (IsILClassificationResponse ilClassificationResponse, IsNSDictionary value) => ilClassificationResponse -> value -> IO ()
setUserInfo ilClassificationResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg ilClassificationResponse (mkSelector "setUserInfo:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithClassificationAction:@
initWithClassificationActionSelector :: Selector
initWithClassificationActionSelector = mkSelector "initWithClassificationAction:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @action@
actionSelector :: Selector
actionSelector = mkSelector "action"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @setUserInfo:@
setUserInfoSelector :: Selector
setUserInfoSelector = mkSelector "setUserInfo:"

