{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , userString
  , setUserString
  , userInfo
  , setUserInfo
  , actionSelector
  , initSelector
  , initWithClassificationActionSelector
  , setUserInfoSelector
  , setUserStringSelector
  , userInfoSelector
  , userStringSelector

  -- * Enum types
  , ILClassificationAction(ILClassificationAction)
  , pattern ILClassificationActionNone
  , pattern ILClassificationActionReportNotJunk
  , pattern ILClassificationActionReportJunk
  , pattern ILClassificationActionReportJunkAndBlockSender

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

-- | @- initWithClassificationAction:@
initWithClassificationAction :: IsILClassificationResponse ilClassificationResponse => ilClassificationResponse -> ILClassificationAction -> IO (Id ILClassificationResponse)
initWithClassificationAction ilClassificationResponse action =
  sendOwnedMessage ilClassificationResponse initWithClassificationActionSelector action

-- | @- init@
init_ :: IsILClassificationResponse ilClassificationResponse => ilClassificationResponse -> IO (Id ILClassificationResponse)
init_ ilClassificationResponse =
  sendOwnedMessage ilClassificationResponse initSelector

-- | @- action@
action :: IsILClassificationResponse ilClassificationResponse => ilClassificationResponse -> IO ILClassificationAction
action ilClassificationResponse =
  sendMessage ilClassificationResponse actionSelector

-- | @- userString@
userString :: IsILClassificationResponse ilClassificationResponse => ilClassificationResponse -> IO (Id NSString)
userString ilClassificationResponse =
  sendMessage ilClassificationResponse userStringSelector

-- | @- setUserString:@
setUserString :: (IsILClassificationResponse ilClassificationResponse, IsNSString value) => ilClassificationResponse -> value -> IO ()
setUserString ilClassificationResponse value =
  sendMessage ilClassificationResponse setUserStringSelector (toNSString value)

-- | @- userInfo@
userInfo :: IsILClassificationResponse ilClassificationResponse => ilClassificationResponse -> IO (Id NSDictionary)
userInfo ilClassificationResponse =
  sendMessage ilClassificationResponse userInfoSelector

-- | @- setUserInfo:@
setUserInfo :: (IsILClassificationResponse ilClassificationResponse, IsNSDictionary value) => ilClassificationResponse -> value -> IO ()
setUserInfo ilClassificationResponse value =
  sendMessage ilClassificationResponse setUserInfoSelector (toNSDictionary value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithClassificationAction:@
initWithClassificationActionSelector :: Selector '[ILClassificationAction] (Id ILClassificationResponse)
initWithClassificationActionSelector = mkSelector "initWithClassificationAction:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ILClassificationResponse)
initSelector = mkSelector "init"

-- | @Selector@ for @action@
actionSelector :: Selector '[] ILClassificationAction
actionSelector = mkSelector "action"

-- | @Selector@ for @userString@
userStringSelector :: Selector '[] (Id NSString)
userStringSelector = mkSelector "userString"

-- | @Selector@ for @setUserString:@
setUserStringSelector :: Selector '[Id NSString] ()
setUserStringSelector = mkSelector "setUserString:"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector '[] (Id NSDictionary)
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @setUserInfo:@
setUserInfoSelector :: Selector '[Id NSDictionary] ()
setUserInfoSelector = mkSelector "setUserInfo:"

