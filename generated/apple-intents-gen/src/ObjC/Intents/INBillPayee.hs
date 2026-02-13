{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INBillPayee@.
module ObjC.Intents.INBillPayee
  ( INBillPayee
  , IsINBillPayee(..)
  , init_
  , initWithNickname_number_organizationName
  , nickname
  , accountNumber
  , organizationName
  , accountNumberSelector
  , initSelector
  , initWithNickname_number_organizationNameSelector
  , nicknameSelector
  , organizationNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINBillPayee inBillPayee => inBillPayee -> IO (Id INBillPayee)
init_ inBillPayee =
  sendOwnedMessage inBillPayee initSelector

-- | @- initWithNickname:number:organizationName:@
initWithNickname_number_organizationName :: (IsINBillPayee inBillPayee, IsINSpeakableString nickname, IsNSString number, IsINSpeakableString organizationName) => inBillPayee -> nickname -> number -> organizationName -> IO (Id INBillPayee)
initWithNickname_number_organizationName inBillPayee nickname number organizationName =
  sendOwnedMessage inBillPayee initWithNickname_number_organizationNameSelector (toINSpeakableString nickname) (toNSString number) (toINSpeakableString organizationName)

-- | @- nickname@
nickname :: IsINBillPayee inBillPayee => inBillPayee -> IO (Id INSpeakableString)
nickname inBillPayee =
  sendMessage inBillPayee nicknameSelector

-- | @- accountNumber@
accountNumber :: IsINBillPayee inBillPayee => inBillPayee -> IO (Id NSString)
accountNumber inBillPayee =
  sendMessage inBillPayee accountNumberSelector

-- | @- organizationName@
organizationName :: IsINBillPayee inBillPayee => inBillPayee -> IO (Id INSpeakableString)
organizationName inBillPayee =
  sendMessage inBillPayee organizationNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INBillPayee)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithNickname:number:organizationName:@
initWithNickname_number_organizationNameSelector :: Selector '[Id INSpeakableString, Id NSString, Id INSpeakableString] (Id INBillPayee)
initWithNickname_number_organizationNameSelector = mkSelector "initWithNickname:number:organizationName:"

-- | @Selector@ for @nickname@
nicknameSelector :: Selector '[] (Id INSpeakableString)
nicknameSelector = mkSelector "nickname"

-- | @Selector@ for @accountNumber@
accountNumberSelector :: Selector '[] (Id NSString)
accountNumberSelector = mkSelector "accountNumber"

-- | @Selector@ for @organizationName@
organizationNameSelector :: Selector '[] (Id INSpeakableString)
organizationNameSelector = mkSelector "organizationName"

