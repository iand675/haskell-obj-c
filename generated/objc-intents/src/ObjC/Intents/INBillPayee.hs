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
  , initSelector
  , initWithNickname_number_organizationNameSelector
  , nicknameSelector
  , accountNumberSelector
  , organizationNameSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINBillPayee inBillPayee => inBillPayee -> IO (Id INBillPayee)
init_ inBillPayee  =
  sendMsg inBillPayee (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithNickname:number:organizationName:@
initWithNickname_number_organizationName :: (IsINBillPayee inBillPayee, IsINSpeakableString nickname, IsNSString number, IsINSpeakableString organizationName) => inBillPayee -> nickname -> number -> organizationName -> IO (Id INBillPayee)
initWithNickname_number_organizationName inBillPayee  nickname number organizationName =
withObjCPtr nickname $ \raw_nickname ->
  withObjCPtr number $ \raw_number ->
    withObjCPtr organizationName $ \raw_organizationName ->
        sendMsg inBillPayee (mkSelector "initWithNickname:number:organizationName:") (retPtr retVoid) [argPtr (castPtr raw_nickname :: Ptr ()), argPtr (castPtr raw_number :: Ptr ()), argPtr (castPtr raw_organizationName :: Ptr ())] >>= ownedObject . castPtr

-- | @- nickname@
nickname :: IsINBillPayee inBillPayee => inBillPayee -> IO (Id INSpeakableString)
nickname inBillPayee  =
  sendMsg inBillPayee (mkSelector "nickname") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- accountNumber@
accountNumber :: IsINBillPayee inBillPayee => inBillPayee -> IO (Id NSString)
accountNumber inBillPayee  =
  sendMsg inBillPayee (mkSelector "accountNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- organizationName@
organizationName :: IsINBillPayee inBillPayee => inBillPayee -> IO (Id INSpeakableString)
organizationName inBillPayee  =
  sendMsg inBillPayee (mkSelector "organizationName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithNickname:number:organizationName:@
initWithNickname_number_organizationNameSelector :: Selector
initWithNickname_number_organizationNameSelector = mkSelector "initWithNickname:number:organizationName:"

-- | @Selector@ for @nickname@
nicknameSelector :: Selector
nicknameSelector = mkSelector "nickname"

-- | @Selector@ for @accountNumber@
accountNumberSelector :: Selector
accountNumberSelector = mkSelector "accountNumber"

-- | @Selector@ for @organizationName@
organizationNameSelector :: Selector
organizationNameSelector = mkSelector "organizationName"

