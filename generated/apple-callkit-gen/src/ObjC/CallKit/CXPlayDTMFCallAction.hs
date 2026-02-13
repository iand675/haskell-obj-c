{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CXPlayDTMFCallAction@.
module ObjC.CallKit.CXPlayDTMFCallAction
  ( CXPlayDTMFCallAction
  , IsCXPlayDTMFCallAction(..)
  , initWithCallUUID_digits_type
  , initWithCoder
  , initWithCallUUID
  , digits
  , setDigits
  , type_
  , setType
  , digitsSelector
  , initWithCallUUIDSelector
  , initWithCallUUID_digits_typeSelector
  , initWithCoderSelector
  , setDigitsSelector
  , setTypeSelector
  , typeSelector

  -- * Enum types
  , CXPlayDTMFCallActionType(CXPlayDTMFCallActionType)
  , pattern CXPlayDTMFCallActionTypeSingleTone
  , pattern CXPlayDTMFCallActionTypeSoftPause
  , pattern CXPlayDTMFCallActionTypeHardPause

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CallKit.Internal.Classes
import ObjC.CallKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithCallUUID:digits:type:@
initWithCallUUID_digits_type :: (IsCXPlayDTMFCallAction cxPlayDTMFCallAction, IsNSUUID callUUID, IsNSString digits) => cxPlayDTMFCallAction -> callUUID -> digits -> CXPlayDTMFCallActionType -> IO (Id CXPlayDTMFCallAction)
initWithCallUUID_digits_type cxPlayDTMFCallAction callUUID digits type_ =
  sendOwnedMessage cxPlayDTMFCallAction initWithCallUUID_digits_typeSelector (toNSUUID callUUID) (toNSString digits) type_

-- | @- initWithCoder:@
initWithCoder :: (IsCXPlayDTMFCallAction cxPlayDTMFCallAction, IsNSCoder aDecoder) => cxPlayDTMFCallAction -> aDecoder -> IO (Id CXPlayDTMFCallAction)
initWithCoder cxPlayDTMFCallAction aDecoder =
  sendOwnedMessage cxPlayDTMFCallAction initWithCoderSelector (toNSCoder aDecoder)

-- | @- initWithCallUUID:@
initWithCallUUID :: (IsCXPlayDTMFCallAction cxPlayDTMFCallAction, IsNSUUID callUUID) => cxPlayDTMFCallAction -> callUUID -> IO (Id CXPlayDTMFCallAction)
initWithCallUUID cxPlayDTMFCallAction callUUID =
  sendOwnedMessage cxPlayDTMFCallAction initWithCallUUIDSelector (toNSUUID callUUID)

-- | @- digits@
digits :: IsCXPlayDTMFCallAction cxPlayDTMFCallAction => cxPlayDTMFCallAction -> IO (Id NSString)
digits cxPlayDTMFCallAction =
  sendMessage cxPlayDTMFCallAction digitsSelector

-- | @- setDigits:@
setDigits :: (IsCXPlayDTMFCallAction cxPlayDTMFCallAction, IsNSString value) => cxPlayDTMFCallAction -> value -> IO ()
setDigits cxPlayDTMFCallAction value =
  sendMessage cxPlayDTMFCallAction setDigitsSelector (toNSString value)

-- | @- type@
type_ :: IsCXPlayDTMFCallAction cxPlayDTMFCallAction => cxPlayDTMFCallAction -> IO CXPlayDTMFCallActionType
type_ cxPlayDTMFCallAction =
  sendMessage cxPlayDTMFCallAction typeSelector

-- | @- setType:@
setType :: IsCXPlayDTMFCallAction cxPlayDTMFCallAction => cxPlayDTMFCallAction -> CXPlayDTMFCallActionType -> IO ()
setType cxPlayDTMFCallAction value =
  sendMessage cxPlayDTMFCallAction setTypeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCallUUID:digits:type:@
initWithCallUUID_digits_typeSelector :: Selector '[Id NSUUID, Id NSString, CXPlayDTMFCallActionType] (Id CXPlayDTMFCallAction)
initWithCallUUID_digits_typeSelector = mkSelector "initWithCallUUID:digits:type:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id CXPlayDTMFCallAction)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithCallUUID:@
initWithCallUUIDSelector :: Selector '[Id NSUUID] (Id CXPlayDTMFCallAction)
initWithCallUUIDSelector = mkSelector "initWithCallUUID:"

-- | @Selector@ for @digits@
digitsSelector :: Selector '[] (Id NSString)
digitsSelector = mkSelector "digits"

-- | @Selector@ for @setDigits:@
setDigitsSelector :: Selector '[Id NSString] ()
setDigitsSelector = mkSelector "setDigits:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] CXPlayDTMFCallActionType
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[CXPlayDTMFCallActionType] ()
setTypeSelector = mkSelector "setType:"

