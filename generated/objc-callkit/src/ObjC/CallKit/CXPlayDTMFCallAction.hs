{-# LANGUAGE PatternSynonyms #-}
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
  , initWithCallUUID_digits_typeSelector
  , initWithCoderSelector
  , initWithCallUUIDSelector
  , digitsSelector
  , setDigitsSelector
  , typeSelector
  , setTypeSelector

  -- * Enum types
  , CXPlayDTMFCallActionType(CXPlayDTMFCallActionType)
  , pattern CXPlayDTMFCallActionTypeSingleTone
  , pattern CXPlayDTMFCallActionTypeSoftPause
  , pattern CXPlayDTMFCallActionTypeHardPause

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
import ObjC.CallKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithCallUUID:digits:type:@
initWithCallUUID_digits_type :: (IsCXPlayDTMFCallAction cxPlayDTMFCallAction, IsNSUUID callUUID, IsNSString digits) => cxPlayDTMFCallAction -> callUUID -> digits -> CXPlayDTMFCallActionType -> IO (Id CXPlayDTMFCallAction)
initWithCallUUID_digits_type cxPlayDTMFCallAction  callUUID digits type_ =
withObjCPtr callUUID $ \raw_callUUID ->
  withObjCPtr digits $ \raw_digits ->
      sendMsg cxPlayDTMFCallAction (mkSelector "initWithCallUUID:digits:type:") (retPtr retVoid) [argPtr (castPtr raw_callUUID :: Ptr ()), argPtr (castPtr raw_digits :: Ptr ()), argCLong (coerce type_)] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsCXPlayDTMFCallAction cxPlayDTMFCallAction, IsNSCoder aDecoder) => cxPlayDTMFCallAction -> aDecoder -> IO (Id CXPlayDTMFCallAction)
initWithCoder cxPlayDTMFCallAction  aDecoder =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg cxPlayDTMFCallAction (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCallUUID:@
initWithCallUUID :: (IsCXPlayDTMFCallAction cxPlayDTMFCallAction, IsNSUUID callUUID) => cxPlayDTMFCallAction -> callUUID -> IO (Id CXPlayDTMFCallAction)
initWithCallUUID cxPlayDTMFCallAction  callUUID =
withObjCPtr callUUID $ \raw_callUUID ->
    sendMsg cxPlayDTMFCallAction (mkSelector "initWithCallUUID:") (retPtr retVoid) [argPtr (castPtr raw_callUUID :: Ptr ())] >>= ownedObject . castPtr

-- | @- digits@
digits :: IsCXPlayDTMFCallAction cxPlayDTMFCallAction => cxPlayDTMFCallAction -> IO (Id NSString)
digits cxPlayDTMFCallAction  =
  sendMsg cxPlayDTMFCallAction (mkSelector "digits") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDigits:@
setDigits :: (IsCXPlayDTMFCallAction cxPlayDTMFCallAction, IsNSString value) => cxPlayDTMFCallAction -> value -> IO ()
setDigits cxPlayDTMFCallAction  value =
withObjCPtr value $ \raw_value ->
    sendMsg cxPlayDTMFCallAction (mkSelector "setDigits:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- type@
type_ :: IsCXPlayDTMFCallAction cxPlayDTMFCallAction => cxPlayDTMFCallAction -> IO CXPlayDTMFCallActionType
type_ cxPlayDTMFCallAction  =
  fmap (coerce :: CLong -> CXPlayDTMFCallActionType) $ sendMsg cxPlayDTMFCallAction (mkSelector "type") retCLong []

-- | @- setType:@
setType :: IsCXPlayDTMFCallAction cxPlayDTMFCallAction => cxPlayDTMFCallAction -> CXPlayDTMFCallActionType -> IO ()
setType cxPlayDTMFCallAction  value =
  sendMsg cxPlayDTMFCallAction (mkSelector "setType:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCallUUID:digits:type:@
initWithCallUUID_digits_typeSelector :: Selector
initWithCallUUID_digits_typeSelector = mkSelector "initWithCallUUID:digits:type:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithCallUUID:@
initWithCallUUIDSelector :: Selector
initWithCallUUIDSelector = mkSelector "initWithCallUUID:"

-- | @Selector@ for @digits@
digitsSelector :: Selector
digitsSelector = mkSelector "digits"

-- | @Selector@ for @setDigits:@
setDigitsSelector :: Selector
setDigitsSelector = mkSelector "setDigits:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

