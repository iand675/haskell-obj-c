{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | User interaction for the secure PIN operations on the SmartCard reader.
--
-- Note: Result is available after the interaction has been successfully completed.
--
-- Generated bindings for @TKSmartCardUserInteractionForPINOperation@.
module ObjC.CryptoTokenKit.TKSmartCardUserInteractionForPINOperation
  ( TKSmartCardUserInteractionForPINOperation
  , IsTKSmartCardUserInteractionForPINOperation(..)
  , pinCompletion
  , setPINCompletion
  , pinMessageIndices
  , setPINMessageIndices
  , locale
  , setLocale
  , resultSW
  , setResultSW
  , resultData
  , setResultData
  , pinCompletionSelector
  , setPINCompletionSelector
  , pinMessageIndicesSelector
  , setPINMessageIndicesSelector
  , localeSelector
  , setLocaleSelector
  , resultSWSelector
  , setResultSWSelector
  , resultDataSelector
  , setResultDataSelector

  -- * Enum types
  , TKSmartCardPINCompletion(TKSmartCardPINCompletion)
  , pattern TKSmartCardPINCompletionMaxLength
  , pattern TKSmartCardPINCompletionKey
  , pattern TKSmartCardPINCompletionTimeout

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

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.CryptoTokenKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Bitmask specifying condition(s) under which PIN entry should be considered complete.
--
-- Note: Default value: TKSmartCardPINCompletionKey
--
-- ObjC selector: @- PINCompletion@
pinCompletion :: IsTKSmartCardUserInteractionForPINOperation tkSmartCardUserInteractionForPINOperation => tkSmartCardUserInteractionForPINOperation -> IO TKSmartCardPINCompletion
pinCompletion tkSmartCardUserInteractionForPINOperation  =
  fmap (coerce :: CULong -> TKSmartCardPINCompletion) $ sendMsg tkSmartCardUserInteractionForPINOperation (mkSelector "PINCompletion") retCULong []

-- | Bitmask specifying condition(s) under which PIN entry should be considered complete.
--
-- Note: Default value: TKSmartCardPINCompletionKey
--
-- ObjC selector: @- setPINCompletion:@
setPINCompletion :: IsTKSmartCardUserInteractionForPINOperation tkSmartCardUserInteractionForPINOperation => tkSmartCardUserInteractionForPINOperation -> TKSmartCardPINCompletion -> IO ()
setPINCompletion tkSmartCardUserInteractionForPINOperation  value =
  sendMsg tkSmartCardUserInteractionForPINOperation (mkSelector "setPINCompletion:") retVoid [argCULong (coerce value)]

-- | List of message indices referring to a predefined message table. It is used to specify the type and number of messages displayed during the PIN operation.
--
-- If nil, the reader does not display any message (reader specific). Typically, PIN verification takes 1 message, PIN modification 1-3 messages.
--
-- Note: Default value: nil
--
-- ObjC selector: @- PINMessageIndices@
pinMessageIndices :: IsTKSmartCardUserInteractionForPINOperation tkSmartCardUserInteractionForPINOperation => tkSmartCardUserInteractionForPINOperation -> IO (Id NSArray)
pinMessageIndices tkSmartCardUserInteractionForPINOperation  =
  sendMsg tkSmartCardUserInteractionForPINOperation (mkSelector "PINMessageIndices") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | List of message indices referring to a predefined message table. It is used to specify the type and number of messages displayed during the PIN operation.
--
-- If nil, the reader does not display any message (reader specific). Typically, PIN verification takes 1 message, PIN modification 1-3 messages.
--
-- Note: Default value: nil
--
-- ObjC selector: @- setPINMessageIndices:@
setPINMessageIndices :: (IsTKSmartCardUserInteractionForPINOperation tkSmartCardUserInteractionForPINOperation, IsNSArray value) => tkSmartCardUserInteractionForPINOperation -> value -> IO ()
setPINMessageIndices tkSmartCardUserInteractionForPINOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg tkSmartCardUserInteractionForPINOperation (mkSelector "setPINMessageIndices:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Locale defining the language of displayed messages. If set to nil, the user's current locale is used.
--
-- Note: Default value: the user's current locale
--
-- ObjC selector: @- locale@
locale :: IsTKSmartCardUserInteractionForPINOperation tkSmartCardUserInteractionForPINOperation => tkSmartCardUserInteractionForPINOperation -> IO (Id NSLocale)
locale tkSmartCardUserInteractionForPINOperation  =
  sendMsg tkSmartCardUserInteractionForPINOperation (mkSelector "locale") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Locale defining the language of displayed messages. If set to nil, the user's current locale is used.
--
-- Note: Default value: the user's current locale
--
-- ObjC selector: @- setLocale:@
setLocale :: (IsTKSmartCardUserInteractionForPINOperation tkSmartCardUserInteractionForPINOperation, IsNSLocale value) => tkSmartCardUserInteractionForPINOperation -> value -> IO ()
setLocale tkSmartCardUserInteractionForPINOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg tkSmartCardUserInteractionForPINOperation (mkSelector "setLocale:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | SW1SW2 result code.
--
-- ObjC selector: @- resultSW@
resultSW :: IsTKSmartCardUserInteractionForPINOperation tkSmartCardUserInteractionForPINOperation => tkSmartCardUserInteractionForPINOperation -> IO CUShort
resultSW tkSmartCardUserInteractionForPINOperation  =
  fmap fromIntegral $ sendMsg tkSmartCardUserInteractionForPINOperation (mkSelector "resultSW") retCUInt []

-- | SW1SW2 result code.
--
-- ObjC selector: @- setResultSW:@
setResultSW :: IsTKSmartCardUserInteractionForPINOperation tkSmartCardUserInteractionForPINOperation => tkSmartCardUserInteractionForPINOperation -> CUShort -> IO ()
setResultSW tkSmartCardUserInteractionForPINOperation  value =
  sendMsg tkSmartCardUserInteractionForPINOperation (mkSelector "setResultSW:") retVoid [argCUInt (fromIntegral value)]

-- | Optional block of returned data (without SW1SW2 bytes).
--
-- ObjC selector: @- resultData@
resultData :: IsTKSmartCardUserInteractionForPINOperation tkSmartCardUserInteractionForPINOperation => tkSmartCardUserInteractionForPINOperation -> IO (Id NSData)
resultData tkSmartCardUserInteractionForPINOperation  =
  sendMsg tkSmartCardUserInteractionForPINOperation (mkSelector "resultData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Optional block of returned data (without SW1SW2 bytes).
--
-- ObjC selector: @- setResultData:@
setResultData :: (IsTKSmartCardUserInteractionForPINOperation tkSmartCardUserInteractionForPINOperation, IsNSData value) => tkSmartCardUserInteractionForPINOperation -> value -> IO ()
setResultData tkSmartCardUserInteractionForPINOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg tkSmartCardUserInteractionForPINOperation (mkSelector "setResultData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @PINCompletion@
pinCompletionSelector :: Selector
pinCompletionSelector = mkSelector "PINCompletion"

-- | @Selector@ for @setPINCompletion:@
setPINCompletionSelector :: Selector
setPINCompletionSelector = mkSelector "setPINCompletion:"

-- | @Selector@ for @PINMessageIndices@
pinMessageIndicesSelector :: Selector
pinMessageIndicesSelector = mkSelector "PINMessageIndices"

-- | @Selector@ for @setPINMessageIndices:@
setPINMessageIndicesSelector :: Selector
setPINMessageIndicesSelector = mkSelector "setPINMessageIndices:"

-- | @Selector@ for @locale@
localeSelector :: Selector
localeSelector = mkSelector "locale"

-- | @Selector@ for @setLocale:@
setLocaleSelector :: Selector
setLocaleSelector = mkSelector "setLocale:"

-- | @Selector@ for @resultSW@
resultSWSelector :: Selector
resultSWSelector = mkSelector "resultSW"

-- | @Selector@ for @setResultSW:@
setResultSWSelector :: Selector
setResultSWSelector = mkSelector "setResultSW:"

-- | @Selector@ for @resultData@
resultDataSelector :: Selector
resultDataSelector = mkSelector "resultData"

-- | @Selector@ for @setResultData:@
setResultDataSelector :: Selector
setResultDataSelector = mkSelector "setResultData:"

