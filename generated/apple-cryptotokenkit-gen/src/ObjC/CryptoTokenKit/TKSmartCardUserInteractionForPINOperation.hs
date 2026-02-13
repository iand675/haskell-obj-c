{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , localeSelector
  , pinCompletionSelector
  , pinMessageIndicesSelector
  , resultDataSelector
  , resultSWSelector
  , setLocaleSelector
  , setPINCompletionSelector
  , setPINMessageIndicesSelector
  , setResultDataSelector
  , setResultSWSelector

  -- * Enum types
  , TKSmartCardPINCompletion(TKSmartCardPINCompletion)
  , pattern TKSmartCardPINCompletionMaxLength
  , pattern TKSmartCardPINCompletionKey
  , pattern TKSmartCardPINCompletionTimeout

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
pinCompletion tkSmartCardUserInteractionForPINOperation =
  sendMessage tkSmartCardUserInteractionForPINOperation pinCompletionSelector

-- | Bitmask specifying condition(s) under which PIN entry should be considered complete.
--
-- Note: Default value: TKSmartCardPINCompletionKey
--
-- ObjC selector: @- setPINCompletion:@
setPINCompletion :: IsTKSmartCardUserInteractionForPINOperation tkSmartCardUserInteractionForPINOperation => tkSmartCardUserInteractionForPINOperation -> TKSmartCardPINCompletion -> IO ()
setPINCompletion tkSmartCardUserInteractionForPINOperation value =
  sendMessage tkSmartCardUserInteractionForPINOperation setPINCompletionSelector value

-- | List of message indices referring to a predefined message table. It is used to specify the type and number of messages displayed during the PIN operation.
--
-- If nil, the reader does not display any message (reader specific). Typically, PIN verification takes 1 message, PIN modification 1-3 messages.
--
-- Note: Default value: nil
--
-- ObjC selector: @- PINMessageIndices@
pinMessageIndices :: IsTKSmartCardUserInteractionForPINOperation tkSmartCardUserInteractionForPINOperation => tkSmartCardUserInteractionForPINOperation -> IO (Id NSArray)
pinMessageIndices tkSmartCardUserInteractionForPINOperation =
  sendMessage tkSmartCardUserInteractionForPINOperation pinMessageIndicesSelector

-- | List of message indices referring to a predefined message table. It is used to specify the type and number of messages displayed during the PIN operation.
--
-- If nil, the reader does not display any message (reader specific). Typically, PIN verification takes 1 message, PIN modification 1-3 messages.
--
-- Note: Default value: nil
--
-- ObjC selector: @- setPINMessageIndices:@
setPINMessageIndices :: (IsTKSmartCardUserInteractionForPINOperation tkSmartCardUserInteractionForPINOperation, IsNSArray value) => tkSmartCardUserInteractionForPINOperation -> value -> IO ()
setPINMessageIndices tkSmartCardUserInteractionForPINOperation value =
  sendMessage tkSmartCardUserInteractionForPINOperation setPINMessageIndicesSelector (toNSArray value)

-- | Locale defining the language of displayed messages. If set to nil, the user's current locale is used.
--
-- Note: Default value: the user's current locale
--
-- ObjC selector: @- locale@
locale :: IsTKSmartCardUserInteractionForPINOperation tkSmartCardUserInteractionForPINOperation => tkSmartCardUserInteractionForPINOperation -> IO (Id NSLocale)
locale tkSmartCardUserInteractionForPINOperation =
  sendMessage tkSmartCardUserInteractionForPINOperation localeSelector

-- | Locale defining the language of displayed messages. If set to nil, the user's current locale is used.
--
-- Note: Default value: the user's current locale
--
-- ObjC selector: @- setLocale:@
setLocale :: (IsTKSmartCardUserInteractionForPINOperation tkSmartCardUserInteractionForPINOperation, IsNSLocale value) => tkSmartCardUserInteractionForPINOperation -> value -> IO ()
setLocale tkSmartCardUserInteractionForPINOperation value =
  sendMessage tkSmartCardUserInteractionForPINOperation setLocaleSelector (toNSLocale value)

-- | SW1SW2 result code.
--
-- ObjC selector: @- resultSW@
resultSW :: IsTKSmartCardUserInteractionForPINOperation tkSmartCardUserInteractionForPINOperation => tkSmartCardUserInteractionForPINOperation -> IO CUShort
resultSW tkSmartCardUserInteractionForPINOperation =
  sendMessage tkSmartCardUserInteractionForPINOperation resultSWSelector

-- | SW1SW2 result code.
--
-- ObjC selector: @- setResultSW:@
setResultSW :: IsTKSmartCardUserInteractionForPINOperation tkSmartCardUserInteractionForPINOperation => tkSmartCardUserInteractionForPINOperation -> CUShort -> IO ()
setResultSW tkSmartCardUserInteractionForPINOperation value =
  sendMessage tkSmartCardUserInteractionForPINOperation setResultSWSelector value

-- | Optional block of returned data (without SW1SW2 bytes).
--
-- ObjC selector: @- resultData@
resultData :: IsTKSmartCardUserInteractionForPINOperation tkSmartCardUserInteractionForPINOperation => tkSmartCardUserInteractionForPINOperation -> IO (Id NSData)
resultData tkSmartCardUserInteractionForPINOperation =
  sendMessage tkSmartCardUserInteractionForPINOperation resultDataSelector

-- | Optional block of returned data (without SW1SW2 bytes).
--
-- ObjC selector: @- setResultData:@
setResultData :: (IsTKSmartCardUserInteractionForPINOperation tkSmartCardUserInteractionForPINOperation, IsNSData value) => tkSmartCardUserInteractionForPINOperation -> value -> IO ()
setResultData tkSmartCardUserInteractionForPINOperation value =
  sendMessage tkSmartCardUserInteractionForPINOperation setResultDataSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @PINCompletion@
pinCompletionSelector :: Selector '[] TKSmartCardPINCompletion
pinCompletionSelector = mkSelector "PINCompletion"

-- | @Selector@ for @setPINCompletion:@
setPINCompletionSelector :: Selector '[TKSmartCardPINCompletion] ()
setPINCompletionSelector = mkSelector "setPINCompletion:"

-- | @Selector@ for @PINMessageIndices@
pinMessageIndicesSelector :: Selector '[] (Id NSArray)
pinMessageIndicesSelector = mkSelector "PINMessageIndices"

-- | @Selector@ for @setPINMessageIndices:@
setPINMessageIndicesSelector :: Selector '[Id NSArray] ()
setPINMessageIndicesSelector = mkSelector "setPINMessageIndices:"

-- | @Selector@ for @locale@
localeSelector :: Selector '[] (Id NSLocale)
localeSelector = mkSelector "locale"

-- | @Selector@ for @setLocale:@
setLocaleSelector :: Selector '[Id NSLocale] ()
setLocaleSelector = mkSelector "setLocale:"

-- | @Selector@ for @resultSW@
resultSWSelector :: Selector '[] CUShort
resultSWSelector = mkSelector "resultSW"

-- | @Selector@ for @setResultSW:@
setResultSWSelector :: Selector '[CUShort] ()
setResultSWSelector = mkSelector "setResultSW:"

-- | @Selector@ for @resultData@
resultDataSelector :: Selector '[] (Id NSData)
resultDataSelector = mkSelector "resultData"

-- | @Selector@ for @setResultData:@
setResultDataSelector :: Selector '[Id NSData] ()
setResultDataSelector = mkSelector "setResultData:"

