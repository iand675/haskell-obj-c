{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that contains information about a received PushKit notification.
--
-- ## Topics
--
-- ### Payload Data
--
-- - ``PushKit/PKPushPayload/dictionaryPayload`` - ``PushKit/PKPushPayload/type``
--
-- Generated bindings for @PKPushPayload@.
module ObjC.PushKit.PKPushPayload
  ( PKPushPayload
  , IsPKPushPayload(..)
  , type_
  , dictionaryPayload
  , typeSelector
  , dictionaryPayloadSelector


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

import ObjC.PushKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The type value indicating how to interpret the payload.
--
-- For possible values, see ``PushKit/PKPushType``.
--
-- ObjC selector: @- type@
type_ :: IsPKPushPayload pkPushPayload => pkPushPayload -> IO (Id NSString)
type_ pkPushPayload  =
  sendMsg pkPushPayload (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The contents of the received payload.
--
-- For VoIP pushes, the sender is free to specify any fields for the contained data as long as it is provided in a text-encodable JSON format.
--
-- ObjC selector: @- dictionaryPayload@
dictionaryPayload :: IsPKPushPayload pkPushPayload => pkPushPayload -> IO (Id NSDictionary)
dictionaryPayload pkPushPayload  =
  sendMsg pkPushPayload (mkSelector "dictionaryPayload") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @dictionaryPayload@
dictionaryPayloadSelector :: Selector
dictionaryPayloadSelector = mkSelector "dictionaryPayload"

