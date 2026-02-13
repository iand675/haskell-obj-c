{-# LANGUAGE DataKinds #-}
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
  , dictionaryPayloadSelector
  , typeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
type_ pkPushPayload =
  sendMessage pkPushPayload typeSelector

-- | The contents of the received payload.
--
-- For VoIP pushes, the sender is free to specify any fields for the contained data as long as it is provided in a text-encodable JSON format.
--
-- ObjC selector: @- dictionaryPayload@
dictionaryPayload :: IsPKPushPayload pkPushPayload => pkPushPayload -> IO (Id NSDictionary)
dictionaryPayload pkPushPayload =
  sendMessage pkPushPayload dictionaryPayloadSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSString)
typeSelector = mkSelector "type"

-- | @Selector@ for @dictionaryPayload@
dictionaryPayloadSelector :: Selector '[] (Id NSDictionary)
dictionaryPayloadSelector = mkSelector "dictionaryPayload"

