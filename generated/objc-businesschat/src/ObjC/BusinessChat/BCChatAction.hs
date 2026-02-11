{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @BCChatAction@.
module ObjC.BusinessChat.BCChatAction
  ( BCChatAction
  , IsBCChatAction(..)
  , openTranscript_intentParameters
  , openTranscript_intentParametersSelector


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

import ObjC.BusinessChat.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Open the chat transcript configured for a given business.
--
-- @businessIdentifier@ — The business identifier for the given business.
--
-- @intentParameters@ — Parameters to be sent with the initial message.
--
-- ObjC selector: @+ openTranscript:intentParameters:@
openTranscript_intentParameters :: (IsNSString businessIdentifier, IsNSDictionary intentParameters) => businessIdentifier -> intentParameters -> IO ()
openTranscript_intentParameters businessIdentifier intentParameters =
  do
    cls' <- getRequiredClass "BCChatAction"
    withObjCPtr businessIdentifier $ \raw_businessIdentifier ->
      withObjCPtr intentParameters $ \raw_intentParameters ->
        sendClassMsg cls' (mkSelector "openTranscript:intentParameters:") retVoid [argPtr (castPtr raw_businessIdentifier :: Ptr ()), argPtr (castPtr raw_intentParameters :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @openTranscript:intentParameters:@
openTranscript_intentParametersSelector :: Selector
openTranscript_intentParametersSelector = mkSelector "openTranscript:intentParameters:"

