{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' openTranscript_intentParametersSelector (toNSString businessIdentifier) (toNSDictionary intentParameters)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @openTranscript:intentParameters:@
openTranscript_intentParametersSelector :: Selector '[Id NSString, Id NSDictionary] ()
openTranscript_intentParametersSelector = mkSelector "openTranscript:intentParameters:"

