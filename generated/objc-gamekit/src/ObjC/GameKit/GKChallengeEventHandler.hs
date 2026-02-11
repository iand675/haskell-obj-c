{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A singleton object responsible for dispatching challenge-related events to its delegate
--
-- Generated bindings for @GKChallengeEventHandler@.
module ObjC.GameKit.GKChallengeEventHandler
  ( GKChallengeEventHandler
  , IsGKChallengeEventHandler(..)
  , challengeEventHandler
  , challengeEventHandlerSelector


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

import ObjC.GameKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ challengeEventHandler@
challengeEventHandler :: IO (Id GKChallengeEventHandler)
challengeEventHandler  =
  do
    cls' <- getRequiredClass "GKChallengeEventHandler"
    sendClassMsg cls' (mkSelector "challengeEventHandler") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @challengeEventHandler@
challengeEventHandlerSelector :: Selector
challengeEventHandlerSelector = mkSelector "challengeEventHandler"

