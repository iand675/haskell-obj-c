{-# LANGUAGE DataKinds #-}
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
  , delegate
  , setDelegate
  , challengeEventHandlerSelector
  , delegateSelector
  , setDelegateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ challengeEventHandler@
challengeEventHandler :: IO (Id GKChallengeEventHandler)
challengeEventHandler  =
  do
    cls' <- getRequiredClass "GKChallengeEventHandler"
    sendClassMessage cls' challengeEventHandlerSelector

-- | @- delegate@
delegate :: IsGKChallengeEventHandler gkChallengeEventHandler => gkChallengeEventHandler -> IO RawId
delegate gkChallengeEventHandler =
  sendMessage gkChallengeEventHandler delegateSelector

-- | @- setDelegate:@
setDelegate :: IsGKChallengeEventHandler gkChallengeEventHandler => gkChallengeEventHandler -> RawId -> IO ()
setDelegate gkChallengeEventHandler value =
  sendMessage gkChallengeEventHandler setDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @challengeEventHandler@
challengeEventHandlerSelector :: Selector '[] (Id GKChallengeEventHandler)
challengeEventHandlerSelector = mkSelector "challengeEventHandler"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

