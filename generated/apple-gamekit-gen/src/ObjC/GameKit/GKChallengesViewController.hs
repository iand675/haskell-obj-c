{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GKChallengesViewController@.
module ObjC.GameKit.GKChallengesViewController
  ( GKChallengesViewController
  , IsGKChallengesViewController(..)
  , challengeDelegate
  , setChallengeDelegate
  , challengeDelegateSelector
  , setChallengeDelegateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- challengeDelegate@
challengeDelegate :: IsGKChallengesViewController gkChallengesViewController => gkChallengesViewController -> IO RawId
challengeDelegate gkChallengesViewController =
  sendMessage gkChallengesViewController challengeDelegateSelector

-- | @- setChallengeDelegate:@
setChallengeDelegate :: IsGKChallengesViewController gkChallengesViewController => gkChallengesViewController -> RawId -> IO ()
setChallengeDelegate gkChallengesViewController value =
  sendMessage gkChallengesViewController setChallengeDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @challengeDelegate@
challengeDelegateSelector :: Selector '[] RawId
challengeDelegateSelector = mkSelector "challengeDelegate"

-- | @Selector@ for @setChallengeDelegate:@
setChallengeDelegateSelector :: Selector '[RawId] ()
setChallengeDelegateSelector = mkSelector "setChallengeDelegate:"

