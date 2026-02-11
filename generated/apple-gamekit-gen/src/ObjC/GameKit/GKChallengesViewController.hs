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
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- challengeDelegate@
challengeDelegate :: IsGKChallengesViewController gkChallengesViewController => gkChallengesViewController -> IO RawId
challengeDelegate gkChallengesViewController  =
    fmap (RawId . castPtr) $ sendMsg gkChallengesViewController (mkSelector "challengeDelegate") (retPtr retVoid) []

-- | @- setChallengeDelegate:@
setChallengeDelegate :: IsGKChallengesViewController gkChallengesViewController => gkChallengesViewController -> RawId -> IO ()
setChallengeDelegate gkChallengesViewController  value =
    sendMsg gkChallengesViewController (mkSelector "setChallengeDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @challengeDelegate@
challengeDelegateSelector :: Selector
challengeDelegateSelector = mkSelector "challengeDelegate"

-- | @Selector@ for @setChallengeDelegate:@
setChallengeDelegateSelector :: Selector
setChallengeDelegateSelector = mkSelector "setChallengeDelegate:"

