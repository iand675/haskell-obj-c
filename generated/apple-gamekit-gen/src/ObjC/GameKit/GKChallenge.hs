{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GKChallenge@.
module ObjC.GameKit.GKChallenge
  ( GKChallenge
  , IsGKChallenge(..)
  , decline
  , issuingPlayer
  , receivingPlayer
  , state
  , issueDate
  , completionDate
  , message
  , issuingPlayerID
  , receivingPlayerID
  , declineSelector
  , issuingPlayerSelector
  , receivingPlayerSelector
  , stateSelector
  , issueDateSelector
  , completionDateSelector
  , messageSelector
  , issuingPlayerIDSelector
  , receivingPlayerIDSelector

  -- * Enum types
  , GKChallengeState(GKChallengeState)
  , pattern GKChallengeStateInvalid
  , pattern GKChallengeStatePending
  , pattern GKChallengeStateCompleted
  , pattern GKChallengeStateDeclined

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
import ObjC.GameKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Any GKChallenge object to be declined must be in a state of GKChallengeStatePending in order to be successfully cancelled
--
-- ObjC selector: @- decline@
decline :: IsGKChallenge gkChallenge => gkChallenge -> IO ()
decline gkChallenge  =
    sendMsg gkChallenge (mkSelector "decline") retVoid []

-- | The GKPlayer who issued the challenge
--
-- ObjC selector: @- issuingPlayer@
issuingPlayer :: IsGKChallenge gkChallenge => gkChallenge -> IO (Id GKPlayer)
issuingPlayer gkChallenge  =
    sendMsg gkChallenge (mkSelector "issuingPlayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The GKPlayer who has received the challenge
--
-- ObjC selector: @- receivingPlayer@
receivingPlayer :: IsGKChallenge gkChallenge => gkChallenge -> IO (Id GKPlayer)
receivingPlayer gkChallenge  =
    sendMsg gkChallenge (mkSelector "receivingPlayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Current state of the challenge
--
-- ObjC selector: @- state@
state :: IsGKChallenge gkChallenge => gkChallenge -> IO GKChallengeState
state gkChallenge  =
    fmap (coerce :: CLong -> GKChallengeState) $ sendMsg gkChallenge (mkSelector "state") retCLong []

-- | Date the challenge was issued
--
-- ObjC selector: @- issueDate@
issueDate :: IsGKChallenge gkChallenge => gkChallenge -> IO (Id NSDate)
issueDate gkChallenge  =
    sendMsg gkChallenge (mkSelector "issueDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Date the challenge was completed or aborted
--
-- ObjC selector: @- completionDate@
completionDate :: IsGKChallenge gkChallenge => gkChallenge -> IO (Id NSDate)
completionDate gkChallenge  =
    sendMsg gkChallenge (mkSelector "completionDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The message sent to receivers of this challenge
--
-- ObjC selector: @- message@
message :: IsGKChallenge gkChallenge => gkChallenge -> IO (Id NSString)
message gkChallenge  =
    sendMsg gkChallenge (mkSelector "message") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | * This property is obsolete. **
--
-- ObjC selector: @- issuingPlayerID@
issuingPlayerID :: IsGKChallenge gkChallenge => gkChallenge -> IO (Id NSString)
issuingPlayerID gkChallenge  =
    sendMsg gkChallenge (mkSelector "issuingPlayerID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | * This property is obsolete. **
--
-- ObjC selector: @- receivingPlayerID@
receivingPlayerID :: IsGKChallenge gkChallenge => gkChallenge -> IO (Id NSString)
receivingPlayerID gkChallenge  =
    sendMsg gkChallenge (mkSelector "receivingPlayerID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @decline@
declineSelector :: Selector
declineSelector = mkSelector "decline"

-- | @Selector@ for @issuingPlayer@
issuingPlayerSelector :: Selector
issuingPlayerSelector = mkSelector "issuingPlayer"

-- | @Selector@ for @receivingPlayer@
receivingPlayerSelector :: Selector
receivingPlayerSelector = mkSelector "receivingPlayer"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @issueDate@
issueDateSelector :: Selector
issueDateSelector = mkSelector "issueDate"

-- | @Selector@ for @completionDate@
completionDateSelector :: Selector
completionDateSelector = mkSelector "completionDate"

-- | @Selector@ for @message@
messageSelector :: Selector
messageSelector = mkSelector "message"

-- | @Selector@ for @issuingPlayerID@
issuingPlayerIDSelector :: Selector
issuingPlayerIDSelector = mkSelector "issuingPlayerID"

-- | @Selector@ for @receivingPlayerID@
receivingPlayerIDSelector :: Selector
receivingPlayerIDSelector = mkSelector "receivingPlayerID"

