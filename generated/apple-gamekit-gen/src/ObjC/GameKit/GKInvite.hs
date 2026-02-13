{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | GKInvite represents an accepted game invite, it is used to create a GKMatchmakerViewController
--
-- Generated bindings for @GKInvite@.
module ObjC.GameKit.GKInvite
  ( GKInvite
  , IsGKInvite(..)
  , sender
  , hosted
  , playerGroup
  , playerAttributes
  , inviter
  , hostedSelector
  , inviterSelector
  , playerAttributesSelector
  , playerGroupSelector
  , senderSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- sender@
sender :: IsGKInvite gkInvite => gkInvite -> IO (Id GKPlayer)
sender gkInvite =
  sendMessage gkInvite senderSelector

-- | @- hosted@
hosted :: IsGKInvite gkInvite => gkInvite -> IO Bool
hosted gkInvite =
  sendMessage gkInvite hostedSelector

-- | player group from inviter's match request
--
-- ObjC selector: @- playerGroup@
playerGroup :: IsGKInvite gkInvite => gkInvite -> IO CULong
playerGroup gkInvite =
  sendMessage gkInvite playerGroupSelector

-- | player attributes from inviter's match request
--
-- ObjC selector: @- playerAttributes@
playerAttributes :: IsGKInvite gkInvite => gkInvite -> IO CUInt
playerAttributes gkInvite =
  sendMessage gkInvite playerAttributesSelector

-- | * This property is obsolete. **
--
-- ObjC selector: @- inviter@
inviter :: IsGKInvite gkInvite => gkInvite -> IO (Id NSString)
inviter gkInvite =
  sendMessage gkInvite inviterSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sender@
senderSelector :: Selector '[] (Id GKPlayer)
senderSelector = mkSelector "sender"

-- | @Selector@ for @hosted@
hostedSelector :: Selector '[] Bool
hostedSelector = mkSelector "hosted"

-- | @Selector@ for @playerGroup@
playerGroupSelector :: Selector '[] CULong
playerGroupSelector = mkSelector "playerGroup"

-- | @Selector@ for @playerAttributes@
playerAttributesSelector :: Selector '[] CUInt
playerAttributesSelector = mkSelector "playerAttributes"

-- | @Selector@ for @inviter@
inviterSelector :: Selector '[] (Id NSString)
inviterSelector = mkSelector "inviter"

