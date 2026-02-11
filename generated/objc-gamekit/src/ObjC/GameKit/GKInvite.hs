{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | GKInvite represents an accepted game invite, it is used to create a GKMatchmakerViewController
--
-- Generated bindings for @GKInvite@.
module ObjC.GameKit.GKInvite
  ( GKInvite
  , IsGKInvite(..)
  , hosted
  , playerGroup
  , playerAttributes
  , inviter
  , hostedSelector
  , playerGroupSelector
  , playerAttributesSelector
  , inviterSelector


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

-- | @- hosted@
hosted :: IsGKInvite gkInvite => gkInvite -> IO Bool
hosted gkInvite  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkInvite (mkSelector "hosted") retCULong []

-- | player group from inviter's match request
--
-- ObjC selector: @- playerGroup@
playerGroup :: IsGKInvite gkInvite => gkInvite -> IO CULong
playerGroup gkInvite  =
  sendMsg gkInvite (mkSelector "playerGroup") retCULong []

-- | player attributes from inviter's match request
--
-- ObjC selector: @- playerAttributes@
playerAttributes :: IsGKInvite gkInvite => gkInvite -> IO CUInt
playerAttributes gkInvite  =
  sendMsg gkInvite (mkSelector "playerAttributes") retCUInt []

-- | * This property is obsolete. **
--
-- ObjC selector: @- inviter@
inviter :: IsGKInvite gkInvite => gkInvite -> IO (Id NSString)
inviter gkInvite  =
  sendMsg gkInvite (mkSelector "inviter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @hosted@
hostedSelector :: Selector
hostedSelector = mkSelector "hosted"

-- | @Selector@ for @playerGroup@
playerGroupSelector :: Selector
playerGroupSelector = mkSelector "playerGroup"

-- | @Selector@ for @playerAttributes@
playerAttributesSelector :: Selector
playerAttributesSelector = mkSelector "playerAttributes"

-- | @Selector@ for @inviter@
inviterSelector :: Selector
inviterSelector = mkSelector "inviter"

