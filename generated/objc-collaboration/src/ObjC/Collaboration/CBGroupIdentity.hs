{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object of the @CBGroupIdentity@ class represents a group identity and is used for viewing the attributes of group identities from an identity authority. The principal attributes of a @CBGroupIdentity@ object are a POSIX group identifier (GID) and a list of members.
--
-- Generated bindings for @CBGroupIdentity@.
module ObjC.Collaboration.CBGroupIdentity
  ( CBGroupIdentity
  , IsCBGroupIdentity(..)
  , groupIdentityWithPosixGID_authority
  , posixGID
  , groupIdentityWithPosixGID_authoritySelector
  , posixGIDSelector


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

import ObjC.Collaboration.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns the group identity with the given POSIX GID in the specified identity authority.
--
-- - Parameters:   - gid: The GID of the group identity you are searching for.   - authority: An identity authority in which to search for the group identity. - Returns: The group identity object with the given GID in the specified identity authority, or @nil@ if no identity exists with the specified GID.
--
-- ## See Also
--
-- - [Identity Services Programming Guide](https://developer.apple.com/library/archive/documentation/Networking/Conceptual/IdentityServices_ProgGuide/Introduction/Introduction.html#//apple_ref/doc/uid/TP40004490)
--
-- ObjC selector: @+ groupIdentityWithPosixGID:authority:@
groupIdentityWithPosixGID_authority :: IsCBIdentityAuthority authority => CUInt -> authority -> IO (Id CBGroupIdentity)
groupIdentityWithPosixGID_authority gid authority =
  do
    cls' <- getRequiredClass "CBGroupIdentity"
    withObjCPtr authority $ \raw_authority ->
      sendClassMsg cls' (mkSelector "groupIdentityWithPosixGID:authority:") (retPtr retVoid) [argCUInt (fromIntegral gid), argPtr (castPtr raw_authority :: Ptr ())] >>= retainedObject . castPtr

-- | Returns the POSIX GID of the identity.
--
-- The POSIX GID is an integer that can identify a group within an identity authority. GIDs are not guaranteed to be unique within an identity authority.
--
-- - Returns: The POSIX GID of the group identity.
--
-- ObjC selector: @- posixGID@
posixGID :: IsCBGroupIdentity cbGroupIdentity => cbGroupIdentity -> IO CUInt
posixGID cbGroupIdentity  =
  sendMsg cbGroupIdentity (mkSelector "posixGID") retCUInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @groupIdentityWithPosixGID:authority:@
groupIdentityWithPosixGID_authoritySelector :: Selector
groupIdentityWithPosixGID_authoritySelector = mkSelector "groupIdentityWithPosixGID:authority:"

-- | @Selector@ for @posixGID@
posixGIDSelector :: Selector
posixGIDSelector = mkSelector "posixGID"

