{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKAllowedSharingOptions@.
module ObjC.CloudKit.CKAllowedSharingOptions
  ( CKAllowedSharingOptions
  , IsCKAllowedSharingOptions(..)
  , initWithAllowedParticipantPermissionOptions_allowedParticipantAccessOptions
  , allowedParticipantPermissionOptions
  , setAllowedParticipantPermissionOptions
  , allowedParticipantAccessOptions
  , setAllowedParticipantAccessOptions
  , allowsParticipantsToInviteOthers
  , setAllowsParticipantsToInviteOthers
  , standardOptions
  , allowsAccessRequests
  , setAllowsAccessRequests
  , initWithAllowedParticipantPermissionOptions_allowedParticipantAccessOptionsSelector
  , allowedParticipantPermissionOptionsSelector
  , setAllowedParticipantPermissionOptionsSelector
  , allowedParticipantAccessOptionsSelector
  , setAllowedParticipantAccessOptionsSelector
  , allowsParticipantsToInviteOthersSelector
  , setAllowsParticipantsToInviteOthersSelector
  , standardOptionsSelector
  , allowsAccessRequestsSelector
  , setAllowsAccessRequestsSelector

  -- * Enum types
  , CKSharingParticipantAccessOption(CKSharingParticipantAccessOption)
  , pattern CKSharingParticipantAccessOptionAnyoneWithLink
  , pattern CKSharingParticipantAccessOptionSpecifiedRecipientsOnly
  , pattern CKSharingParticipantAccessOptionAny
  , CKSharingParticipantPermissionOption(CKSharingParticipantPermissionOption)
  , pattern CKSharingParticipantPermissionOptionReadOnly
  , pattern CKSharingParticipantPermissionOptionReadWrite
  , pattern CKSharingParticipantPermissionOptionAny

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

import ObjC.CloudKit.Internal.Classes
import ObjC.CloudKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithAllowedParticipantPermissionOptions:allowedParticipantAccessOptions:@
initWithAllowedParticipantPermissionOptions_allowedParticipantAccessOptions :: IsCKAllowedSharingOptions ckAllowedSharingOptions => ckAllowedSharingOptions -> CKSharingParticipantPermissionOption -> CKSharingParticipantAccessOption -> IO (Id CKAllowedSharingOptions)
initWithAllowedParticipantPermissionOptions_allowedParticipantAccessOptions ckAllowedSharingOptions  allowedParticipantPermissionOptions allowedParticipantAccessOptions =
  sendMsg ckAllowedSharingOptions (mkSelector "initWithAllowedParticipantPermissionOptions:allowedParticipantAccessOptions:") (retPtr retVoid) [argCULong (coerce allowedParticipantPermissionOptions), argCULong (coerce allowedParticipantAccessOptions)] >>= ownedObject . castPtr

-- | @- allowedParticipantPermissionOptions@
allowedParticipantPermissionOptions :: IsCKAllowedSharingOptions ckAllowedSharingOptions => ckAllowedSharingOptions -> IO CKSharingParticipantPermissionOption
allowedParticipantPermissionOptions ckAllowedSharingOptions  =
  fmap (coerce :: CULong -> CKSharingParticipantPermissionOption) $ sendMsg ckAllowedSharingOptions (mkSelector "allowedParticipantPermissionOptions") retCULong []

-- | @- setAllowedParticipantPermissionOptions:@
setAllowedParticipantPermissionOptions :: IsCKAllowedSharingOptions ckAllowedSharingOptions => ckAllowedSharingOptions -> CKSharingParticipantPermissionOption -> IO ()
setAllowedParticipantPermissionOptions ckAllowedSharingOptions  value =
  sendMsg ckAllowedSharingOptions (mkSelector "setAllowedParticipantPermissionOptions:") retVoid [argCULong (coerce value)]

-- | @- allowedParticipantAccessOptions@
allowedParticipantAccessOptions :: IsCKAllowedSharingOptions ckAllowedSharingOptions => ckAllowedSharingOptions -> IO CKSharingParticipantAccessOption
allowedParticipantAccessOptions ckAllowedSharingOptions  =
  fmap (coerce :: CULong -> CKSharingParticipantAccessOption) $ sendMsg ckAllowedSharingOptions (mkSelector "allowedParticipantAccessOptions") retCULong []

-- | @- setAllowedParticipantAccessOptions:@
setAllowedParticipantAccessOptions :: IsCKAllowedSharingOptions ckAllowedSharingOptions => ckAllowedSharingOptions -> CKSharingParticipantAccessOption -> IO ()
setAllowedParticipantAccessOptions ckAllowedSharingOptions  value =
  sendMsg ckAllowedSharingOptions (mkSelector "setAllowedParticipantAccessOptions:") retVoid [argCULong (coerce value)]

-- | Default value is @NO@. If set, the system sharing UI will allow the user to choose whether added participants can invite others to the share. Shares with ``CloudKit/CKShareParticipantRole/CKShareParticipantRoleAdministrator`` participants will be returned as read-only to devices running OS versions prior to this role being introduced. Administrator participants on these read-only shares will be returned as ``CloudKit/CKShareParticipantRole/CKShareParticipantRolePrivateUser``.
--
-- ObjC selector: @- allowsParticipantsToInviteOthers@
allowsParticipantsToInviteOthers :: IsCKAllowedSharingOptions ckAllowedSharingOptions => ckAllowedSharingOptions -> IO Bool
allowsParticipantsToInviteOthers ckAllowedSharingOptions  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ckAllowedSharingOptions (mkSelector "allowsParticipantsToInviteOthers") retCULong []

-- | Default value is @NO@. If set, the system sharing UI will allow the user to choose whether added participants can invite others to the share. Shares with ``CloudKit/CKShareParticipantRole/CKShareParticipantRoleAdministrator`` participants will be returned as read-only to devices running OS versions prior to this role being introduced. Administrator participants on these read-only shares will be returned as ``CloudKit/CKShareParticipantRole/CKShareParticipantRolePrivateUser``.
--
-- ObjC selector: @- setAllowsParticipantsToInviteOthers:@
setAllowsParticipantsToInviteOthers :: IsCKAllowedSharingOptions ckAllowedSharingOptions => ckAllowedSharingOptions -> Bool -> IO ()
setAllowsParticipantsToInviteOthers ckAllowedSharingOptions  value =
  sendMsg ckAllowedSharingOptions (mkSelector "setAllowsParticipantsToInviteOthers:") retVoid [argCULong (if value then 1 else 0)]

-- | Standard allowed options are most permissive i.e. @allowedParticipantPermissionOptions@ = @CKSharingParticipantPermissionOptionAny@ and @allowedParticipantAccessOptions@ = @CKSharingParticipantAccessOptionAny@
--
-- ObjC selector: @+ standardOptions@
standardOptions :: IO (Id CKAllowedSharingOptions)
standardOptions  =
  do
    cls' <- getRequiredClass "CKAllowedSharingOptions"
    sendClassMsg cls' (mkSelector "standardOptions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Default value is @NO@. If set, the system sharing UI will allow the user to configure whether access requests are enabled on the share.
--
-- ObjC selector: @- allowsAccessRequests@
allowsAccessRequests :: IsCKAllowedSharingOptions ckAllowedSharingOptions => ckAllowedSharingOptions -> IO Bool
allowsAccessRequests ckAllowedSharingOptions  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ckAllowedSharingOptions (mkSelector "allowsAccessRequests") retCULong []

-- | Default value is @NO@. If set, the system sharing UI will allow the user to configure whether access requests are enabled on the share.
--
-- ObjC selector: @- setAllowsAccessRequests:@
setAllowsAccessRequests :: IsCKAllowedSharingOptions ckAllowedSharingOptions => ckAllowedSharingOptions -> Bool -> IO ()
setAllowsAccessRequests ckAllowedSharingOptions  value =
  sendMsg ckAllowedSharingOptions (mkSelector "setAllowsAccessRequests:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAllowedParticipantPermissionOptions:allowedParticipantAccessOptions:@
initWithAllowedParticipantPermissionOptions_allowedParticipantAccessOptionsSelector :: Selector
initWithAllowedParticipantPermissionOptions_allowedParticipantAccessOptionsSelector = mkSelector "initWithAllowedParticipantPermissionOptions:allowedParticipantAccessOptions:"

-- | @Selector@ for @allowedParticipantPermissionOptions@
allowedParticipantPermissionOptionsSelector :: Selector
allowedParticipantPermissionOptionsSelector = mkSelector "allowedParticipantPermissionOptions"

-- | @Selector@ for @setAllowedParticipantPermissionOptions:@
setAllowedParticipantPermissionOptionsSelector :: Selector
setAllowedParticipantPermissionOptionsSelector = mkSelector "setAllowedParticipantPermissionOptions:"

-- | @Selector@ for @allowedParticipantAccessOptions@
allowedParticipantAccessOptionsSelector :: Selector
allowedParticipantAccessOptionsSelector = mkSelector "allowedParticipantAccessOptions"

-- | @Selector@ for @setAllowedParticipantAccessOptions:@
setAllowedParticipantAccessOptionsSelector :: Selector
setAllowedParticipantAccessOptionsSelector = mkSelector "setAllowedParticipantAccessOptions:"

-- | @Selector@ for @allowsParticipantsToInviteOthers@
allowsParticipantsToInviteOthersSelector :: Selector
allowsParticipantsToInviteOthersSelector = mkSelector "allowsParticipantsToInviteOthers"

-- | @Selector@ for @setAllowsParticipantsToInviteOthers:@
setAllowsParticipantsToInviteOthersSelector :: Selector
setAllowsParticipantsToInviteOthersSelector = mkSelector "setAllowsParticipantsToInviteOthers:"

-- | @Selector@ for @standardOptions@
standardOptionsSelector :: Selector
standardOptionsSelector = mkSelector "standardOptions"

-- | @Selector@ for @allowsAccessRequests@
allowsAccessRequestsSelector :: Selector
allowsAccessRequestsSelector = mkSelector "allowsAccessRequests"

-- | @Selector@ for @setAllowsAccessRequests:@
setAllowsAccessRequestsSelector :: Selector
setAllowsAccessRequestsSelector = mkSelector "setAllowsAccessRequests:"

