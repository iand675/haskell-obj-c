{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , allowedParticipantAccessOptionsSelector
  , allowedParticipantPermissionOptionsSelector
  , allowsAccessRequestsSelector
  , allowsParticipantsToInviteOthersSelector
  , initWithAllowedParticipantPermissionOptions_allowedParticipantAccessOptionsSelector
  , setAllowedParticipantAccessOptionsSelector
  , setAllowedParticipantPermissionOptionsSelector
  , setAllowsAccessRequestsSelector
  , setAllowsParticipantsToInviteOthersSelector
  , standardOptionsSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.CloudKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithAllowedParticipantPermissionOptions:allowedParticipantAccessOptions:@
initWithAllowedParticipantPermissionOptions_allowedParticipantAccessOptions :: IsCKAllowedSharingOptions ckAllowedSharingOptions => ckAllowedSharingOptions -> CKSharingParticipantPermissionOption -> CKSharingParticipantAccessOption -> IO (Id CKAllowedSharingOptions)
initWithAllowedParticipantPermissionOptions_allowedParticipantAccessOptions ckAllowedSharingOptions allowedParticipantPermissionOptions allowedParticipantAccessOptions =
  sendOwnedMessage ckAllowedSharingOptions initWithAllowedParticipantPermissionOptions_allowedParticipantAccessOptionsSelector allowedParticipantPermissionOptions allowedParticipantAccessOptions

-- | @- allowedParticipantPermissionOptions@
allowedParticipantPermissionOptions :: IsCKAllowedSharingOptions ckAllowedSharingOptions => ckAllowedSharingOptions -> IO CKSharingParticipantPermissionOption
allowedParticipantPermissionOptions ckAllowedSharingOptions =
  sendMessage ckAllowedSharingOptions allowedParticipantPermissionOptionsSelector

-- | @- setAllowedParticipantPermissionOptions:@
setAllowedParticipantPermissionOptions :: IsCKAllowedSharingOptions ckAllowedSharingOptions => ckAllowedSharingOptions -> CKSharingParticipantPermissionOption -> IO ()
setAllowedParticipantPermissionOptions ckAllowedSharingOptions value =
  sendMessage ckAllowedSharingOptions setAllowedParticipantPermissionOptionsSelector value

-- | @- allowedParticipantAccessOptions@
allowedParticipantAccessOptions :: IsCKAllowedSharingOptions ckAllowedSharingOptions => ckAllowedSharingOptions -> IO CKSharingParticipantAccessOption
allowedParticipantAccessOptions ckAllowedSharingOptions =
  sendMessage ckAllowedSharingOptions allowedParticipantAccessOptionsSelector

-- | @- setAllowedParticipantAccessOptions:@
setAllowedParticipantAccessOptions :: IsCKAllowedSharingOptions ckAllowedSharingOptions => ckAllowedSharingOptions -> CKSharingParticipantAccessOption -> IO ()
setAllowedParticipantAccessOptions ckAllowedSharingOptions value =
  sendMessage ckAllowedSharingOptions setAllowedParticipantAccessOptionsSelector value

-- | Default value is @NO@. If set, the system sharing UI will allow the user to choose whether added participants can invite others to the share. Shares with ``CloudKit/CKShareParticipantRole/CKShareParticipantRoleAdministrator`` participants will be returned as read-only to devices running OS versions prior to this role being introduced. Administrator participants on these read-only shares will be returned as ``CloudKit/CKShareParticipantRole/CKShareParticipantRolePrivateUser``.
--
-- ObjC selector: @- allowsParticipantsToInviteOthers@
allowsParticipantsToInviteOthers :: IsCKAllowedSharingOptions ckAllowedSharingOptions => ckAllowedSharingOptions -> IO Bool
allowsParticipantsToInviteOthers ckAllowedSharingOptions =
  sendMessage ckAllowedSharingOptions allowsParticipantsToInviteOthersSelector

-- | Default value is @NO@. If set, the system sharing UI will allow the user to choose whether added participants can invite others to the share. Shares with ``CloudKit/CKShareParticipantRole/CKShareParticipantRoleAdministrator`` participants will be returned as read-only to devices running OS versions prior to this role being introduced. Administrator participants on these read-only shares will be returned as ``CloudKit/CKShareParticipantRole/CKShareParticipantRolePrivateUser``.
--
-- ObjC selector: @- setAllowsParticipantsToInviteOthers:@
setAllowsParticipantsToInviteOthers :: IsCKAllowedSharingOptions ckAllowedSharingOptions => ckAllowedSharingOptions -> Bool -> IO ()
setAllowsParticipantsToInviteOthers ckAllowedSharingOptions value =
  sendMessage ckAllowedSharingOptions setAllowsParticipantsToInviteOthersSelector value

-- | Standard allowed options are most permissive i.e. @allowedParticipantPermissionOptions@ = @CKSharingParticipantPermissionOptionAny@ and @allowedParticipantAccessOptions@ = @CKSharingParticipantAccessOptionAny@
--
-- ObjC selector: @+ standardOptions@
standardOptions :: IO (Id CKAllowedSharingOptions)
standardOptions  =
  do
    cls' <- getRequiredClass "CKAllowedSharingOptions"
    sendClassMessage cls' standardOptionsSelector

-- | Default value is @NO@. If set, the system sharing UI will allow the user to configure whether access requests are enabled on the share.
--
-- ObjC selector: @- allowsAccessRequests@
allowsAccessRequests :: IsCKAllowedSharingOptions ckAllowedSharingOptions => ckAllowedSharingOptions -> IO Bool
allowsAccessRequests ckAllowedSharingOptions =
  sendMessage ckAllowedSharingOptions allowsAccessRequestsSelector

-- | Default value is @NO@. If set, the system sharing UI will allow the user to configure whether access requests are enabled on the share.
--
-- ObjC selector: @- setAllowsAccessRequests:@
setAllowsAccessRequests :: IsCKAllowedSharingOptions ckAllowedSharingOptions => ckAllowedSharingOptions -> Bool -> IO ()
setAllowsAccessRequests ckAllowedSharingOptions value =
  sendMessage ckAllowedSharingOptions setAllowsAccessRequestsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAllowedParticipantPermissionOptions:allowedParticipantAccessOptions:@
initWithAllowedParticipantPermissionOptions_allowedParticipantAccessOptionsSelector :: Selector '[CKSharingParticipantPermissionOption, CKSharingParticipantAccessOption] (Id CKAllowedSharingOptions)
initWithAllowedParticipantPermissionOptions_allowedParticipantAccessOptionsSelector = mkSelector "initWithAllowedParticipantPermissionOptions:allowedParticipantAccessOptions:"

-- | @Selector@ for @allowedParticipantPermissionOptions@
allowedParticipantPermissionOptionsSelector :: Selector '[] CKSharingParticipantPermissionOption
allowedParticipantPermissionOptionsSelector = mkSelector "allowedParticipantPermissionOptions"

-- | @Selector@ for @setAllowedParticipantPermissionOptions:@
setAllowedParticipantPermissionOptionsSelector :: Selector '[CKSharingParticipantPermissionOption] ()
setAllowedParticipantPermissionOptionsSelector = mkSelector "setAllowedParticipantPermissionOptions:"

-- | @Selector@ for @allowedParticipantAccessOptions@
allowedParticipantAccessOptionsSelector :: Selector '[] CKSharingParticipantAccessOption
allowedParticipantAccessOptionsSelector = mkSelector "allowedParticipantAccessOptions"

-- | @Selector@ for @setAllowedParticipantAccessOptions:@
setAllowedParticipantAccessOptionsSelector :: Selector '[CKSharingParticipantAccessOption] ()
setAllowedParticipantAccessOptionsSelector = mkSelector "setAllowedParticipantAccessOptions:"

-- | @Selector@ for @allowsParticipantsToInviteOthers@
allowsParticipantsToInviteOthersSelector :: Selector '[] Bool
allowsParticipantsToInviteOthersSelector = mkSelector "allowsParticipantsToInviteOthers"

-- | @Selector@ for @setAllowsParticipantsToInviteOthers:@
setAllowsParticipantsToInviteOthersSelector :: Selector '[Bool] ()
setAllowsParticipantsToInviteOthersSelector = mkSelector "setAllowsParticipantsToInviteOthers:"

-- | @Selector@ for @standardOptions@
standardOptionsSelector :: Selector '[] (Id CKAllowedSharingOptions)
standardOptionsSelector = mkSelector "standardOptions"

-- | @Selector@ for @allowsAccessRequests@
allowsAccessRequestsSelector :: Selector '[] Bool
allowsAccessRequestsSelector = mkSelector "allowsAccessRequests"

-- | @Selector@ for @setAllowsAccessRequests:@
setAllowsAccessRequestsSelector :: Selector '[Bool] ()
setAllowsAccessRequestsSelector = mkSelector "setAllowsAccessRequests:"

