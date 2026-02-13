{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SFAuthorizationView@.
module ObjC.SecurityInterface.SFAuthorizationView
  ( SFAuthorizationView
  , IsSFAuthorizationView(..)
  , setString
  , setAuthorizationRights
  , authorizationRights
  , authorization
  , updateStatus
  , setAutoupdate
  , setAutoupdate_interval
  , setEnabled
  , isEnabled
  , setFlags
  , setDelegate
  , delegate
  , authorize
  , deauthorize
  , authorizationRightsSelector
  , authorizationSelector
  , authorizeSelector
  , deauthorizeSelector
  , delegateSelector
  , isEnabledSelector
  , setAuthorizationRightsSelector
  , setAutoupdateSelector
  , setAutoupdate_intervalSelector
  , setDelegateSelector
  , setEnabledSelector
  , setFlagsSelector
  , setStringSelector
  , updateStatusSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SecurityInterface.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.SecurityFoundation.Internal.Classes

-- | setString:
--
-- A convenience method to specify an authorization rights set containing a single item with the name set to the specified string.
--
-- @authorizationString@ — Authorization string.
--
-- ObjC selector: @- setString:@
setString :: IsSFAuthorizationView sfAuthorizationView => sfAuthorizationView -> RawId -> IO ()
setString sfAuthorizationView authorizationString =
  sendMessage sfAuthorizationView setStringSelector authorizationString

-- | setAuthorizationRights:
--
-- Sets the authorization rights for this view.
--
-- @authorizationRights@ — Authorization rights.
--
-- ObjC selector: @- setAuthorizationRights:@
setAuthorizationRights :: IsSFAuthorizationView sfAuthorizationView => sfAuthorizationView -> Const RawId -> IO ()
setAuthorizationRights sfAuthorizationView authorizationRights =
  sendMessage sfAuthorizationView setAuthorizationRightsSelector authorizationRights

-- | authorizationRights
--
-- Returns the authorization rights for this view.
--
-- ObjC selector: @- authorizationRights@
authorizationRights :: IsSFAuthorizationView sfAuthorizationView => sfAuthorizationView -> IO RawId
authorizationRights sfAuthorizationView =
  sendMessage sfAuthorizationView authorizationRightsSelector

-- | authorization
--
-- Returns the authorization object associated with this view.
--
-- ObjC selector: @- authorization@
authorization :: IsSFAuthorizationView sfAuthorizationView => sfAuthorizationView -> IO (Id SFAuthorization)
authorization sfAuthorizationView =
  sendMessage sfAuthorizationView authorizationSelector

-- | updateStatus:
--
-- This method is called when the state of the authorization object has changed.
--
-- @inSender@ — The action that is marked for updateStatus.
--
-- ObjC selector: @- updateStatus:@
updateStatus :: IsSFAuthorizationView sfAuthorizationView => sfAuthorizationView -> RawId -> IO Bool
updateStatus sfAuthorizationView inSender =
  sendMessage sfAuthorizationView updateStatusSelector inSender

-- | setAutoupdate:
--
-- ObjC selector: @- setAutoupdate:@
setAutoupdate :: IsSFAuthorizationView sfAuthorizationView => sfAuthorizationView -> Bool -> IO ()
setAutoupdate sfAuthorizationView autoupdate =
  sendMessage sfAuthorizationView setAutoupdateSelector autoupdate

-- | setAutoUpdate:interval:
--
-- ObjC selector: @- setAutoupdate:interval:@
setAutoupdate_interval :: IsSFAuthorizationView sfAuthorizationView => sfAuthorizationView -> Bool -> CDouble -> IO ()
setAutoupdate_interval sfAuthorizationView autoupdate interval =
  sendMessage sfAuthorizationView setAutoupdate_intervalSelector autoupdate interval

-- | setEnabled:
--
-- Sets the current state of the authorization view.
--
-- @enabled@ — Enable flag.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsSFAuthorizationView sfAuthorizationView => sfAuthorizationView -> Bool -> IO ()
setEnabled sfAuthorizationView enabled =
  sendMessage sfAuthorizationView setEnabledSelector enabled

-- | isEnabled
--
-- Indicates if the authorization view is enabled or disabled.
--
-- ObjC selector: @- isEnabled@
isEnabled :: IsSFAuthorizationView sfAuthorizationView => sfAuthorizationView -> IO Bool
isEnabled sfAuthorizationView =
  sendMessage sfAuthorizationView isEnabledSelector

-- | setFlags:
--
-- Sets the current authorization flags for the view.
--
-- @flags@ — Authorization flags.
--
-- ObjC selector: @- setFlags:@
setFlags :: IsSFAuthorizationView sfAuthorizationView => sfAuthorizationView -> CInt -> IO ()
setFlags sfAuthorizationView flags =
  sendMessage sfAuthorizationView setFlagsSelector flags

-- | setDelegate:
--
-- Sets the delegate for this authorization view. If you want to hear state changes (for example, the user clicked the button), set your delegate and implement the delegate methods mentioned for SFAuthorizationViewDelegate
--
-- @delegate@ — The client's delegate object.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsSFAuthorizationView sfAuthorizationView => sfAuthorizationView -> RawId -> IO ()
setDelegate sfAuthorizationView delegate =
  sendMessage sfAuthorizationView setDelegateSelector delegate

-- | delegate
--
-- ObjC selector: @- delegate@
delegate :: IsSFAuthorizationView sfAuthorizationView => sfAuthorizationView -> IO RawId
delegate sfAuthorizationView =
  sendMessage sfAuthorizationView delegateSelector

-- | authorize:
--
-- ObjC selector: @- authorize:@
authorize :: IsSFAuthorizationView sfAuthorizationView => sfAuthorizationView -> RawId -> IO Bool
authorize sfAuthorizationView inSender =
  sendMessage sfAuthorizationView authorizeSelector inSender

-- | deauthorize:
--
-- ObjC selector: @- deauthorize:@
deauthorize :: IsSFAuthorizationView sfAuthorizationView => sfAuthorizationView -> RawId -> IO Bool
deauthorize sfAuthorizationView inSender =
  sendMessage sfAuthorizationView deauthorizeSelector inSender

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setString:@
setStringSelector :: Selector '[RawId] ()
setStringSelector = mkSelector "setString:"

-- | @Selector@ for @setAuthorizationRights:@
setAuthorizationRightsSelector :: Selector '[Const RawId] ()
setAuthorizationRightsSelector = mkSelector "setAuthorizationRights:"

-- | @Selector@ for @authorizationRights@
authorizationRightsSelector :: Selector '[] RawId
authorizationRightsSelector = mkSelector "authorizationRights"

-- | @Selector@ for @authorization@
authorizationSelector :: Selector '[] (Id SFAuthorization)
authorizationSelector = mkSelector "authorization"

-- | @Selector@ for @updateStatus:@
updateStatusSelector :: Selector '[RawId] Bool
updateStatusSelector = mkSelector "updateStatus:"

-- | @Selector@ for @setAutoupdate:@
setAutoupdateSelector :: Selector '[Bool] ()
setAutoupdateSelector = mkSelector "setAutoupdate:"

-- | @Selector@ for @setAutoupdate:interval:@
setAutoupdate_intervalSelector :: Selector '[Bool, CDouble] ()
setAutoupdate_intervalSelector = mkSelector "setAutoupdate:interval:"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @isEnabled@
isEnabledSelector :: Selector '[] Bool
isEnabledSelector = mkSelector "isEnabled"

-- | @Selector@ for @setFlags:@
setFlagsSelector :: Selector '[CInt] ()
setFlagsSelector = mkSelector "setFlags:"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @authorize:@
authorizeSelector :: Selector '[RawId] Bool
authorizeSelector = mkSelector "authorize:"

-- | @Selector@ for @deauthorize:@
deauthorizeSelector :: Selector '[RawId] Bool
deauthorizeSelector = mkSelector "deauthorize:"

