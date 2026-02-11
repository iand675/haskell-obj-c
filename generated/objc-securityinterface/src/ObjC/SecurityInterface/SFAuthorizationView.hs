{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SFAuthorizationView@.
module ObjC.SecurityInterface.SFAuthorizationView
  ( SFAuthorizationView
  , IsSFAuthorizationView(..)
  , setString
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
  , setStringSelector
  , authorizationSelector
  , updateStatusSelector
  , setAutoupdateSelector
  , setAutoupdate_intervalSelector
  , setEnabledSelector
  , isEnabledSelector
  , setFlagsSelector
  , setDelegateSelector
  , delegateSelector
  , authorizeSelector
  , deauthorizeSelector


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
setString sfAuthorizationView  authorizationString =
  sendMsg sfAuthorizationView (mkSelector "setString:") retVoid [argPtr (castPtr (unRawId authorizationString) :: Ptr ())]

-- | authorization
--
-- Returns the authorization object associated with this view.
--
-- ObjC selector: @- authorization@
authorization :: IsSFAuthorizationView sfAuthorizationView => sfAuthorizationView -> IO (Id SFAuthorization)
authorization sfAuthorizationView  =
  sendMsg sfAuthorizationView (mkSelector "authorization") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | updateStatus:
--
-- This method is called when the state of the authorization object has changed.
--
-- @inSender@ — The action that is marked for updateStatus.
--
-- ObjC selector: @- updateStatus:@
updateStatus :: IsSFAuthorizationView sfAuthorizationView => sfAuthorizationView -> RawId -> IO Bool
updateStatus sfAuthorizationView  inSender =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg sfAuthorizationView (mkSelector "updateStatus:") retCULong [argPtr (castPtr (unRawId inSender) :: Ptr ())]

-- | setAutoupdate:
--
-- ObjC selector: @- setAutoupdate:@
setAutoupdate :: IsSFAuthorizationView sfAuthorizationView => sfAuthorizationView -> Bool -> IO ()
setAutoupdate sfAuthorizationView  autoupdate =
  sendMsg sfAuthorizationView (mkSelector "setAutoupdate:") retVoid [argCULong (if autoupdate then 1 else 0)]

-- | setAutoUpdate:interval:
--
-- ObjC selector: @- setAutoupdate:interval:@
setAutoupdate_interval :: IsSFAuthorizationView sfAuthorizationView => sfAuthorizationView -> Bool -> CDouble -> IO ()
setAutoupdate_interval sfAuthorizationView  autoupdate interval =
  sendMsg sfAuthorizationView (mkSelector "setAutoupdate:interval:") retVoid [argCULong (if autoupdate then 1 else 0), argCDouble (fromIntegral interval)]

-- | setEnabled:
--
-- Sets the current state of the authorization view.
--
-- @enabled@ — Enable flag.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsSFAuthorizationView sfAuthorizationView => sfAuthorizationView -> Bool -> IO ()
setEnabled sfAuthorizationView  enabled =
  sendMsg sfAuthorizationView (mkSelector "setEnabled:") retVoid [argCULong (if enabled then 1 else 0)]

-- | isEnabled
--
-- Indicates if the authorization view is enabled or disabled.
--
-- ObjC selector: @- isEnabled@
isEnabled :: IsSFAuthorizationView sfAuthorizationView => sfAuthorizationView -> IO Bool
isEnabled sfAuthorizationView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg sfAuthorizationView (mkSelector "isEnabled") retCULong []

-- | setFlags:
--
-- Sets the current authorization flags for the view.
--
-- @flags@ — Authorization flags.
--
-- ObjC selector: @- setFlags:@
setFlags :: IsSFAuthorizationView sfAuthorizationView => sfAuthorizationView -> AuthorizationFlags -> IO ()
setFlags sfAuthorizationView  flags =
  sendMsg sfAuthorizationView (mkSelector "setFlags:") retVoid [argCUInt (coerce flags)]

-- | setDelegate:
--
-- Sets the delegate for this authorization view. If you want to hear state changes (for example, the user clicked the button), set your delegate and implement the delegate methods mentioned for SFAuthorizationViewDelegate
--
-- @delegate@ — The client's delegate object.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsSFAuthorizationView sfAuthorizationView => sfAuthorizationView -> RawId -> IO ()
setDelegate sfAuthorizationView  delegate =
  sendMsg sfAuthorizationView (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId delegate) :: Ptr ())]

-- | delegate
--
-- ObjC selector: @- delegate@
delegate :: IsSFAuthorizationView sfAuthorizationView => sfAuthorizationView -> IO RawId
delegate sfAuthorizationView  =
  fmap (RawId . castPtr) $ sendMsg sfAuthorizationView (mkSelector "delegate") (retPtr retVoid) []

-- | authorize:
--
-- ObjC selector: @- authorize:@
authorize :: IsSFAuthorizationView sfAuthorizationView => sfAuthorizationView -> RawId -> IO Bool
authorize sfAuthorizationView  inSender =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg sfAuthorizationView (mkSelector "authorize:") retCULong [argPtr (castPtr (unRawId inSender) :: Ptr ())]

-- | deauthorize:
--
-- ObjC selector: @- deauthorize:@
deauthorize :: IsSFAuthorizationView sfAuthorizationView => sfAuthorizationView -> RawId -> IO Bool
deauthorize sfAuthorizationView  inSender =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg sfAuthorizationView (mkSelector "deauthorize:") retCULong [argPtr (castPtr (unRawId inSender) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setString:@
setStringSelector :: Selector
setStringSelector = mkSelector "setString:"

-- | @Selector@ for @authorization@
authorizationSelector :: Selector
authorizationSelector = mkSelector "authorization"

-- | @Selector@ for @updateStatus:@
updateStatusSelector :: Selector
updateStatusSelector = mkSelector "updateStatus:"

-- | @Selector@ for @setAutoupdate:@
setAutoupdateSelector :: Selector
setAutoupdateSelector = mkSelector "setAutoupdate:"

-- | @Selector@ for @setAutoupdate:interval:@
setAutoupdate_intervalSelector :: Selector
setAutoupdate_intervalSelector = mkSelector "setAutoupdate:interval:"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @isEnabled@
isEnabledSelector :: Selector
isEnabledSelector = mkSelector "isEnabled"

-- | @Selector@ for @setFlags:@
setFlagsSelector :: Selector
setFlagsSelector = mkSelector "setFlags:"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @authorize:@
authorizeSelector :: Selector
authorizeSelector = mkSelector "authorize:"

-- | @Selector@ for @deauthorize:@
deauthorizeSelector :: Selector
deauthorizeSelector = mkSelector "deauthorize:"

