{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SFAuthorizationView
--
-- SFAuthorizationView is a custom view that you can use to visually represent restricted functionality, requiring authorization for access.
--
-- You can add SFAuthorizationView to your application as a custom view, make your controller the delegate for the view and initialize the view by setting a right string (setString:) or rights structure (setAuthorizationRights:) to check for, as well as auto-updates (setAutoupdate:) or manual updates (updateStatus:). Note that you'll have to call updateStatus: to set the lock icon to its initial state. Changes to the current state as well as the startup state (after the initial updateStatus) are communicated to the delegate.  Implementing any of the following is optional):          authorized: changed to unlocked      deauthorized: changed to locked      shouldDeauthorize: when a user wants to lock, the delegates can react to this before it happens and avoid it by returning NO.      cancelAuthorization: the user cancelled authorization.    Calls to updateStatus: return YES if in the unlocked state, NO otherwise. Note that when committing changes or performing actions, authorization has to be checked again before going ahead with it. The default behavior of this view is to pre-authorize rights, if this is not possible it will unlock and wait for authorization to be checked when explicitly required. For handing the SFAuthorization (authorization:) to another process it's NSCoder support can be used.
--
-- Generated bindings for @SFAuthorizationViewDelegate@.
module ObjC.SecurityInterface.SFAuthorizationViewDelegate
  ( SFAuthorizationViewDelegate
  , IsSFAuthorizationViewDelegate(..)


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

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

