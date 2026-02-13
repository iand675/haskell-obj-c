{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTrackingArea@.
module ObjC.AppKit.NSTrackingArea
  ( NSTrackingArea
  , IsNSTrackingArea(..)
  , initWithRect_options_owner_userInfo
  , rect
  , options
  , owner
  , userInfo
  , initWithRect_options_owner_userInfoSelector
  , optionsSelector
  , ownerSelector
  , rectSelector
  , userInfoSelector

  -- * Enum types
  , NSTrackingAreaOptions(NSTrackingAreaOptions)
  , pattern NSTrackingMouseEnteredAndExited
  , pattern NSTrackingMouseMoved
  , pattern NSTrackingCursorUpdate
  , pattern NSTrackingActiveWhenFirstResponder
  , pattern NSTrackingActiveInKeyWindow
  , pattern NSTrackingActiveInActiveApp
  , pattern NSTrackingActiveAlways
  , pattern NSTrackingAssumeInside
  , pattern NSTrackingInVisibleRect
  , pattern NSTrackingEnabledDuringMouseDrag

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithRect:options:owner:userInfo:@
initWithRect_options_owner_userInfo :: (IsNSTrackingArea nsTrackingArea, IsNSDictionary userInfo) => nsTrackingArea -> NSRect -> NSTrackingAreaOptions -> RawId -> userInfo -> IO (Id NSTrackingArea)
initWithRect_options_owner_userInfo nsTrackingArea rect options owner userInfo =
  sendOwnedMessage nsTrackingArea initWithRect_options_owner_userInfoSelector rect options owner (toNSDictionary userInfo)

-- | @- rect@
rect :: IsNSTrackingArea nsTrackingArea => nsTrackingArea -> IO NSRect
rect nsTrackingArea =
  sendMessage nsTrackingArea rectSelector

-- | @- options@
options :: IsNSTrackingArea nsTrackingArea => nsTrackingArea -> IO NSTrackingAreaOptions
options nsTrackingArea =
  sendMessage nsTrackingArea optionsSelector

-- | @- owner@
owner :: IsNSTrackingArea nsTrackingArea => nsTrackingArea -> IO RawId
owner nsTrackingArea =
  sendMessage nsTrackingArea ownerSelector

-- | @- userInfo@
userInfo :: IsNSTrackingArea nsTrackingArea => nsTrackingArea -> IO (Id NSDictionary)
userInfo nsTrackingArea =
  sendMessage nsTrackingArea userInfoSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRect:options:owner:userInfo:@
initWithRect_options_owner_userInfoSelector :: Selector '[NSRect, NSTrackingAreaOptions, RawId, Id NSDictionary] (Id NSTrackingArea)
initWithRect_options_owner_userInfoSelector = mkSelector "initWithRect:options:owner:userInfo:"

-- | @Selector@ for @rect@
rectSelector :: Selector '[] NSRect
rectSelector = mkSelector "rect"

-- | @Selector@ for @options@
optionsSelector :: Selector '[] NSTrackingAreaOptions
optionsSelector = mkSelector "options"

-- | @Selector@ for @owner@
ownerSelector :: Selector '[] RawId
ownerSelector = mkSelector "owner"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector '[] (Id NSDictionary)
userInfoSelector = mkSelector "userInfo"

