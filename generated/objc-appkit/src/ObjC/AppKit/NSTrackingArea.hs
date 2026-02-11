{-# LANGUAGE PatternSynonyms #-}
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
  , rectSelector
  , optionsSelector
  , ownerSelector
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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithRect:options:owner:userInfo:@
initWithRect_options_owner_userInfo :: (IsNSTrackingArea nsTrackingArea, IsNSDictionary userInfo) => nsTrackingArea -> NSRect -> NSTrackingAreaOptions -> RawId -> userInfo -> IO (Id NSTrackingArea)
initWithRect_options_owner_userInfo nsTrackingArea  rect options owner userInfo =
withObjCPtr userInfo $ \raw_userInfo ->
    sendMsg nsTrackingArea (mkSelector "initWithRect:options:owner:userInfo:") (retPtr retVoid) [argNSRect rect, argCULong (coerce options), argPtr (castPtr (unRawId owner) :: Ptr ()), argPtr (castPtr raw_userInfo :: Ptr ())] >>= ownedObject . castPtr

-- | @- rect@
rect :: IsNSTrackingArea nsTrackingArea => nsTrackingArea -> IO NSRect
rect nsTrackingArea  =
  sendMsgStret nsTrackingArea (mkSelector "rect") retNSRect []

-- | @- options@
options :: IsNSTrackingArea nsTrackingArea => nsTrackingArea -> IO NSTrackingAreaOptions
options nsTrackingArea  =
  fmap (coerce :: CULong -> NSTrackingAreaOptions) $ sendMsg nsTrackingArea (mkSelector "options") retCULong []

-- | @- owner@
owner :: IsNSTrackingArea nsTrackingArea => nsTrackingArea -> IO RawId
owner nsTrackingArea  =
  fmap (RawId . castPtr) $ sendMsg nsTrackingArea (mkSelector "owner") (retPtr retVoid) []

-- | @- userInfo@
userInfo :: IsNSTrackingArea nsTrackingArea => nsTrackingArea -> IO (Id NSDictionary)
userInfo nsTrackingArea  =
  sendMsg nsTrackingArea (mkSelector "userInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRect:options:owner:userInfo:@
initWithRect_options_owner_userInfoSelector :: Selector
initWithRect_options_owner_userInfoSelector = mkSelector "initWithRect:options:owner:userInfo:"

-- | @Selector@ for @rect@
rectSelector :: Selector
rectSelector = mkSelector "rect"

-- | @Selector@ for @options@
optionsSelector :: Selector
optionsSelector = mkSelector "options"

-- | @Selector@ for @owner@
ownerSelector :: Selector
ownerSelector = mkSelector "owner"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector
userInfoSelector = mkSelector "userInfo"

