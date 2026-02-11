{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A WKNavigationAction object contains information about an action that may cause a navigation, used for making policy decisions.
--
-- Generated bindings for @WKNavigationAction@.
module ObjC.WebKit.WKNavigationAction
  ( WKNavigationAction
  , IsWKNavigationAction(..)
  , sourceFrame
  , targetFrame
  , navigationType
  , request
  , shouldPerformDownload
  , isContentRuleListRedirect
  , modifierFlags
  , buttonNumber
  , sourceFrameSelector
  , targetFrameSelector
  , navigationTypeSelector
  , requestSelector
  , shouldPerformDownloadSelector
  , isContentRuleListRedirectSelector
  , modifierFlagsSelector
  , buttonNumberSelector

  -- * Enum types
  , NSEventModifierFlags(NSEventModifierFlags)
  , pattern NSEventModifierFlagCapsLock
  , pattern NSEventModifierFlagShift
  , pattern NSEventModifierFlagControl
  , pattern NSEventModifierFlagOption
  , pattern NSEventModifierFlagCommand
  , pattern NSEventModifierFlagNumericPad
  , pattern NSEventModifierFlagHelp
  , pattern NSEventModifierFlagFunction
  , pattern NSEventModifierFlagDeviceIndependentFlagsMask
  , WKNavigationType(WKNavigationType)
  , pattern WKNavigationTypeLinkActivated
  , pattern WKNavigationTypeFormSubmitted
  , pattern WKNavigationTypeBackForward
  , pattern WKNavigationTypeReload
  , pattern WKNavigationTypeFormResubmitted
  , pattern WKNavigationTypeOther

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

import ObjC.WebKit.Internal.Classes
import ObjC.WebKit.Internal.Enums
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | The frame requesting the navigation.
--
-- ObjC selector: @- sourceFrame@
sourceFrame :: IsWKNavigationAction wkNavigationAction => wkNavigationAction -> IO (Id WKFrameInfo)
sourceFrame wkNavigationAction  =
  sendMsg wkNavigationAction (mkSelector "sourceFrame") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The target frame, or nil if this is a new window navigation.
--
-- ObjC selector: @- targetFrame@
targetFrame :: IsWKNavigationAction wkNavigationAction => wkNavigationAction -> IO (Id WKFrameInfo)
targetFrame wkNavigationAction  =
  sendMsg wkNavigationAction (mkSelector "targetFrame") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The type of action that triggered the navigation.
--
-- The value is one of the constants of the enumerated type WKNavigationType.
--
-- ObjC selector: @- navigationType@
navigationType :: IsWKNavigationAction wkNavigationAction => wkNavigationAction -> IO WKNavigationType
navigationType wkNavigationAction  =
  fmap (coerce :: CLong -> WKNavigationType) $ sendMsg wkNavigationAction (mkSelector "navigationType") retCLong []

-- | The navigation's request.
--
-- ObjC selector: @- request@
request :: IsWKNavigationAction wkNavigationAction => wkNavigationAction -> IO (Id NSURLRequest)
request wkNavigationAction  =
  sendMsg wkNavigationAction (mkSelector "request") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A value indicating whether the web content used a download attribute to indicate that this should be downloaded.
--
-- ObjC selector: @- shouldPerformDownload@
shouldPerformDownload :: IsWKNavigationAction wkNavigationAction => wkNavigationAction -> IO Bool
shouldPerformDownload wkNavigationAction  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkNavigationAction (mkSelector "shouldPerformDownload") retCULong []

-- | Whether or not the navigation is a redirect from a content rule list.
--
-- ObjC selector: @- isContentRuleListRedirect@
isContentRuleListRedirect :: IsWKNavigationAction wkNavigationAction => wkNavigationAction -> IO Bool
isContentRuleListRedirect wkNavigationAction  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkNavigationAction (mkSelector "isContentRuleListRedirect") retCULong []

-- | The modifier keys that were in effect when the navigation was requested.
--
-- ObjC selector: @- modifierFlags@
modifierFlags :: IsWKNavigationAction wkNavigationAction => wkNavigationAction -> IO NSEventModifierFlags
modifierFlags wkNavigationAction  =
  fmap (coerce :: CULong -> NSEventModifierFlags) $ sendMsg wkNavigationAction (mkSelector "modifierFlags") retCULong []

-- | The number of the mouse button causing the navigation to be requested.
--
-- ObjC selector: @- buttonNumber@
buttonNumber :: IsWKNavigationAction wkNavigationAction => wkNavigationAction -> IO CLong
buttonNumber wkNavigationAction  =
  sendMsg wkNavigationAction (mkSelector "buttonNumber") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sourceFrame@
sourceFrameSelector :: Selector
sourceFrameSelector = mkSelector "sourceFrame"

-- | @Selector@ for @targetFrame@
targetFrameSelector :: Selector
targetFrameSelector = mkSelector "targetFrame"

-- | @Selector@ for @navigationType@
navigationTypeSelector :: Selector
navigationTypeSelector = mkSelector "navigationType"

-- | @Selector@ for @request@
requestSelector :: Selector
requestSelector = mkSelector "request"

-- | @Selector@ for @shouldPerformDownload@
shouldPerformDownloadSelector :: Selector
shouldPerformDownloadSelector = mkSelector "shouldPerformDownload"

-- | @Selector@ for @isContentRuleListRedirect@
isContentRuleListRedirectSelector :: Selector
isContentRuleListRedirectSelector = mkSelector "isContentRuleListRedirect"

-- | @Selector@ for @modifierFlags@
modifierFlagsSelector :: Selector
modifierFlagsSelector = mkSelector "modifierFlags"

-- | @Selector@ for @buttonNumber@
buttonNumberSelector :: Selector
buttonNumberSelector = mkSelector "buttonNumber"

