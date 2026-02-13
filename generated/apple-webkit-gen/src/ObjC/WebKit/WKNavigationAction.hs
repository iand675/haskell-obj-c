{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , buttonNumberSelector
  , isContentRuleListRedirectSelector
  , modifierFlagsSelector
  , navigationTypeSelector
  , requestSelector
  , shouldPerformDownloadSelector
  , sourceFrameSelector
  , targetFrameSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
sourceFrame wkNavigationAction =
  sendMessage wkNavigationAction sourceFrameSelector

-- | The target frame, or nil if this is a new window navigation.
--
-- ObjC selector: @- targetFrame@
targetFrame :: IsWKNavigationAction wkNavigationAction => wkNavigationAction -> IO (Id WKFrameInfo)
targetFrame wkNavigationAction =
  sendMessage wkNavigationAction targetFrameSelector

-- | The type of action that triggered the navigation.
--
-- The value is one of the constants of the enumerated type WKNavigationType.
--
-- ObjC selector: @- navigationType@
navigationType :: IsWKNavigationAction wkNavigationAction => wkNavigationAction -> IO WKNavigationType
navigationType wkNavigationAction =
  sendMessage wkNavigationAction navigationTypeSelector

-- | The navigation's request.
--
-- ObjC selector: @- request@
request :: IsWKNavigationAction wkNavigationAction => wkNavigationAction -> IO (Id NSURLRequest)
request wkNavigationAction =
  sendMessage wkNavigationAction requestSelector

-- | A value indicating whether the web content used a download attribute to indicate that this should be downloaded.
--
-- ObjC selector: @- shouldPerformDownload@
shouldPerformDownload :: IsWKNavigationAction wkNavigationAction => wkNavigationAction -> IO Bool
shouldPerformDownload wkNavigationAction =
  sendMessage wkNavigationAction shouldPerformDownloadSelector

-- | Whether or not the navigation is a redirect from a content rule list.
--
-- ObjC selector: @- isContentRuleListRedirect@
isContentRuleListRedirect :: IsWKNavigationAction wkNavigationAction => wkNavigationAction -> IO Bool
isContentRuleListRedirect wkNavigationAction =
  sendMessage wkNavigationAction isContentRuleListRedirectSelector

-- | The modifier keys that were in effect when the navigation was requested.
--
-- ObjC selector: @- modifierFlags@
modifierFlags :: IsWKNavigationAction wkNavigationAction => wkNavigationAction -> IO NSEventModifierFlags
modifierFlags wkNavigationAction =
  sendMessage wkNavigationAction modifierFlagsSelector

-- | The number of the mouse button causing the navigation to be requested.
--
-- ObjC selector: @- buttonNumber@
buttonNumber :: IsWKNavigationAction wkNavigationAction => wkNavigationAction -> IO CLong
buttonNumber wkNavigationAction =
  sendMessage wkNavigationAction buttonNumberSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sourceFrame@
sourceFrameSelector :: Selector '[] (Id WKFrameInfo)
sourceFrameSelector = mkSelector "sourceFrame"

-- | @Selector@ for @targetFrame@
targetFrameSelector :: Selector '[] (Id WKFrameInfo)
targetFrameSelector = mkSelector "targetFrame"

-- | @Selector@ for @navigationType@
navigationTypeSelector :: Selector '[] WKNavigationType
navigationTypeSelector = mkSelector "navigationType"

-- | @Selector@ for @request@
requestSelector :: Selector '[] (Id NSURLRequest)
requestSelector = mkSelector "request"

-- | @Selector@ for @shouldPerformDownload@
shouldPerformDownloadSelector :: Selector '[] Bool
shouldPerformDownloadSelector = mkSelector "shouldPerformDownload"

-- | @Selector@ for @isContentRuleListRedirect@
isContentRuleListRedirectSelector :: Selector '[] Bool
isContentRuleListRedirectSelector = mkSelector "isContentRuleListRedirect"

-- | @Selector@ for @modifierFlags@
modifierFlagsSelector :: Selector '[] NSEventModifierFlags
modifierFlagsSelector = mkSelector "modifierFlags"

-- | @Selector@ for @buttonNumber@
buttonNumberSelector :: Selector '[] CLong
buttonNumberSelector = mkSelector "buttonNumber"

