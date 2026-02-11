{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Specifies whether a specific type of sharing should be disabled in the share picker, and if so, whether a reason should be provided for the disablement If a reason is provided, the corresponding mode will show up as an option, but an alert explaining why it is disabled will show if it is chosen, and the mode will switch back to the supported one Optionally, an extra alert button can be provided for a "recovery suggestion". This can give a user a way to fix whatever is causing this type of sharing to be disabled If no reason is provided, the corresponding mode will not show up as an option
--
-- Generated bindings for @NSSharingCollaborationModeRestriction@.
module ObjC.AppKit.NSSharingCollaborationModeRestriction
  ( NSSharingCollaborationModeRestriction
  , IsNSSharingCollaborationModeRestriction(..)
  , initWithDisabledMode
  , initWithDisabledMode_alertTitle_alertMessage
  , initWithDisabledMode_alertTitle_alertMessage_alertDismissButtonTitle
  , initWithDisabledMode_alertTitle_alertMessage_alertDismissButtonTitle_alertRecoverySuggestionButtonTitle_alertRecoverySuggestionButtonLaunchURL
  , init_
  , new
  , disabledMode
  , alertTitle
  , alertMessage
  , alertDismissButtonTitle
  , alertRecoverySuggestionButtonTitle
  , alertRecoverySuggestionButtonLaunchURL
  , initWithDisabledModeSelector
  , initWithDisabledMode_alertTitle_alertMessageSelector
  , initWithDisabledMode_alertTitle_alertMessage_alertDismissButtonTitleSelector
  , initWithDisabledMode_alertTitle_alertMessage_alertDismissButtonTitle_alertRecoverySuggestionButtonTitle_alertRecoverySuggestionButtonLaunchURLSelector
  , initSelector
  , newSelector
  , disabledModeSelector
  , alertTitleSelector
  , alertMessageSelector
  , alertDismissButtonTitleSelector
  , alertRecoverySuggestionButtonTitleSelector
  , alertRecoverySuggestionButtonLaunchURLSelector

  -- * Enum types
  , NSSharingCollaborationMode(NSSharingCollaborationMode)
  , pattern NSSharingCollaborationModeSendCopy
  , pattern NSSharingCollaborationModeCollaborate

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

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | - Parameters:     - disabledMode: The disabled type of sharing
--
-- ObjC selector: @- initWithDisabledMode:@
initWithDisabledMode :: IsNSSharingCollaborationModeRestriction nsSharingCollaborationModeRestriction => nsSharingCollaborationModeRestriction -> NSSharingCollaborationMode -> IO (Id NSSharingCollaborationModeRestriction)
initWithDisabledMode nsSharingCollaborationModeRestriction  disabledMode =
  sendMsg nsSharingCollaborationModeRestriction (mkSelector "initWithDisabledMode:") (retPtr retVoid) [argCLong (coerce disabledMode)] >>= ownedObject . castPtr

-- | - Parameters:     - disabledMode: The disabled type of sharing     - alertTitle: The alert title     - alertMessage: The alert message
--
-- ObjC selector: @- initWithDisabledMode:alertTitle:alertMessage:@
initWithDisabledMode_alertTitle_alertMessage :: (IsNSSharingCollaborationModeRestriction nsSharingCollaborationModeRestriction, IsNSString alertTitle, IsNSString alertMessage) => nsSharingCollaborationModeRestriction -> NSSharingCollaborationMode -> alertTitle -> alertMessage -> IO (Id NSSharingCollaborationModeRestriction)
initWithDisabledMode_alertTitle_alertMessage nsSharingCollaborationModeRestriction  disabledMode alertTitle alertMessage =
withObjCPtr alertTitle $ \raw_alertTitle ->
  withObjCPtr alertMessage $ \raw_alertMessage ->
      sendMsg nsSharingCollaborationModeRestriction (mkSelector "initWithDisabledMode:alertTitle:alertMessage:") (retPtr retVoid) [argCLong (coerce disabledMode), argPtr (castPtr raw_alertTitle :: Ptr ()), argPtr (castPtr raw_alertMessage :: Ptr ())] >>= ownedObject . castPtr

-- | - Parameters:     - disabledMode: The disabled type of sharing     - alertTitle: The alert title     - alertMessage: The alert message     - alertDismissButtonTitle: The label on the default alert button
--
-- ObjC selector: @- initWithDisabledMode:alertTitle:alertMessage:alertDismissButtonTitle:@
initWithDisabledMode_alertTitle_alertMessage_alertDismissButtonTitle :: (IsNSSharingCollaborationModeRestriction nsSharingCollaborationModeRestriction, IsNSString alertTitle, IsNSString alertMessage, IsNSString alertDismissButtonTitle) => nsSharingCollaborationModeRestriction -> NSSharingCollaborationMode -> alertTitle -> alertMessage -> alertDismissButtonTitle -> IO (Id NSSharingCollaborationModeRestriction)
initWithDisabledMode_alertTitle_alertMessage_alertDismissButtonTitle nsSharingCollaborationModeRestriction  disabledMode alertTitle alertMessage alertDismissButtonTitle =
withObjCPtr alertTitle $ \raw_alertTitle ->
  withObjCPtr alertMessage $ \raw_alertMessage ->
    withObjCPtr alertDismissButtonTitle $ \raw_alertDismissButtonTitle ->
        sendMsg nsSharingCollaborationModeRestriction (mkSelector "initWithDisabledMode:alertTitle:alertMessage:alertDismissButtonTitle:") (retPtr retVoid) [argCLong (coerce disabledMode), argPtr (castPtr raw_alertTitle :: Ptr ()), argPtr (castPtr raw_alertMessage :: Ptr ()), argPtr (castPtr raw_alertDismissButtonTitle :: Ptr ())] >>= ownedObject . castPtr

-- | - Parameters:     - disabledMode: The disabled type of sharing     - alertTitle: The alert title     - alertMessage: The alert message     - alertDismissButtonTitle: The label on the default alert button     - alertRecoverySuggestionButtonTitle: The label on the optional recovery suggestion button on the alert     - alertRecoverySuggestionButtonLaunchURL: The URL that is opened when the optional recovery suggestion button is selected
--
-- ObjC selector: @- initWithDisabledMode:alertTitle:alertMessage:alertDismissButtonTitle:alertRecoverySuggestionButtonTitle:alertRecoverySuggestionButtonLaunchURL:@
initWithDisabledMode_alertTitle_alertMessage_alertDismissButtonTitle_alertRecoverySuggestionButtonTitle_alertRecoverySuggestionButtonLaunchURL :: (IsNSSharingCollaborationModeRestriction nsSharingCollaborationModeRestriction, IsNSString alertTitle, IsNSString alertMessage, IsNSString alertDismissButtonTitle, IsNSString alertRecoverySuggestionButtonTitle, IsNSURL alertRecoverySuggestionButtonLaunchURL) => nsSharingCollaborationModeRestriction -> NSSharingCollaborationMode -> alertTitle -> alertMessage -> alertDismissButtonTitle -> alertRecoverySuggestionButtonTitle -> alertRecoverySuggestionButtonLaunchURL -> IO (Id NSSharingCollaborationModeRestriction)
initWithDisabledMode_alertTitle_alertMessage_alertDismissButtonTitle_alertRecoverySuggestionButtonTitle_alertRecoverySuggestionButtonLaunchURL nsSharingCollaborationModeRestriction  disabledMode alertTitle alertMessage alertDismissButtonTitle alertRecoverySuggestionButtonTitle alertRecoverySuggestionButtonLaunchURL =
withObjCPtr alertTitle $ \raw_alertTitle ->
  withObjCPtr alertMessage $ \raw_alertMessage ->
    withObjCPtr alertDismissButtonTitle $ \raw_alertDismissButtonTitle ->
      withObjCPtr alertRecoverySuggestionButtonTitle $ \raw_alertRecoverySuggestionButtonTitle ->
        withObjCPtr alertRecoverySuggestionButtonLaunchURL $ \raw_alertRecoverySuggestionButtonLaunchURL ->
            sendMsg nsSharingCollaborationModeRestriction (mkSelector "initWithDisabledMode:alertTitle:alertMessage:alertDismissButtonTitle:alertRecoverySuggestionButtonTitle:alertRecoverySuggestionButtonLaunchURL:") (retPtr retVoid) [argCLong (coerce disabledMode), argPtr (castPtr raw_alertTitle :: Ptr ()), argPtr (castPtr raw_alertMessage :: Ptr ()), argPtr (castPtr raw_alertDismissButtonTitle :: Ptr ()), argPtr (castPtr raw_alertRecoverySuggestionButtonTitle :: Ptr ()), argPtr (castPtr raw_alertRecoverySuggestionButtonLaunchURL :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSSharingCollaborationModeRestriction nsSharingCollaborationModeRestriction => nsSharingCollaborationModeRestriction -> IO (Id NSSharingCollaborationModeRestriction)
init_ nsSharingCollaborationModeRestriction  =
  sendMsg nsSharingCollaborationModeRestriction (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NSSharingCollaborationModeRestriction)
new  =
  do
    cls' <- getRequiredClass "NSSharingCollaborationModeRestriction"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The type of sharing which should be disabled
--
-- ObjC selector: @- disabledMode@
disabledMode :: IsNSSharingCollaborationModeRestriction nsSharingCollaborationModeRestriction => nsSharingCollaborationModeRestriction -> IO NSSharingCollaborationMode
disabledMode nsSharingCollaborationModeRestriction  =
  fmap (coerce :: CLong -> NSSharingCollaborationMode) $ sendMsg nsSharingCollaborationModeRestriction (mkSelector "disabledMode") retCLong []

-- | The title of the alert if a reason for disabling is provided
--
-- ObjC selector: @- alertTitle@
alertTitle :: IsNSSharingCollaborationModeRestriction nsSharingCollaborationModeRestriction => nsSharingCollaborationModeRestriction -> IO (Id NSString)
alertTitle nsSharingCollaborationModeRestriction  =
  sendMsg nsSharingCollaborationModeRestriction (mkSelector "alertTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The message of the alert if a reason for disabling is provided
--
-- ObjC selector: @- alertMessage@
alertMessage :: IsNSSharingCollaborationModeRestriction nsSharingCollaborationModeRestriction => nsSharingCollaborationModeRestriction -> IO (Id NSString)
alertMessage nsSharingCollaborationModeRestriction  =
  sendMsg nsSharingCollaborationModeRestriction (mkSelector "alertMessage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The label on the alert button which will simply confirm that the alert was viewed and dismiss it Defaults to "OK"
--
-- ObjC selector: @- alertDismissButtonTitle@
alertDismissButtonTitle :: IsNSSharingCollaborationModeRestriction nsSharingCollaborationModeRestriction => nsSharingCollaborationModeRestriction -> IO (Id NSString)
alertDismissButtonTitle nsSharingCollaborationModeRestriction  =
  sendMsg nsSharingCollaborationModeRestriction (mkSelector "alertDismissButtonTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The label on the recovery suggestion button if it is provided
--
-- ObjC selector: @- alertRecoverySuggestionButtonTitle@
alertRecoverySuggestionButtonTitle :: IsNSSharingCollaborationModeRestriction nsSharingCollaborationModeRestriction => nsSharingCollaborationModeRestriction -> IO (Id NSString)
alertRecoverySuggestionButtonTitle nsSharingCollaborationModeRestriction  =
  sendMsg nsSharingCollaborationModeRestriction (mkSelector "alertRecoverySuggestionButtonTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The URL that is opened when the user selects the recovery suggestion, if any
--
-- ObjC selector: @- alertRecoverySuggestionButtonLaunchURL@
alertRecoverySuggestionButtonLaunchURL :: IsNSSharingCollaborationModeRestriction nsSharingCollaborationModeRestriction => nsSharingCollaborationModeRestriction -> IO (Id NSURL)
alertRecoverySuggestionButtonLaunchURL nsSharingCollaborationModeRestriction  =
  sendMsg nsSharingCollaborationModeRestriction (mkSelector "alertRecoverySuggestionButtonLaunchURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDisabledMode:@
initWithDisabledModeSelector :: Selector
initWithDisabledModeSelector = mkSelector "initWithDisabledMode:"

-- | @Selector@ for @initWithDisabledMode:alertTitle:alertMessage:@
initWithDisabledMode_alertTitle_alertMessageSelector :: Selector
initWithDisabledMode_alertTitle_alertMessageSelector = mkSelector "initWithDisabledMode:alertTitle:alertMessage:"

-- | @Selector@ for @initWithDisabledMode:alertTitle:alertMessage:alertDismissButtonTitle:@
initWithDisabledMode_alertTitle_alertMessage_alertDismissButtonTitleSelector :: Selector
initWithDisabledMode_alertTitle_alertMessage_alertDismissButtonTitleSelector = mkSelector "initWithDisabledMode:alertTitle:alertMessage:alertDismissButtonTitle:"

-- | @Selector@ for @initWithDisabledMode:alertTitle:alertMessage:alertDismissButtonTitle:alertRecoverySuggestionButtonTitle:alertRecoverySuggestionButtonLaunchURL:@
initWithDisabledMode_alertTitle_alertMessage_alertDismissButtonTitle_alertRecoverySuggestionButtonTitle_alertRecoverySuggestionButtonLaunchURLSelector :: Selector
initWithDisabledMode_alertTitle_alertMessage_alertDismissButtonTitle_alertRecoverySuggestionButtonTitle_alertRecoverySuggestionButtonLaunchURLSelector = mkSelector "initWithDisabledMode:alertTitle:alertMessage:alertDismissButtonTitle:alertRecoverySuggestionButtonTitle:alertRecoverySuggestionButtonLaunchURL:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @disabledMode@
disabledModeSelector :: Selector
disabledModeSelector = mkSelector "disabledMode"

-- | @Selector@ for @alertTitle@
alertTitleSelector :: Selector
alertTitleSelector = mkSelector "alertTitle"

-- | @Selector@ for @alertMessage@
alertMessageSelector :: Selector
alertMessageSelector = mkSelector "alertMessage"

-- | @Selector@ for @alertDismissButtonTitle@
alertDismissButtonTitleSelector :: Selector
alertDismissButtonTitleSelector = mkSelector "alertDismissButtonTitle"

-- | @Selector@ for @alertRecoverySuggestionButtonTitle@
alertRecoverySuggestionButtonTitleSelector :: Selector
alertRecoverySuggestionButtonTitleSelector = mkSelector "alertRecoverySuggestionButtonTitle"

-- | @Selector@ for @alertRecoverySuggestionButtonLaunchURL@
alertRecoverySuggestionButtonLaunchURLSelector :: Selector
alertRecoverySuggestionButtonLaunchURLSelector = mkSelector "alertRecoverySuggestionButtonLaunchURL"

