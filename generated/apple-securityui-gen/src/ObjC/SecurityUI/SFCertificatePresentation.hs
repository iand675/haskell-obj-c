{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SFCertificatePresentation@.
module ObjC.SecurityUI.SFCertificatePresentation
  ( SFCertificatePresentation
  , IsSFCertificatePresentation(..)
  , initWithTrust
  , init_
  , new
  , presentSheetInWindow_dismissHandler
  , dismissSheet
  , trust
  , title
  , setTitle
  , message
  , setMessage
  , helpURL
  , setHelpURL
  , dismissSheetSelector
  , helpURLSelector
  , initSelector
  , initWithTrustSelector
  , messageSelector
  , newSelector
  , presentSheetInWindow_dismissHandlerSelector
  , setHelpURLSelector
  , setMessageSelector
  , setTitleSelector
  , titleSelector
  , trustSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SecurityUI.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize the certificate presentation with a certificate trust reference.
--
-- ObjC selector: @- initWithTrust:@
initWithTrust :: IsSFCertificatePresentation sfCertificatePresentation => sfCertificatePresentation -> Ptr () -> IO (Id SFCertificatePresentation)
initWithTrust sfCertificatePresentation trust =
  sendOwnedMessage sfCertificatePresentation initWithTrustSelector trust

-- | Clients should use designated initializers.
--
-- ObjC selector: @- init@
init_ :: IsSFCertificatePresentation sfCertificatePresentation => sfCertificatePresentation -> IO (Id SFCertificatePresentation)
init_ sfCertificatePresentation =
  sendOwnedMessage sfCertificatePresentation initSelector

-- | @+ new@
new :: IO (Id SFCertificatePresentation)
new  =
  do
    cls' <- getRequiredClass "SFCertificatePresentation"
    sendOwnedClassMessage cls' newSelector

-- | @- presentSheetInWindow:dismissHandler:@
presentSheetInWindow_dismissHandler :: (IsSFCertificatePresentation sfCertificatePresentation, IsNSWindow window) => sfCertificatePresentation -> window -> Ptr () -> IO ()
presentSheetInWindow_dismissHandler sfCertificatePresentation window dismissHandler =
  sendMessage sfCertificatePresentation presentSheetInWindow_dismissHandlerSelector (toNSWindow window) dismissHandler

-- | Dismisses the certificate sheet.
--
-- ObjC selector: @- dismissSheet@
dismissSheet :: IsSFCertificatePresentation sfCertificatePresentation => sfCertificatePresentation -> IO ()
dismissSheet sfCertificatePresentation =
  sendMessage sfCertificatePresentation dismissSheetSelector

-- | A trust reference, previously created with SecTrustCreateWithCertificates (see <Security/SecTrust.h>).
--
-- ObjC selector: @- trust@
trust :: IsSFCertificatePresentation sfCertificatePresentation => sfCertificatePresentation -> IO (Ptr ())
trust sfCertificatePresentation =
  sendMessage sfCertificatePresentation trustSelector

-- | Title string to be displayed. If no title is provided, a default title will be used.
--
-- ObjC selector: @- title@
title :: IsSFCertificatePresentation sfCertificatePresentation => sfCertificatePresentation -> IO (Id NSString)
title sfCertificatePresentation =
  sendMessage sfCertificatePresentation titleSelector

-- | Title string to be displayed. If no title is provided, a default title will be used.
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsSFCertificatePresentation sfCertificatePresentation, IsNSString value) => sfCertificatePresentation -> value -> IO ()
setTitle sfCertificatePresentation value =
  sendMessage sfCertificatePresentation setTitleSelector (toNSString value)

-- | Message string to be displayed. If no message is provided, a default message will be used.
--
-- ObjC selector: @- message@
message :: IsSFCertificatePresentation sfCertificatePresentation => sfCertificatePresentation -> IO (Id NSString)
message sfCertificatePresentation =
  sendMessage sfCertificatePresentation messageSelector

-- | Message string to be displayed. If no message is provided, a default message will be used.
--
-- ObjC selector: @- setMessage:@
setMessage :: (IsSFCertificatePresentation sfCertificatePresentation, IsNSString value) => sfCertificatePresentation -> value -> IO ()
setMessage sfCertificatePresentation value =
  sendMessage sfCertificatePresentation setMessageSelector (toNSString value)

-- | The URL that will be opened by clicking the "Learn More" button.
--
-- ObjC selector: @- helpURL@
helpURL :: IsSFCertificatePresentation sfCertificatePresentation => sfCertificatePresentation -> IO (Id NSURL)
helpURL sfCertificatePresentation =
  sendMessage sfCertificatePresentation helpURLSelector

-- | The URL that will be opened by clicking the "Learn More" button.
--
-- ObjC selector: @- setHelpURL:@
setHelpURL :: (IsSFCertificatePresentation sfCertificatePresentation, IsNSURL value) => sfCertificatePresentation -> value -> IO ()
setHelpURL sfCertificatePresentation value =
  sendMessage sfCertificatePresentation setHelpURLSelector (toNSURL value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTrust:@
initWithTrustSelector :: Selector '[Ptr ()] (Id SFCertificatePresentation)
initWithTrustSelector = mkSelector "initWithTrust:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SFCertificatePresentation)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SFCertificatePresentation)
newSelector = mkSelector "new"

-- | @Selector@ for @presentSheetInWindow:dismissHandler:@
presentSheetInWindow_dismissHandlerSelector :: Selector '[Id NSWindow, Ptr ()] ()
presentSheetInWindow_dismissHandlerSelector = mkSelector "presentSheetInWindow:dismissHandler:"

-- | @Selector@ for @dismissSheet@
dismissSheetSelector :: Selector '[] ()
dismissSheetSelector = mkSelector "dismissSheet"

-- | @Selector@ for @trust@
trustSelector :: Selector '[] (Ptr ())
trustSelector = mkSelector "trust"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @message@
messageSelector :: Selector '[] (Id NSString)
messageSelector = mkSelector "message"

-- | @Selector@ for @setMessage:@
setMessageSelector :: Selector '[Id NSString] ()
setMessageSelector = mkSelector "setMessage:"

-- | @Selector@ for @helpURL@
helpURLSelector :: Selector '[] (Id NSURL)
helpURLSelector = mkSelector "helpURL"

-- | @Selector@ for @setHelpURL:@
setHelpURLSelector :: Selector '[Id NSURL] ()
setHelpURLSelector = mkSelector "setHelpURL:"

