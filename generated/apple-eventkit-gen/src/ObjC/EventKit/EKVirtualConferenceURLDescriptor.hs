{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | EKVirtualConferenceURLDescriptor
--
-- Describes a URL that can be used to join a virtual conference.
--
-- Generated bindings for @EKVirtualConferenceURLDescriptor@.
module ObjC.EventKit.EKVirtualConferenceURLDescriptor
  ( EKVirtualConferenceURLDescriptor
  , IsEKVirtualConferenceURLDescriptor(..)
  , initWithTitle_URL
  , init_
  , new
  , title
  , url
  , initSelector
  , initWithTitle_URLSelector
  , newSelector
  , titleSelector
  , urlSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.EventKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithTitle:URL:
--
-- Initializes an instance of EKVirtualConferenceURLDescriptor.
--
-- @title@ — A user-readable title describing this URL. This string may be displayed                            in the UI. This string is optional and may be left nil. If your virtual                            conference only has one way to join it, then you can probably leave this                            nil. However, if your virtual conference has multiple ways to join it,                             you should have a title for each URL so that users can better understand                            what each URL represents.
--
-- @URL@ — A URL that, when opened, will join the virtual conference.
--
-- ObjC selector: @- initWithTitle:URL:@
initWithTitle_URL :: (IsEKVirtualConferenceURLDescriptor ekVirtualConferenceURLDescriptor, IsNSString title, IsNSURL url) => ekVirtualConferenceURLDescriptor -> title -> url -> IO (Id EKVirtualConferenceURLDescriptor)
initWithTitle_URL ekVirtualConferenceURLDescriptor title url =
  sendOwnedMessage ekVirtualConferenceURLDescriptor initWithTitle_URLSelector (toNSString title) (toNSURL url)

-- | @- init@
init_ :: IsEKVirtualConferenceURLDescriptor ekVirtualConferenceURLDescriptor => ekVirtualConferenceURLDescriptor -> IO (Id EKVirtualConferenceURLDescriptor)
init_ ekVirtualConferenceURLDescriptor =
  sendOwnedMessage ekVirtualConferenceURLDescriptor initSelector

-- | @+ new@
new :: IO (Id EKVirtualConferenceURLDescriptor)
new  =
  do
    cls' <- getRequiredClass "EKVirtualConferenceURLDescriptor"
    sendOwnedClassMessage cls' newSelector

-- | @- title@
title :: IsEKVirtualConferenceURLDescriptor ekVirtualConferenceURLDescriptor => ekVirtualConferenceURLDescriptor -> IO (Id NSString)
title ekVirtualConferenceURLDescriptor =
  sendMessage ekVirtualConferenceURLDescriptor titleSelector

-- | @- URL@
url :: IsEKVirtualConferenceURLDescriptor ekVirtualConferenceURLDescriptor => ekVirtualConferenceURLDescriptor -> IO (Id NSURL)
url ekVirtualConferenceURLDescriptor =
  sendMessage ekVirtualConferenceURLDescriptor urlSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTitle:URL:@
initWithTitle_URLSelector :: Selector '[Id NSString, Id NSURL] (Id EKVirtualConferenceURLDescriptor)
initWithTitle_URLSelector = mkSelector "initWithTitle:URL:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id EKVirtualConferenceURLDescriptor)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id EKVirtualConferenceURLDescriptor)
newSelector = mkSelector "new"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

