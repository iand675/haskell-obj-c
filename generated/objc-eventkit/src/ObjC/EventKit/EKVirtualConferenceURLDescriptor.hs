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
  , initWithTitle_URLSelector
  , initSelector
  , newSelector
  , titleSelector
  , urlSelector


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
initWithTitle_URL ekVirtualConferenceURLDescriptor  title url =
withObjCPtr title $ \raw_title ->
  withObjCPtr url $ \raw_url ->
      sendMsg ekVirtualConferenceURLDescriptor (mkSelector "initWithTitle:URL:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsEKVirtualConferenceURLDescriptor ekVirtualConferenceURLDescriptor => ekVirtualConferenceURLDescriptor -> IO (Id EKVirtualConferenceURLDescriptor)
init_ ekVirtualConferenceURLDescriptor  =
  sendMsg ekVirtualConferenceURLDescriptor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id EKVirtualConferenceURLDescriptor)
new  =
  do
    cls' <- getRequiredClass "EKVirtualConferenceURLDescriptor"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- title@
title :: IsEKVirtualConferenceURLDescriptor ekVirtualConferenceURLDescriptor => ekVirtualConferenceURLDescriptor -> IO (Id NSString)
title ekVirtualConferenceURLDescriptor  =
  sendMsg ekVirtualConferenceURLDescriptor (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- URL@
url :: IsEKVirtualConferenceURLDescriptor ekVirtualConferenceURLDescriptor => ekVirtualConferenceURLDescriptor -> IO (Id NSURL)
url ekVirtualConferenceURLDescriptor  =
  sendMsg ekVirtualConferenceURLDescriptor (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTitle:URL:@
initWithTitle_URLSelector :: Selector
initWithTitle_URLSelector = mkSelector "initWithTitle:URL:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

