{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | EKVirtualConferenceDescriptor
--
-- Describes a virtual conference.
--
-- Generated bindings for @EKVirtualConferenceDescriptor@.
module ObjC.EventKit.EKVirtualConferenceDescriptor
  ( EKVirtualConferenceDescriptor
  , IsEKVirtualConferenceDescriptor(..)
  , initWithTitle_URLDescriptors_conferenceDetails
  , init_
  , new
  , title
  , urlDescriptors
  , conferenceDetails
  , initWithTitle_URLDescriptors_conferenceDetailsSelector
  , initSelector
  , newSelector
  , titleSelector
  , urlDescriptorsSelector
  , conferenceDetailsSelector


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

-- | initWithTitle:URLDescriptors:conferenceDetails:
--
-- Initializes an instance of EKVirtualConferenceDescriptor.
--
-- @title@ — A user-readable title describing this virtual conference. This string                                    may be displayed in the UI. This string is optional and may be left nil.
--
-- @URLDescriptors@ — An array of EKVirtualConferenceURLDescriptors, representing the various                                    ways to join your virtual conference. Do not pass an empty array. Your                                    array should be ordered such that the most preferred method of joining                                    the virtual conference comes first in the array, with subsequent methods                                    of joining appearing in descending priority.
--
-- @conferenceDetails@ — A user-readable string containing any other information you wish to                                     communicate to the user about this virtual conference. This string will                                    be displayed in the UI. This argument is optional and may be left nil.
--
-- ObjC selector: @- initWithTitle:URLDescriptors:conferenceDetails:@
initWithTitle_URLDescriptors_conferenceDetails :: (IsEKVirtualConferenceDescriptor ekVirtualConferenceDescriptor, IsNSString title, IsNSArray urlDescriptors, IsNSString conferenceDetails) => ekVirtualConferenceDescriptor -> title -> urlDescriptors -> conferenceDetails -> IO (Id EKVirtualConferenceDescriptor)
initWithTitle_URLDescriptors_conferenceDetails ekVirtualConferenceDescriptor  title urlDescriptors conferenceDetails =
withObjCPtr title $ \raw_title ->
  withObjCPtr urlDescriptors $ \raw_urlDescriptors ->
    withObjCPtr conferenceDetails $ \raw_conferenceDetails ->
        sendMsg ekVirtualConferenceDescriptor (mkSelector "initWithTitle:URLDescriptors:conferenceDetails:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_urlDescriptors :: Ptr ()), argPtr (castPtr raw_conferenceDetails :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsEKVirtualConferenceDescriptor ekVirtualConferenceDescriptor => ekVirtualConferenceDescriptor -> IO (Id EKVirtualConferenceDescriptor)
init_ ekVirtualConferenceDescriptor  =
  sendMsg ekVirtualConferenceDescriptor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id EKVirtualConferenceDescriptor)
new  =
  do
    cls' <- getRequiredClass "EKVirtualConferenceDescriptor"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- title@
title :: IsEKVirtualConferenceDescriptor ekVirtualConferenceDescriptor => ekVirtualConferenceDescriptor -> IO (Id NSString)
title ekVirtualConferenceDescriptor  =
  sendMsg ekVirtualConferenceDescriptor (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- URLDescriptors@
urlDescriptors :: IsEKVirtualConferenceDescriptor ekVirtualConferenceDescriptor => ekVirtualConferenceDescriptor -> IO (Id NSArray)
urlDescriptors ekVirtualConferenceDescriptor  =
  sendMsg ekVirtualConferenceDescriptor (mkSelector "URLDescriptors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- conferenceDetails@
conferenceDetails :: IsEKVirtualConferenceDescriptor ekVirtualConferenceDescriptor => ekVirtualConferenceDescriptor -> IO (Id NSString)
conferenceDetails ekVirtualConferenceDescriptor  =
  sendMsg ekVirtualConferenceDescriptor (mkSelector "conferenceDetails") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTitle:URLDescriptors:conferenceDetails:@
initWithTitle_URLDescriptors_conferenceDetailsSelector :: Selector
initWithTitle_URLDescriptors_conferenceDetailsSelector = mkSelector "initWithTitle:URLDescriptors:conferenceDetails:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @URLDescriptors@
urlDescriptorsSelector :: Selector
urlDescriptorsSelector = mkSelector "URLDescriptors"

-- | @Selector@ for @conferenceDetails@
conferenceDetailsSelector :: Selector
conferenceDetailsSelector = mkSelector "conferenceDetails"

