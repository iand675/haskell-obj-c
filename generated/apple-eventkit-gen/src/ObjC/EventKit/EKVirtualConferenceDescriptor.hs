{-# LANGUAGE DataKinds #-}
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
  , conferenceDetailsSelector
  , initSelector
  , initWithTitle_URLDescriptors_conferenceDetailsSelector
  , newSelector
  , titleSelector
  , urlDescriptorsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithTitle_URLDescriptors_conferenceDetails ekVirtualConferenceDescriptor title urlDescriptors conferenceDetails =
  sendOwnedMessage ekVirtualConferenceDescriptor initWithTitle_URLDescriptors_conferenceDetailsSelector (toNSString title) (toNSArray urlDescriptors) (toNSString conferenceDetails)

-- | @- init@
init_ :: IsEKVirtualConferenceDescriptor ekVirtualConferenceDescriptor => ekVirtualConferenceDescriptor -> IO (Id EKVirtualConferenceDescriptor)
init_ ekVirtualConferenceDescriptor =
  sendOwnedMessage ekVirtualConferenceDescriptor initSelector

-- | @+ new@
new :: IO (Id EKVirtualConferenceDescriptor)
new  =
  do
    cls' <- getRequiredClass "EKVirtualConferenceDescriptor"
    sendOwnedClassMessage cls' newSelector

-- | @- title@
title :: IsEKVirtualConferenceDescriptor ekVirtualConferenceDescriptor => ekVirtualConferenceDescriptor -> IO (Id NSString)
title ekVirtualConferenceDescriptor =
  sendMessage ekVirtualConferenceDescriptor titleSelector

-- | @- URLDescriptors@
urlDescriptors :: IsEKVirtualConferenceDescriptor ekVirtualConferenceDescriptor => ekVirtualConferenceDescriptor -> IO (Id NSArray)
urlDescriptors ekVirtualConferenceDescriptor =
  sendMessage ekVirtualConferenceDescriptor urlDescriptorsSelector

-- | @- conferenceDetails@
conferenceDetails :: IsEKVirtualConferenceDescriptor ekVirtualConferenceDescriptor => ekVirtualConferenceDescriptor -> IO (Id NSString)
conferenceDetails ekVirtualConferenceDescriptor =
  sendMessage ekVirtualConferenceDescriptor conferenceDetailsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTitle:URLDescriptors:conferenceDetails:@
initWithTitle_URLDescriptors_conferenceDetailsSelector :: Selector '[Id NSString, Id NSArray, Id NSString] (Id EKVirtualConferenceDescriptor)
initWithTitle_URLDescriptors_conferenceDetailsSelector = mkSelector "initWithTitle:URLDescriptors:conferenceDetails:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id EKVirtualConferenceDescriptor)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id EKVirtualConferenceDescriptor)
newSelector = mkSelector "new"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @URLDescriptors@
urlDescriptorsSelector :: Selector '[] (Id NSArray)
urlDescriptorsSelector = mkSelector "URLDescriptors"

-- | @Selector@ for @conferenceDetails@
conferenceDetailsSelector :: Selector '[] (Id NSString)
conferenceDetailsSelector = mkSelector "conferenceDetails"

