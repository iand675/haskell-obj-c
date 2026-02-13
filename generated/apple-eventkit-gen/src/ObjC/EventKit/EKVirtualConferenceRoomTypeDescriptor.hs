{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | EKVirtualConferenceRoomTypeDescriptor
--
-- Describes a virtual conference room type.
--
-- Generated bindings for @EKVirtualConferenceRoomTypeDescriptor@.
module ObjC.EventKit.EKVirtualConferenceRoomTypeDescriptor
  ( EKVirtualConferenceRoomTypeDescriptor
  , IsEKVirtualConferenceRoomTypeDescriptor(..)
  , initWithTitle_identifier
  , init_
  , new
  , title
  , identifier
  , identifierSelector
  , initSelector
  , initWithTitle_identifierSelector
  , newSelector
  , titleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.EventKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithTitle:identifier:
--
-- Initializes an instance of EKVirtualConferenceRoomTypeDescriptor.
--
-- @title@ — A user-readable title describing this room type. This string will be                            displayed in UI.
--
-- @identifier@ — An EKVirtualConferenceRoomTypeIdentifier that your extension can use to                             distinguish this room type from the other room types that your extension                            provides. This is chosen by your extension and is passed back to your                            extension if the user chooses to create a virtual conference of the                             associated room type.
--
-- ObjC selector: @- initWithTitle:identifier:@
initWithTitle_identifier :: (IsEKVirtualConferenceRoomTypeDescriptor ekVirtualConferenceRoomTypeDescriptor, IsNSString title, IsNSString identifier) => ekVirtualConferenceRoomTypeDescriptor -> title -> identifier -> IO (Id EKVirtualConferenceRoomTypeDescriptor)
initWithTitle_identifier ekVirtualConferenceRoomTypeDescriptor title identifier =
  sendOwnedMessage ekVirtualConferenceRoomTypeDescriptor initWithTitle_identifierSelector (toNSString title) (toNSString identifier)

-- | @- init@
init_ :: IsEKVirtualConferenceRoomTypeDescriptor ekVirtualConferenceRoomTypeDescriptor => ekVirtualConferenceRoomTypeDescriptor -> IO (Id EKVirtualConferenceRoomTypeDescriptor)
init_ ekVirtualConferenceRoomTypeDescriptor =
  sendOwnedMessage ekVirtualConferenceRoomTypeDescriptor initSelector

-- | @+ new@
new :: IO (Id EKVirtualConferenceRoomTypeDescriptor)
new  =
  do
    cls' <- getRequiredClass "EKVirtualConferenceRoomTypeDescriptor"
    sendOwnedClassMessage cls' newSelector

-- | @- title@
title :: IsEKVirtualConferenceRoomTypeDescriptor ekVirtualConferenceRoomTypeDescriptor => ekVirtualConferenceRoomTypeDescriptor -> IO (Id NSString)
title ekVirtualConferenceRoomTypeDescriptor =
  sendMessage ekVirtualConferenceRoomTypeDescriptor titleSelector

-- | @- identifier@
identifier :: IsEKVirtualConferenceRoomTypeDescriptor ekVirtualConferenceRoomTypeDescriptor => ekVirtualConferenceRoomTypeDescriptor -> IO (Id NSString)
identifier ekVirtualConferenceRoomTypeDescriptor =
  sendMessage ekVirtualConferenceRoomTypeDescriptor identifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTitle:identifier:@
initWithTitle_identifierSelector :: Selector '[Id NSString, Id NSString] (Id EKVirtualConferenceRoomTypeDescriptor)
initWithTitle_identifierSelector = mkSelector "initWithTitle:identifier:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id EKVirtualConferenceRoomTypeDescriptor)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id EKVirtualConferenceRoomTypeDescriptor)
newSelector = mkSelector "new"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

