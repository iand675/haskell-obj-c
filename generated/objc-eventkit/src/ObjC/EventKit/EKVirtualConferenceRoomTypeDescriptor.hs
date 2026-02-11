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
  , initWithTitle_identifierSelector
  , initSelector
  , newSelector
  , titleSelector
  , identifierSelector


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
initWithTitle_identifier ekVirtualConferenceRoomTypeDescriptor  title identifier =
withObjCPtr title $ \raw_title ->
  withObjCPtr identifier $ \raw_identifier ->
      sendMsg ekVirtualConferenceRoomTypeDescriptor (mkSelector "initWithTitle:identifier:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsEKVirtualConferenceRoomTypeDescriptor ekVirtualConferenceRoomTypeDescriptor => ekVirtualConferenceRoomTypeDescriptor -> IO (Id EKVirtualConferenceRoomTypeDescriptor)
init_ ekVirtualConferenceRoomTypeDescriptor  =
  sendMsg ekVirtualConferenceRoomTypeDescriptor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id EKVirtualConferenceRoomTypeDescriptor)
new  =
  do
    cls' <- getRequiredClass "EKVirtualConferenceRoomTypeDescriptor"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- title@
title :: IsEKVirtualConferenceRoomTypeDescriptor ekVirtualConferenceRoomTypeDescriptor => ekVirtualConferenceRoomTypeDescriptor -> IO (Id NSString)
title ekVirtualConferenceRoomTypeDescriptor  =
  sendMsg ekVirtualConferenceRoomTypeDescriptor (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- identifier@
identifier :: IsEKVirtualConferenceRoomTypeDescriptor ekVirtualConferenceRoomTypeDescriptor => ekVirtualConferenceRoomTypeDescriptor -> IO (Id NSString)
identifier ekVirtualConferenceRoomTypeDescriptor  =
  sendMsg ekVirtualConferenceRoomTypeDescriptor (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTitle:identifier:@
initWithTitle_identifierSelector :: Selector
initWithTitle_identifierSelector = mkSelector "initWithTitle:identifier:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

