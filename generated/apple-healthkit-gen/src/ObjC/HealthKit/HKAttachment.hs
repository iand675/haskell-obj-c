{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKAttachment
--
-- An HKAttachment represents a file attachment stored in the HealthKit database.
--
-- Generated bindings for @HKAttachment@.
module ObjC.HealthKit.HKAttachment
  ( HKAttachment
  , IsHKAttachment(..)
  , init_
  , new
  , identifier
  , name
  , contentType
  , size
  , creationDate
  , metadata
  , contentTypeSelector
  , creationDateSelector
  , identifierSelector
  , initSelector
  , metadataSelector
  , nameSelector
  , newSelector
  , sizeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- | init
--
-- The init method is unavailable. To create an attachment, use HKAttachmentStore.
--
-- ObjC selector: @- init@
init_ :: IsHKAttachment hkAttachment => hkAttachment -> IO (Id HKAttachment)
init_ hkAttachment =
  sendOwnedMessage hkAttachment initSelector

-- | new
--
-- The new method is unavailable. To create an attachment, use HKAttachmentStore.
--
-- ObjC selector: @+ new@
new :: IO (Id HKAttachment)
new  =
  do
    cls' <- getRequiredClass "HKAttachment"
    sendOwnedClassMessage cls' newSelector

-- | identifier
--
-- A unique identifier of the receiver in the HealthKit database.
--
-- ObjC selector: @- identifier@
identifier :: IsHKAttachment hkAttachment => hkAttachment -> IO (Id NSUUID)
identifier hkAttachment =
  sendMessage hkAttachment identifierSelector

-- | name
--
-- Represents the name of the file.
--
-- ObjC selector: @- name@
name :: IsHKAttachment hkAttachment => hkAttachment -> IO (Id NSString)
name hkAttachment =
  sendMessage hkAttachment nameSelector

-- | contentType
--
-- The Uniform Type of the file.
--
-- ObjC selector: @- contentType@
contentType :: IsHKAttachment hkAttachment => hkAttachment -> IO (Id UTType)
contentType hkAttachment =
  sendMessage hkAttachment contentTypeSelector

-- | size
--
-- The size in bytes of the file.
--
-- ObjC selector: @- size@
size :: IsHKAttachment hkAttachment => hkAttachment -> IO CLong
size hkAttachment =
  sendMessage hkAttachment sizeSelector

-- | creationDate
--
-- The date the receiver was created.
--
-- ObjC selector: @- creationDate@
creationDate :: IsHKAttachment hkAttachment => hkAttachment -> IO (Id NSDate)
creationDate hkAttachment =
  sendMessage hkAttachment creationDateSelector

-- | metadata
--
-- Extra information describing the attachment.
--
-- Keys must be NSString and values must be either NSString, NSNumber, or NSDate.
--
-- ObjC selector: @- metadata@
metadata :: IsHKAttachment hkAttachment => hkAttachment -> IO (Id NSDictionary)
metadata hkAttachment =
  sendMessage hkAttachment metadataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKAttachment)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id HKAttachment)
newSelector = mkSelector "new"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSUUID)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @contentType@
contentTypeSelector :: Selector '[] (Id UTType)
contentTypeSelector = mkSelector "contentType"

-- | @Selector@ for @size@
sizeSelector :: Selector '[] CLong
sizeSelector = mkSelector "size"

-- | @Selector@ for @creationDate@
creationDateSelector :: Selector '[] (Id NSDate)
creationDateSelector = mkSelector "creationDate"

-- | @Selector@ for @metadata@
metadataSelector :: Selector '[] (Id NSDictionary)
metadataSelector = mkSelector "metadata"

