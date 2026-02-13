{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @EKSource@.
module ObjC.EventKit.EKSource
  ( EKSource
  , IsEKSource(..)
  , calendarsForEntityType
  , sourceIdentifier
  , sourceType
  , title
  , isDelegate
  , calendarsForEntityTypeSelector
  , isDelegateSelector
  , sourceIdentifierSelector
  , sourceTypeSelector
  , titleSelector

  -- * Enum types
  , EKEntityType(EKEntityType)
  , pattern EKEntityTypeEvent
  , pattern EKEntityTypeReminder
  , EKSourceType(EKSourceType)
  , pattern EKSourceTypeLocal
  , pattern EKSourceTypeExchange
  , pattern EKSourceTypeCalDAV
  , pattern EKSourceTypeMobileMe
  , pattern EKSourceTypeSubscribed
  , pattern EKSourceTypeBirthdays

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.EventKit.Internal.Classes
import ObjC.EventKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | calendarsForEntityType
--
-- Returns the calendars that belong to this source that               support a given entity type (reminders, events)
--
-- ObjC selector: @- calendarsForEntityType:@
calendarsForEntityType :: IsEKSource ekSource => ekSource -> EKEntityType -> IO (Id NSSet)
calendarsForEntityType ekSource entityType =
  sendMessage ekSource calendarsForEntityTypeSelector entityType

-- | @- sourceIdentifier@
sourceIdentifier :: IsEKSource ekSource => ekSource -> IO (Id NSString)
sourceIdentifier ekSource =
  sendMessage ekSource sourceIdentifierSelector

-- | @- sourceType@
sourceType :: IsEKSource ekSource => ekSource -> IO EKSourceType
sourceType ekSource =
  sendMessage ekSource sourceTypeSelector

-- | @- title@
title :: IsEKSource ekSource => ekSource -> IO (Id NSString)
title ekSource =
  sendMessage ekSource titleSelector

-- | isDelegate
--
-- Returns YES if this EKSource represents an account delegated by another user.
--
-- ObjC selector: @- isDelegate@
isDelegate :: IsEKSource ekSource => ekSource -> IO Bool
isDelegate ekSource =
  sendMessage ekSource isDelegateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @calendarsForEntityType:@
calendarsForEntityTypeSelector :: Selector '[EKEntityType] (Id NSSet)
calendarsForEntityTypeSelector = mkSelector "calendarsForEntityType:"

-- | @Selector@ for @sourceIdentifier@
sourceIdentifierSelector :: Selector '[] (Id NSString)
sourceIdentifierSelector = mkSelector "sourceIdentifier"

-- | @Selector@ for @sourceType@
sourceTypeSelector :: Selector '[] EKSourceType
sourceTypeSelector = mkSelector "sourceType"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @isDelegate@
isDelegateSelector :: Selector '[] Bool
isDelegateSelector = mkSelector "isDelegate"

