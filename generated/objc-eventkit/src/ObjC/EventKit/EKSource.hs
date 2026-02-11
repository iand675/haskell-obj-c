{-# LANGUAGE PatternSynonyms #-}
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
  , sourceIdentifierSelector
  , sourceTypeSelector
  , titleSelector
  , isDelegateSelector

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
import ObjC.EventKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | calendarsForEntityType
--
-- Returns the calendars that belong to this source that               support a given entity type (reminders, events)
--
-- ObjC selector: @- calendarsForEntityType:@
calendarsForEntityType :: IsEKSource ekSource => ekSource -> EKEntityType -> IO (Id NSSet)
calendarsForEntityType ekSource  entityType =
  sendMsg ekSource (mkSelector "calendarsForEntityType:") (retPtr retVoid) [argCULong (coerce entityType)] >>= retainedObject . castPtr

-- | @- sourceIdentifier@
sourceIdentifier :: IsEKSource ekSource => ekSource -> IO (Id NSString)
sourceIdentifier ekSource  =
  sendMsg ekSource (mkSelector "sourceIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sourceType@
sourceType :: IsEKSource ekSource => ekSource -> IO EKSourceType
sourceType ekSource  =
  fmap (coerce :: CLong -> EKSourceType) $ sendMsg ekSource (mkSelector "sourceType") retCLong []

-- | @- title@
title :: IsEKSource ekSource => ekSource -> IO (Id NSString)
title ekSource  =
  sendMsg ekSource (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | isDelegate
--
-- Returns YES if this EKSource represents an account delegated by another user.
--
-- ObjC selector: @- isDelegate@
isDelegate :: IsEKSource ekSource => ekSource -> IO Bool
isDelegate ekSource  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ekSource (mkSelector "isDelegate") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @calendarsForEntityType:@
calendarsForEntityTypeSelector :: Selector
calendarsForEntityTypeSelector = mkSelector "calendarsForEntityType:"

-- | @Selector@ for @sourceIdentifier@
sourceIdentifierSelector :: Selector
sourceIdentifierSelector = mkSelector "sourceIdentifier"

-- | @Selector@ for @sourceType@
sourceTypeSelector :: Selector
sourceTypeSelector = mkSelector "sourceType"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @isDelegate@
isDelegateSelector :: Selector
isDelegateSelector = mkSelector "isDelegate"

