{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CalCalendar@.
module ObjC.CalendarStore.CalCalendar
  ( CalCalendar
  , IsCalCalendar(..)
  , calendar
  , notes
  , setNotes
  , title
  , setTitle
  , type_
  , uid
  , isEditable
  , calendarSelector
  , isEditableSelector
  , notesSelector
  , setNotesSelector
  , setTitleSelector
  , titleSelector
  , typeSelector
  , uidSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CalendarStore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ calendar@
calendar :: IO RawId
calendar  =
  do
    cls' <- getRequiredClass "CalCalendar"
    sendClassMessage cls' calendarSelector

-- | @- notes@
notes :: IsCalCalendar calCalendar => calCalendar -> IO (Id NSString)
notes calCalendar =
  sendMessage calCalendar notesSelector

-- | @- setNotes:@
setNotes :: (IsCalCalendar calCalendar, IsNSString value) => calCalendar -> value -> IO ()
setNotes calCalendar value =
  sendMessage calCalendar setNotesSelector (toNSString value)

-- | @- title@
title :: IsCalCalendar calCalendar => calCalendar -> IO (Id NSString)
title calCalendar =
  sendMessage calCalendar titleSelector

-- | @- setTitle:@
setTitle :: (IsCalCalendar calCalendar, IsNSString value) => calCalendar -> value -> IO ()
setTitle calCalendar value =
  sendMessage calCalendar setTitleSelector (toNSString value)

-- | @- type@
type_ :: IsCalCalendar calCalendar => calCalendar -> IO (Id NSString)
type_ calCalendar =
  sendMessage calCalendar typeSelector

-- | @- uid@
uid :: IsCalCalendar calCalendar => calCalendar -> IO (Id NSString)
uid calCalendar =
  sendMessage calCalendar uidSelector

-- | @- isEditable@
isEditable :: IsCalCalendar calCalendar => calCalendar -> IO Bool
isEditable calCalendar =
  sendMessage calCalendar isEditableSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @calendar@
calendarSelector :: Selector '[] RawId
calendarSelector = mkSelector "calendar"

-- | @Selector@ for @notes@
notesSelector :: Selector '[] (Id NSString)
notesSelector = mkSelector "notes"

-- | @Selector@ for @setNotes:@
setNotesSelector :: Selector '[Id NSString] ()
setNotesSelector = mkSelector "setNotes:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSString)
typeSelector = mkSelector "type"

-- | @Selector@ for @uid@
uidSelector :: Selector '[] (Id NSString)
uidSelector = mkSelector "uid"

-- | @Selector@ for @isEditable@
isEditableSelector :: Selector '[] Bool
isEditableSelector = mkSelector "isEditable"

