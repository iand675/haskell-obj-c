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
  , notesSelector
  , setNotesSelector
  , titleSelector
  , setTitleSelector
  , typeSelector
  , uidSelector
  , isEditableSelector


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

import ObjC.CalendarStore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ calendar@
calendar :: IO RawId
calendar  =
  do
    cls' <- getRequiredClass "CalCalendar"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "calendar") (retPtr retVoid) []

-- | @- notes@
notes :: IsCalCalendar calCalendar => calCalendar -> IO (Id NSString)
notes calCalendar  =
  sendMsg calCalendar (mkSelector "notes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNotes:@
setNotes :: (IsCalCalendar calCalendar, IsNSString value) => calCalendar -> value -> IO ()
setNotes calCalendar  value =
withObjCPtr value $ \raw_value ->
    sendMsg calCalendar (mkSelector "setNotes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- title@
title :: IsCalCalendar calCalendar => calCalendar -> IO (Id NSString)
title calCalendar  =
  sendMsg calCalendar (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitle:@
setTitle :: (IsCalCalendar calCalendar, IsNSString value) => calCalendar -> value -> IO ()
setTitle calCalendar  value =
withObjCPtr value $ \raw_value ->
    sendMsg calCalendar (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- type@
type_ :: IsCalCalendar calCalendar => calCalendar -> IO (Id NSString)
type_ calCalendar  =
  sendMsg calCalendar (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- uid@
uid :: IsCalCalendar calCalendar => calCalendar -> IO (Id NSString)
uid calCalendar  =
  sendMsg calCalendar (mkSelector "uid") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- isEditable@
isEditable :: IsCalCalendar calCalendar => calCalendar -> IO Bool
isEditable calCalendar  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg calCalendar (mkSelector "isEditable") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @calendar@
calendarSelector :: Selector
calendarSelector = mkSelector "calendar"

-- | @Selector@ for @notes@
notesSelector :: Selector
notesSelector = mkSelector "notes"

-- | @Selector@ for @setNotes:@
setNotesSelector :: Selector
setNotesSelector = mkSelector "setNotes:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @uid@
uidSelector :: Selector
uidSelector = mkSelector "uid"

-- | @Selector@ for @isEditable@
isEditableSelector :: Selector
isEditableSelector = mkSelector "isEditable"

