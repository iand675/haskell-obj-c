{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SWCollaborationOption
--
-- A user-facing option for configuring a shareable collaborative item
--
-- SWCollaborationOptions represent the available settings (such as permissions) a user can configure on a collaborative item
--
-- Generated bindings for @SWCollaborationOption@.
module ObjC.SharedWithYouCore.SWCollaborationOption
  ( SWCollaborationOption
  , IsSWCollaborationOption(..)
  , initWithTitle_identifier
  , init_
  , new
  , optionWithTitle_identifier
  , title
  , setTitle
  , identifier
  , subtitle
  , setSubtitle
  , selected
  , setSelected
  , requiredOptionsIdentifiers
  , setRequiredOptionsIdentifiers
  , initWithTitle_identifierSelector
  , initSelector
  , newSelector
  , optionWithTitle_identifierSelector
  , titleSelector
  , setTitleSelector
  , identifierSelector
  , subtitleSelector
  , setSubtitleSelector
  , selectedSelector
  , setSelectedSelector
  , requiredOptionsIdentifiersSelector
  , setRequiredOptionsIdentifiersSelector


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

import ObjC.SharedWithYouCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes a collaboration option object with a title and unique identifier
--
-- @title@ — A localized title string to be used when displaying the option
--
-- @identifier@ — The unique identifier for the option
--
-- ObjC selector: @- initWithTitle:identifier:@
initWithTitle_identifier :: (IsSWCollaborationOption swCollaborationOption, IsNSString title, IsNSString identifier) => swCollaborationOption -> title -> identifier -> IO (Id SWCollaborationOption)
initWithTitle_identifier swCollaborationOption  title identifier =
withObjCPtr title $ \raw_title ->
  withObjCPtr identifier $ \raw_identifier ->
      sendMsg swCollaborationOption (mkSelector "initWithTitle:identifier:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsSWCollaborationOption swCollaborationOption => swCollaborationOption -> IO (Id SWCollaborationOption)
init_ swCollaborationOption  =
  sendMsg swCollaborationOption (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SWCollaborationOption)
new  =
  do
    cls' <- getRequiredClass "SWCollaborationOption"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Initializes a collaboration option with a title and unique identifier
--
-- @title@ — A localized title string to be used when displaying the option
--
-- @identifier@ — The unique identifier for the option
--
-- ObjC selector: @+ optionWithTitle:identifier:@
optionWithTitle_identifier :: (IsNSString title, IsNSString identifier) => title -> identifier -> IO (Id SWCollaborationOption)
optionWithTitle_identifier title identifier =
  do
    cls' <- getRequiredClass "SWCollaborationOption"
    withObjCPtr title $ \raw_title ->
      withObjCPtr identifier $ \raw_identifier ->
        sendClassMsg cls' (mkSelector "optionWithTitle:identifier:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

-- | A localized title string to be used when displaying the option
--
-- ObjC selector: @- title@
title :: IsSWCollaborationOption swCollaborationOption => swCollaborationOption -> IO (Id NSString)
title swCollaborationOption  =
  sendMsg swCollaborationOption (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A localized title string to be used when displaying the option
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsSWCollaborationOption swCollaborationOption, IsNSString value) => swCollaborationOption -> value -> IO ()
setTitle swCollaborationOption  value =
withObjCPtr value $ \raw_value ->
    sendMsg swCollaborationOption (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Unique identifier
--
-- ObjC selector: @- identifier@
identifier :: IsSWCollaborationOption swCollaborationOption => swCollaborationOption -> IO (Id NSString)
identifier swCollaborationOption  =
  sendMsg swCollaborationOption (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A localized subtitle string to be used when displaying the option
--
-- ObjC selector: @- subtitle@
subtitle :: IsSWCollaborationOption swCollaborationOption => swCollaborationOption -> IO (Id NSString)
subtitle swCollaborationOption  =
  sendMsg swCollaborationOption (mkSelector "subtitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A localized subtitle string to be used when displaying the option
--
-- ObjC selector: @- setSubtitle:@
setSubtitle :: (IsSWCollaborationOption swCollaborationOption, IsNSString value) => swCollaborationOption -> value -> IO ()
setSubtitle swCollaborationOption  value =
withObjCPtr value $ \raw_value ->
    sendMsg swCollaborationOption (mkSelector "setSubtitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A flag that indicates whether the option is selected.
--
-- This property should only be set directly when the option represents an individual switch. Defaults to NO
--
-- ObjC selector: @- selected@
selected :: IsSWCollaborationOption swCollaborationOption => swCollaborationOption -> IO Bool
selected swCollaborationOption  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg swCollaborationOption (mkSelector "selected") retCULong []

-- | A flag that indicates whether the option is selected.
--
-- This property should only be set directly when the option represents an individual switch. Defaults to NO
--
-- ObjC selector: @- setSelected:@
setSelected :: IsSWCollaborationOption swCollaborationOption => swCollaborationOption -> Bool -> IO ()
setSelected swCollaborationOption  value =
  sendMsg swCollaborationOption (mkSelector "setSelected:") retVoid [argCULong (if value then 1 else 0)]

-- | An array of option identifiers that must already be selected in order to be interacted with
--
-- ObjC selector: @- requiredOptionsIdentifiers@
requiredOptionsIdentifiers :: IsSWCollaborationOption swCollaborationOption => swCollaborationOption -> IO (Id NSArray)
requiredOptionsIdentifiers swCollaborationOption  =
  sendMsg swCollaborationOption (mkSelector "requiredOptionsIdentifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An array of option identifiers that must already be selected in order to be interacted with
--
-- ObjC selector: @- setRequiredOptionsIdentifiers:@
setRequiredOptionsIdentifiers :: (IsSWCollaborationOption swCollaborationOption, IsNSArray value) => swCollaborationOption -> value -> IO ()
setRequiredOptionsIdentifiers swCollaborationOption  value =
withObjCPtr value $ \raw_value ->
    sendMsg swCollaborationOption (mkSelector "setRequiredOptionsIdentifiers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @optionWithTitle:identifier:@
optionWithTitle_identifierSelector :: Selector
optionWithTitle_identifierSelector = mkSelector "optionWithTitle:identifier:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @subtitle@
subtitleSelector :: Selector
subtitleSelector = mkSelector "subtitle"

-- | @Selector@ for @setSubtitle:@
setSubtitleSelector :: Selector
setSubtitleSelector = mkSelector "setSubtitle:"

-- | @Selector@ for @selected@
selectedSelector :: Selector
selectedSelector = mkSelector "selected"

-- | @Selector@ for @setSelected:@
setSelectedSelector :: Selector
setSelectedSelector = mkSelector "setSelected:"

-- | @Selector@ for @requiredOptionsIdentifiers@
requiredOptionsIdentifiersSelector :: Selector
requiredOptionsIdentifiersSelector = mkSelector "requiredOptionsIdentifiers"

-- | @Selector@ for @setRequiredOptionsIdentifiers:@
setRequiredOptionsIdentifiersSelector :: Selector
setRequiredOptionsIdentifiersSelector = mkSelector "setRequiredOptionsIdentifiers:"

