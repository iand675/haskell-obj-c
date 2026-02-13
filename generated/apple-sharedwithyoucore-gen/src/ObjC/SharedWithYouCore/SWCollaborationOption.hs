{-# LANGUAGE DataKinds #-}
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
  , identifierSelector
  , initSelector
  , initWithTitle_identifierSelector
  , newSelector
  , optionWithTitle_identifierSelector
  , requiredOptionsIdentifiersSelector
  , selectedSelector
  , setRequiredOptionsIdentifiersSelector
  , setSelectedSelector
  , setSubtitleSelector
  , setTitleSelector
  , subtitleSelector
  , titleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithTitle_identifier swCollaborationOption title identifier =
  sendOwnedMessage swCollaborationOption initWithTitle_identifierSelector (toNSString title) (toNSString identifier)

-- | @- init@
init_ :: IsSWCollaborationOption swCollaborationOption => swCollaborationOption -> IO (Id SWCollaborationOption)
init_ swCollaborationOption =
  sendOwnedMessage swCollaborationOption initSelector

-- | @+ new@
new :: IO (Id SWCollaborationOption)
new  =
  do
    cls' <- getRequiredClass "SWCollaborationOption"
    sendOwnedClassMessage cls' newSelector

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
    sendClassMessage cls' optionWithTitle_identifierSelector (toNSString title) (toNSString identifier)

-- | A localized title string to be used when displaying the option
--
-- ObjC selector: @- title@
title :: IsSWCollaborationOption swCollaborationOption => swCollaborationOption -> IO (Id NSString)
title swCollaborationOption =
  sendMessage swCollaborationOption titleSelector

-- | A localized title string to be used when displaying the option
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsSWCollaborationOption swCollaborationOption, IsNSString value) => swCollaborationOption -> value -> IO ()
setTitle swCollaborationOption value =
  sendMessage swCollaborationOption setTitleSelector (toNSString value)

-- | Unique identifier
--
-- ObjC selector: @- identifier@
identifier :: IsSWCollaborationOption swCollaborationOption => swCollaborationOption -> IO (Id NSString)
identifier swCollaborationOption =
  sendMessage swCollaborationOption identifierSelector

-- | A localized subtitle string to be used when displaying the option
--
-- ObjC selector: @- subtitle@
subtitle :: IsSWCollaborationOption swCollaborationOption => swCollaborationOption -> IO (Id NSString)
subtitle swCollaborationOption =
  sendMessage swCollaborationOption subtitleSelector

-- | A localized subtitle string to be used when displaying the option
--
-- ObjC selector: @- setSubtitle:@
setSubtitle :: (IsSWCollaborationOption swCollaborationOption, IsNSString value) => swCollaborationOption -> value -> IO ()
setSubtitle swCollaborationOption value =
  sendMessage swCollaborationOption setSubtitleSelector (toNSString value)

-- | A flag that indicates whether the option is selected.
--
-- This property should only be set directly when the option represents an individual switch. Defaults to NO
--
-- ObjC selector: @- selected@
selected :: IsSWCollaborationOption swCollaborationOption => swCollaborationOption -> IO Bool
selected swCollaborationOption =
  sendMessage swCollaborationOption selectedSelector

-- | A flag that indicates whether the option is selected.
--
-- This property should only be set directly when the option represents an individual switch. Defaults to NO
--
-- ObjC selector: @- setSelected:@
setSelected :: IsSWCollaborationOption swCollaborationOption => swCollaborationOption -> Bool -> IO ()
setSelected swCollaborationOption value =
  sendMessage swCollaborationOption setSelectedSelector value

-- | An array of option identifiers that must already be selected in order to be interacted with
--
-- ObjC selector: @- requiredOptionsIdentifiers@
requiredOptionsIdentifiers :: IsSWCollaborationOption swCollaborationOption => swCollaborationOption -> IO (Id NSArray)
requiredOptionsIdentifiers swCollaborationOption =
  sendMessage swCollaborationOption requiredOptionsIdentifiersSelector

-- | An array of option identifiers that must already be selected in order to be interacted with
--
-- ObjC selector: @- setRequiredOptionsIdentifiers:@
setRequiredOptionsIdentifiers :: (IsSWCollaborationOption swCollaborationOption, IsNSArray value) => swCollaborationOption -> value -> IO ()
setRequiredOptionsIdentifiers swCollaborationOption value =
  sendMessage swCollaborationOption setRequiredOptionsIdentifiersSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTitle:identifier:@
initWithTitle_identifierSelector :: Selector '[Id NSString, Id NSString] (Id SWCollaborationOption)
initWithTitle_identifierSelector = mkSelector "initWithTitle:identifier:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SWCollaborationOption)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SWCollaborationOption)
newSelector = mkSelector "new"

-- | @Selector@ for @optionWithTitle:identifier:@
optionWithTitle_identifierSelector :: Selector '[Id NSString, Id NSString] (Id SWCollaborationOption)
optionWithTitle_identifierSelector = mkSelector "optionWithTitle:identifier:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @subtitle@
subtitleSelector :: Selector '[] (Id NSString)
subtitleSelector = mkSelector "subtitle"

-- | @Selector@ for @setSubtitle:@
setSubtitleSelector :: Selector '[Id NSString] ()
setSubtitleSelector = mkSelector "setSubtitle:"

-- | @Selector@ for @selected@
selectedSelector :: Selector '[] Bool
selectedSelector = mkSelector "selected"

-- | @Selector@ for @setSelected:@
setSelectedSelector :: Selector '[Bool] ()
setSelectedSelector = mkSelector "setSelected:"

-- | @Selector@ for @requiredOptionsIdentifiers@
requiredOptionsIdentifiersSelector :: Selector '[] (Id NSArray)
requiredOptionsIdentifiersSelector = mkSelector "requiredOptionsIdentifiers"

-- | @Selector@ for @setRequiredOptionsIdentifiers:@
setRequiredOptionsIdentifiersSelector :: Selector '[Id NSArray] ()
setRequiredOptionsIdentifiersSelector = mkSelector "setRequiredOptionsIdentifiers:"

