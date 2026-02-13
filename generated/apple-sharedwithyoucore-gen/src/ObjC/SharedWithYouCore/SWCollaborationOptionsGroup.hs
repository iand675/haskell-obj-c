{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SWCollaborationOptionsGroup
--
-- A group of SWCollaborationOptions that should be displayed and configured together
--
-- Use SWCollaborationOptionsGroup to represent a group of options used to configure a collaborative item. An SWCollaborationOptionsGroup with one option indicates a switch.
--
-- Generated bindings for @SWCollaborationOptionsGroup@.
module ObjC.SharedWithYouCore.SWCollaborationOptionsGroup
  ( SWCollaborationOptionsGroup
  , IsSWCollaborationOptionsGroup(..)
  , initWithIdentifier_options
  , optionsGroupWithIdentifier_options
  , init_
  , new
  , title
  , setTitle
  , identifier
  , footer
  , setFooter
  , options
  , setOptions
  , footerSelector
  , identifierSelector
  , initSelector
  , initWithIdentifier_optionsSelector
  , newSelector
  , optionsGroupWithIdentifier_optionsSelector
  , optionsSelector
  , setFooterSelector
  , setOptionsSelector
  , setTitleSelector
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

-- | Initializes a new option group
--
-- @identifier@ — unique identifier for the group
--
-- @options@ — SWCollaborationOptions to display in the section
--
-- ObjC selector: @- initWithIdentifier:options:@
initWithIdentifier_options :: (IsSWCollaborationOptionsGroup swCollaborationOptionsGroup, IsNSString identifier, IsNSArray options) => swCollaborationOptionsGroup -> identifier -> options -> IO (Id SWCollaborationOptionsGroup)
initWithIdentifier_options swCollaborationOptionsGroup identifier options =
  sendOwnedMessage swCollaborationOptionsGroup initWithIdentifier_optionsSelector (toNSString identifier) (toNSArray options)

-- | Initializes a new option group
--
-- @identifier@ — unique identifier for the group
--
-- @options@ — SWCollaborationOptions to display in the group
--
-- ObjC selector: @+ optionsGroupWithIdentifier:options:@
optionsGroupWithIdentifier_options :: (IsNSString identifier, IsNSArray options) => identifier -> options -> IO (Id SWCollaborationOptionsGroup)
optionsGroupWithIdentifier_options identifier options =
  do
    cls' <- getRequiredClass "SWCollaborationOptionsGroup"
    sendClassMessage cls' optionsGroupWithIdentifier_optionsSelector (toNSString identifier) (toNSArray options)

-- | @- init@
init_ :: IsSWCollaborationOptionsGroup swCollaborationOptionsGroup => swCollaborationOptionsGroup -> IO (Id SWCollaborationOptionsGroup)
init_ swCollaborationOptionsGroup =
  sendOwnedMessage swCollaborationOptionsGroup initSelector

-- | @+ new@
new :: IO (Id SWCollaborationOptionsGroup)
new  =
  do
    cls' <- getRequiredClass "SWCollaborationOptionsGroup"
    sendOwnedClassMessage cls' newSelector

-- | Localized string used to title the section
--
-- ObjC selector: @- title@
title :: IsSWCollaborationOptionsGroup swCollaborationOptionsGroup => swCollaborationOptionsGroup -> IO (Id NSString)
title swCollaborationOptionsGroup =
  sendMessage swCollaborationOptionsGroup titleSelector

-- | Localized string used to title the section
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsSWCollaborationOptionsGroup swCollaborationOptionsGroup, IsNSString value) => swCollaborationOptionsGroup -> value -> IO ()
setTitle swCollaborationOptionsGroup value =
  sendMessage swCollaborationOptionsGroup setTitleSelector (toNSString value)

-- | A unique identifier
--
-- ObjC selector: @- identifier@
identifier :: IsSWCollaborationOptionsGroup swCollaborationOptionsGroup => swCollaborationOptionsGroup -> IO (Id NSString)
identifier swCollaborationOptionsGroup =
  sendMessage swCollaborationOptionsGroup identifierSelector

-- | Localized string to describe or provide additional information about the group of options
--
-- ObjC selector: @- footer@
footer :: IsSWCollaborationOptionsGroup swCollaborationOptionsGroup => swCollaborationOptionsGroup -> IO (Id NSString)
footer swCollaborationOptionsGroup =
  sendMessage swCollaborationOptionsGroup footerSelector

-- | Localized string to describe or provide additional information about the group of options
--
-- ObjC selector: @- setFooter:@
setFooter :: (IsSWCollaborationOptionsGroup swCollaborationOptionsGroup, IsNSString value) => swCollaborationOptionsGroup -> value -> IO ()
setFooter swCollaborationOptionsGroup value =
  sendMessage swCollaborationOptionsGroup setFooterSelector (toNSString value)

-- | SWCollaborationOptions to be displayed in the group
--
-- ObjC selector: @- options@
options :: IsSWCollaborationOptionsGroup swCollaborationOptionsGroup => swCollaborationOptionsGroup -> IO (Id NSArray)
options swCollaborationOptionsGroup =
  sendMessage swCollaborationOptionsGroup optionsSelector

-- | SWCollaborationOptions to be displayed in the group
--
-- ObjC selector: @- setOptions:@
setOptions :: (IsSWCollaborationOptionsGroup swCollaborationOptionsGroup, IsNSArray value) => swCollaborationOptionsGroup -> value -> IO ()
setOptions swCollaborationOptionsGroup value =
  sendMessage swCollaborationOptionsGroup setOptionsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifier:options:@
initWithIdentifier_optionsSelector :: Selector '[Id NSString, Id NSArray] (Id SWCollaborationOptionsGroup)
initWithIdentifier_optionsSelector = mkSelector "initWithIdentifier:options:"

-- | @Selector@ for @optionsGroupWithIdentifier:options:@
optionsGroupWithIdentifier_optionsSelector :: Selector '[Id NSString, Id NSArray] (Id SWCollaborationOptionsGroup)
optionsGroupWithIdentifier_optionsSelector = mkSelector "optionsGroupWithIdentifier:options:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SWCollaborationOptionsGroup)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SWCollaborationOptionsGroup)
newSelector = mkSelector "new"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @footer@
footerSelector :: Selector '[] (Id NSString)
footerSelector = mkSelector "footer"

-- | @Selector@ for @setFooter:@
setFooterSelector :: Selector '[Id NSString] ()
setFooterSelector = mkSelector "setFooter:"

-- | @Selector@ for @options@
optionsSelector :: Selector '[] (Id NSArray)
optionsSelector = mkSelector "options"

-- | @Selector@ for @setOptions:@
setOptionsSelector :: Selector '[Id NSArray] ()
setOptionsSelector = mkSelector "setOptions:"

