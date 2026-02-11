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
  , initWithIdentifier_optionsSelector
  , optionsGroupWithIdentifier_optionsSelector
  , initSelector
  , newSelector
  , titleSelector
  , setTitleSelector
  , identifierSelector
  , footerSelector
  , setFooterSelector
  , optionsSelector
  , setOptionsSelector


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

-- | Initializes a new option group
--
-- @identifier@ — unique identifier for the group
--
-- @options@ — SWCollaborationOptions to display in the section
--
-- ObjC selector: @- initWithIdentifier:options:@
initWithIdentifier_options :: (IsSWCollaborationOptionsGroup swCollaborationOptionsGroup, IsNSString identifier, IsNSArray options) => swCollaborationOptionsGroup -> identifier -> options -> IO (Id SWCollaborationOptionsGroup)
initWithIdentifier_options swCollaborationOptionsGroup  identifier options =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr options $ \raw_options ->
      sendMsg swCollaborationOptionsGroup (mkSelector "initWithIdentifier:options:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

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
    withObjCPtr identifier $ \raw_identifier ->
      withObjCPtr options $ \raw_options ->
        sendClassMsg cls' (mkSelector "optionsGroupWithIdentifier:options:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsSWCollaborationOptionsGroup swCollaborationOptionsGroup => swCollaborationOptionsGroup -> IO (Id SWCollaborationOptionsGroup)
init_ swCollaborationOptionsGroup  =
  sendMsg swCollaborationOptionsGroup (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SWCollaborationOptionsGroup)
new  =
  do
    cls' <- getRequiredClass "SWCollaborationOptionsGroup"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Localized string used to title the section
--
-- ObjC selector: @- title@
title :: IsSWCollaborationOptionsGroup swCollaborationOptionsGroup => swCollaborationOptionsGroup -> IO (Id NSString)
title swCollaborationOptionsGroup  =
  sendMsg swCollaborationOptionsGroup (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Localized string used to title the section
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsSWCollaborationOptionsGroup swCollaborationOptionsGroup, IsNSString value) => swCollaborationOptionsGroup -> value -> IO ()
setTitle swCollaborationOptionsGroup  value =
withObjCPtr value $ \raw_value ->
    sendMsg swCollaborationOptionsGroup (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A unique identifier
--
-- ObjC selector: @- identifier@
identifier :: IsSWCollaborationOptionsGroup swCollaborationOptionsGroup => swCollaborationOptionsGroup -> IO (Id NSString)
identifier swCollaborationOptionsGroup  =
  sendMsg swCollaborationOptionsGroup (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Localized string to describe or provide additional information about the group of options
--
-- ObjC selector: @- footer@
footer :: IsSWCollaborationOptionsGroup swCollaborationOptionsGroup => swCollaborationOptionsGroup -> IO (Id NSString)
footer swCollaborationOptionsGroup  =
  sendMsg swCollaborationOptionsGroup (mkSelector "footer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Localized string to describe or provide additional information about the group of options
--
-- ObjC selector: @- setFooter:@
setFooter :: (IsSWCollaborationOptionsGroup swCollaborationOptionsGroup, IsNSString value) => swCollaborationOptionsGroup -> value -> IO ()
setFooter swCollaborationOptionsGroup  value =
withObjCPtr value $ \raw_value ->
    sendMsg swCollaborationOptionsGroup (mkSelector "setFooter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | SWCollaborationOptions to be displayed in the group
--
-- ObjC selector: @- options@
options :: IsSWCollaborationOptionsGroup swCollaborationOptionsGroup => swCollaborationOptionsGroup -> IO (Id NSArray)
options swCollaborationOptionsGroup  =
  sendMsg swCollaborationOptionsGroup (mkSelector "options") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | SWCollaborationOptions to be displayed in the group
--
-- ObjC selector: @- setOptions:@
setOptions :: (IsSWCollaborationOptionsGroup swCollaborationOptionsGroup, IsNSArray value) => swCollaborationOptionsGroup -> value -> IO ()
setOptions swCollaborationOptionsGroup  value =
withObjCPtr value $ \raw_value ->
    sendMsg swCollaborationOptionsGroup (mkSelector "setOptions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifier:options:@
initWithIdentifier_optionsSelector :: Selector
initWithIdentifier_optionsSelector = mkSelector "initWithIdentifier:options:"

-- | @Selector@ for @optionsGroupWithIdentifier:options:@
optionsGroupWithIdentifier_optionsSelector :: Selector
optionsGroupWithIdentifier_optionsSelector = mkSelector "optionsGroupWithIdentifier:options:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @footer@
footerSelector :: Selector
footerSelector = mkSelector "footer"

-- | @Selector@ for @setFooter:@
setFooterSelector :: Selector
setFooterSelector = mkSelector "setFooter:"

-- | @Selector@ for @options@
optionsSelector :: Selector
optionsSelector = mkSelector "options"

-- | @Selector@ for @setOptions:@
setOptionsSelector :: Selector
setOptionsSelector = mkSelector "setOptions:"

