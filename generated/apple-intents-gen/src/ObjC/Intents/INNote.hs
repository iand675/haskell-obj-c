{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INNote@.
module ObjC.Intents.INNote
  ( INNote
  , IsINNote(..)
  , initWithTitle_contents_groupName_createdDateComponents_modifiedDateComponents_identifier
  , title
  , contents
  , groupName
  , createdDateComponents
  , modifiedDateComponents
  , identifier
  , contentsSelector
  , createdDateComponentsSelector
  , groupNameSelector
  , identifierSelector
  , initWithTitle_contents_groupName_createdDateComponents_modifiedDateComponents_identifierSelector
  , modifiedDateComponentsSelector
  , titleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithTitle:contents:groupName:createdDateComponents:modifiedDateComponents:identifier:@
initWithTitle_contents_groupName_createdDateComponents_modifiedDateComponents_identifier :: (IsINNote inNote, IsINSpeakableString title, IsNSArray contents, IsINSpeakableString groupName, IsNSDateComponents createdDateComponents, IsNSDateComponents modifiedDateComponents, IsNSString identifier) => inNote -> title -> contents -> groupName -> createdDateComponents -> modifiedDateComponents -> identifier -> IO (Id INNote)
initWithTitle_contents_groupName_createdDateComponents_modifiedDateComponents_identifier inNote title contents groupName createdDateComponents modifiedDateComponents identifier =
  sendOwnedMessage inNote initWithTitle_contents_groupName_createdDateComponents_modifiedDateComponents_identifierSelector (toINSpeakableString title) (toNSArray contents) (toINSpeakableString groupName) (toNSDateComponents createdDateComponents) (toNSDateComponents modifiedDateComponents) (toNSString identifier)

-- | @- title@
title :: IsINNote inNote => inNote -> IO (Id INSpeakableString)
title inNote =
  sendMessage inNote titleSelector

-- | @- contents@
contents :: IsINNote inNote => inNote -> IO (Id NSArray)
contents inNote =
  sendMessage inNote contentsSelector

-- | @- groupName@
groupName :: IsINNote inNote => inNote -> IO (Id INSpeakableString)
groupName inNote =
  sendMessage inNote groupNameSelector

-- | @- createdDateComponents@
createdDateComponents :: IsINNote inNote => inNote -> IO (Id NSDateComponents)
createdDateComponents inNote =
  sendMessage inNote createdDateComponentsSelector

-- | @- modifiedDateComponents@
modifiedDateComponents :: IsINNote inNote => inNote -> IO (Id NSDateComponents)
modifiedDateComponents inNote =
  sendMessage inNote modifiedDateComponentsSelector

-- | @- identifier@
identifier :: IsINNote inNote => inNote -> IO (Id NSString)
identifier inNote =
  sendMessage inNote identifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTitle:contents:groupName:createdDateComponents:modifiedDateComponents:identifier:@
initWithTitle_contents_groupName_createdDateComponents_modifiedDateComponents_identifierSelector :: Selector '[Id INSpeakableString, Id NSArray, Id INSpeakableString, Id NSDateComponents, Id NSDateComponents, Id NSString] (Id INNote)
initWithTitle_contents_groupName_createdDateComponents_modifiedDateComponents_identifierSelector = mkSelector "initWithTitle:contents:groupName:createdDateComponents:modifiedDateComponents:identifier:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id INSpeakableString)
titleSelector = mkSelector "title"

-- | @Selector@ for @contents@
contentsSelector :: Selector '[] (Id NSArray)
contentsSelector = mkSelector "contents"

-- | @Selector@ for @groupName@
groupNameSelector :: Selector '[] (Id INSpeakableString)
groupNameSelector = mkSelector "groupName"

-- | @Selector@ for @createdDateComponents@
createdDateComponentsSelector :: Selector '[] (Id NSDateComponents)
createdDateComponentsSelector = mkSelector "createdDateComponents"

-- | @Selector@ for @modifiedDateComponents@
modifiedDateComponentsSelector :: Selector '[] (Id NSDateComponents)
modifiedDateComponentsSelector = mkSelector "modifiedDateComponents"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

