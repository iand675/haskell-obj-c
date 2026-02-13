{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INTaskList@.
module ObjC.Intents.INTaskList
  ( INTaskList
  , IsINTaskList(..)
  , initWithTitle_tasks_groupName_createdDateComponents_modifiedDateComponents_identifier
  , title
  , tasks
  , groupName
  , createdDateComponents
  , modifiedDateComponents
  , identifier
  , createdDateComponentsSelector
  , groupNameSelector
  , identifierSelector
  , initWithTitle_tasks_groupName_createdDateComponents_modifiedDateComponents_identifierSelector
  , modifiedDateComponentsSelector
  , tasksSelector
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

-- | @- initWithTitle:tasks:groupName:createdDateComponents:modifiedDateComponents:identifier:@
initWithTitle_tasks_groupName_createdDateComponents_modifiedDateComponents_identifier :: (IsINTaskList inTaskList, IsINSpeakableString title, IsNSArray tasks, IsINSpeakableString groupName, IsNSDateComponents createdDateComponents, IsNSDateComponents modifiedDateComponents, IsNSString identifier) => inTaskList -> title -> tasks -> groupName -> createdDateComponents -> modifiedDateComponents -> identifier -> IO (Id INTaskList)
initWithTitle_tasks_groupName_createdDateComponents_modifiedDateComponents_identifier inTaskList title tasks groupName createdDateComponents modifiedDateComponents identifier =
  sendOwnedMessage inTaskList initWithTitle_tasks_groupName_createdDateComponents_modifiedDateComponents_identifierSelector (toINSpeakableString title) (toNSArray tasks) (toINSpeakableString groupName) (toNSDateComponents createdDateComponents) (toNSDateComponents modifiedDateComponents) (toNSString identifier)

-- | @- title@
title :: IsINTaskList inTaskList => inTaskList -> IO (Id INSpeakableString)
title inTaskList =
  sendMessage inTaskList titleSelector

-- | @- tasks@
tasks :: IsINTaskList inTaskList => inTaskList -> IO (Id NSArray)
tasks inTaskList =
  sendMessage inTaskList tasksSelector

-- | @- groupName@
groupName :: IsINTaskList inTaskList => inTaskList -> IO (Id INSpeakableString)
groupName inTaskList =
  sendMessage inTaskList groupNameSelector

-- | @- createdDateComponents@
createdDateComponents :: IsINTaskList inTaskList => inTaskList -> IO (Id NSDateComponents)
createdDateComponents inTaskList =
  sendMessage inTaskList createdDateComponentsSelector

-- | @- modifiedDateComponents@
modifiedDateComponents :: IsINTaskList inTaskList => inTaskList -> IO (Id NSDateComponents)
modifiedDateComponents inTaskList =
  sendMessage inTaskList modifiedDateComponentsSelector

-- | @- identifier@
identifier :: IsINTaskList inTaskList => inTaskList -> IO (Id NSString)
identifier inTaskList =
  sendMessage inTaskList identifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTitle:tasks:groupName:createdDateComponents:modifiedDateComponents:identifier:@
initWithTitle_tasks_groupName_createdDateComponents_modifiedDateComponents_identifierSelector :: Selector '[Id INSpeakableString, Id NSArray, Id INSpeakableString, Id NSDateComponents, Id NSDateComponents, Id NSString] (Id INTaskList)
initWithTitle_tasks_groupName_createdDateComponents_modifiedDateComponents_identifierSelector = mkSelector "initWithTitle:tasks:groupName:createdDateComponents:modifiedDateComponents:identifier:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id INSpeakableString)
titleSelector = mkSelector "title"

-- | @Selector@ for @tasks@
tasksSelector :: Selector '[] (Id NSArray)
tasksSelector = mkSelector "tasks"

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

