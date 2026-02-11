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
  , initWithTitle_tasks_groupName_createdDateComponents_modifiedDateComponents_identifierSelector
  , titleSelector
  , tasksSelector
  , groupNameSelector
  , createdDateComponentsSelector
  , modifiedDateComponentsSelector
  , identifierSelector


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

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithTitle:tasks:groupName:createdDateComponents:modifiedDateComponents:identifier:@
initWithTitle_tasks_groupName_createdDateComponents_modifiedDateComponents_identifier :: (IsINTaskList inTaskList, IsINSpeakableString title, IsNSArray tasks, IsINSpeakableString groupName, IsNSDateComponents createdDateComponents, IsNSDateComponents modifiedDateComponents, IsNSString identifier) => inTaskList -> title -> tasks -> groupName -> createdDateComponents -> modifiedDateComponents -> identifier -> IO (Id INTaskList)
initWithTitle_tasks_groupName_createdDateComponents_modifiedDateComponents_identifier inTaskList  title tasks groupName createdDateComponents modifiedDateComponents identifier =
withObjCPtr title $ \raw_title ->
  withObjCPtr tasks $ \raw_tasks ->
    withObjCPtr groupName $ \raw_groupName ->
      withObjCPtr createdDateComponents $ \raw_createdDateComponents ->
        withObjCPtr modifiedDateComponents $ \raw_modifiedDateComponents ->
          withObjCPtr identifier $ \raw_identifier ->
              sendMsg inTaskList (mkSelector "initWithTitle:tasks:groupName:createdDateComponents:modifiedDateComponents:identifier:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_tasks :: Ptr ()), argPtr (castPtr raw_groupName :: Ptr ()), argPtr (castPtr raw_createdDateComponents :: Ptr ()), argPtr (castPtr raw_modifiedDateComponents :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- title@
title :: IsINTaskList inTaskList => inTaskList -> IO (Id INSpeakableString)
title inTaskList  =
  sendMsg inTaskList (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- tasks@
tasks :: IsINTaskList inTaskList => inTaskList -> IO (Id NSArray)
tasks inTaskList  =
  sendMsg inTaskList (mkSelector "tasks") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- groupName@
groupName :: IsINTaskList inTaskList => inTaskList -> IO (Id INSpeakableString)
groupName inTaskList  =
  sendMsg inTaskList (mkSelector "groupName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- createdDateComponents@
createdDateComponents :: IsINTaskList inTaskList => inTaskList -> IO (Id NSDateComponents)
createdDateComponents inTaskList  =
  sendMsg inTaskList (mkSelector "createdDateComponents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- modifiedDateComponents@
modifiedDateComponents :: IsINTaskList inTaskList => inTaskList -> IO (Id NSDateComponents)
modifiedDateComponents inTaskList  =
  sendMsg inTaskList (mkSelector "modifiedDateComponents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- identifier@
identifier :: IsINTaskList inTaskList => inTaskList -> IO (Id NSString)
identifier inTaskList  =
  sendMsg inTaskList (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTitle:tasks:groupName:createdDateComponents:modifiedDateComponents:identifier:@
initWithTitle_tasks_groupName_createdDateComponents_modifiedDateComponents_identifierSelector :: Selector
initWithTitle_tasks_groupName_createdDateComponents_modifiedDateComponents_identifierSelector = mkSelector "initWithTitle:tasks:groupName:createdDateComponents:modifiedDateComponents:identifier:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @tasks@
tasksSelector :: Selector
tasksSelector = mkSelector "tasks"

-- | @Selector@ for @groupName@
groupNameSelector :: Selector
groupNameSelector = mkSelector "groupName"

-- | @Selector@ for @createdDateComponents@
createdDateComponentsSelector :: Selector
createdDateComponentsSelector = mkSelector "createdDateComponents"

-- | @Selector@ for @modifiedDateComponents@
modifiedDateComponentsSelector :: Selector
modifiedDateComponentsSelector = mkSelector "modifiedDateComponents"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

