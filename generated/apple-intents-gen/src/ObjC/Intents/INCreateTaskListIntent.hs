{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INCreateTaskListIntent@.
module ObjC.Intents.INCreateTaskListIntent
  ( INCreateTaskListIntent
  , IsINCreateTaskListIntent(..)
  , initWithTitle_taskTitles_groupName
  , title
  , taskTitles
  , groupName
  , groupNameSelector
  , initWithTitle_taskTitles_groupNameSelector
  , taskTitlesSelector
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

-- | @- initWithTitle:taskTitles:groupName:@
initWithTitle_taskTitles_groupName :: (IsINCreateTaskListIntent inCreateTaskListIntent, IsINSpeakableString title, IsNSArray taskTitles, IsINSpeakableString groupName) => inCreateTaskListIntent -> title -> taskTitles -> groupName -> IO (Id INCreateTaskListIntent)
initWithTitle_taskTitles_groupName inCreateTaskListIntent title taskTitles groupName =
  sendOwnedMessage inCreateTaskListIntent initWithTitle_taskTitles_groupNameSelector (toINSpeakableString title) (toNSArray taskTitles) (toINSpeakableString groupName)

-- | @- title@
title :: IsINCreateTaskListIntent inCreateTaskListIntent => inCreateTaskListIntent -> IO (Id INSpeakableString)
title inCreateTaskListIntent =
  sendMessage inCreateTaskListIntent titleSelector

-- | @- taskTitles@
taskTitles :: IsINCreateTaskListIntent inCreateTaskListIntent => inCreateTaskListIntent -> IO (Id NSArray)
taskTitles inCreateTaskListIntent =
  sendMessage inCreateTaskListIntent taskTitlesSelector

-- | @- groupName@
groupName :: IsINCreateTaskListIntent inCreateTaskListIntent => inCreateTaskListIntent -> IO (Id INSpeakableString)
groupName inCreateTaskListIntent =
  sendMessage inCreateTaskListIntent groupNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTitle:taskTitles:groupName:@
initWithTitle_taskTitles_groupNameSelector :: Selector '[Id INSpeakableString, Id NSArray, Id INSpeakableString] (Id INCreateTaskListIntent)
initWithTitle_taskTitles_groupNameSelector = mkSelector "initWithTitle:taskTitles:groupName:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id INSpeakableString)
titleSelector = mkSelector "title"

-- | @Selector@ for @taskTitles@
taskTitlesSelector :: Selector '[] (Id NSArray)
taskTitlesSelector = mkSelector "taskTitles"

-- | @Selector@ for @groupName@
groupNameSelector :: Selector '[] (Id INSpeakableString)
groupNameSelector = mkSelector "groupName"

