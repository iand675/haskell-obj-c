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
  , initWithTitle_taskTitles_groupNameSelector
  , titleSelector
  , taskTitlesSelector
  , groupNameSelector


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

-- | @- initWithTitle:taskTitles:groupName:@
initWithTitle_taskTitles_groupName :: (IsINCreateTaskListIntent inCreateTaskListIntent, IsINSpeakableString title, IsNSArray taskTitles, IsINSpeakableString groupName) => inCreateTaskListIntent -> title -> taskTitles -> groupName -> IO (Id INCreateTaskListIntent)
initWithTitle_taskTitles_groupName inCreateTaskListIntent  title taskTitles groupName =
withObjCPtr title $ \raw_title ->
  withObjCPtr taskTitles $ \raw_taskTitles ->
    withObjCPtr groupName $ \raw_groupName ->
        sendMsg inCreateTaskListIntent (mkSelector "initWithTitle:taskTitles:groupName:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_taskTitles :: Ptr ()), argPtr (castPtr raw_groupName :: Ptr ())] >>= ownedObject . castPtr

-- | @- title@
title :: IsINCreateTaskListIntent inCreateTaskListIntent => inCreateTaskListIntent -> IO (Id INSpeakableString)
title inCreateTaskListIntent  =
  sendMsg inCreateTaskListIntent (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- taskTitles@
taskTitles :: IsINCreateTaskListIntent inCreateTaskListIntent => inCreateTaskListIntent -> IO (Id NSArray)
taskTitles inCreateTaskListIntent  =
  sendMsg inCreateTaskListIntent (mkSelector "taskTitles") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- groupName@
groupName :: IsINCreateTaskListIntent inCreateTaskListIntent => inCreateTaskListIntent -> IO (Id INSpeakableString)
groupName inCreateTaskListIntent  =
  sendMsg inCreateTaskListIntent (mkSelector "groupName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTitle:taskTitles:groupName:@
initWithTitle_taskTitles_groupNameSelector :: Selector
initWithTitle_taskTitles_groupNameSelector = mkSelector "initWithTitle:taskTitles:groupName:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @taskTitles@
taskTitlesSelector :: Selector
taskTitlesSelector = mkSelector "taskTitles"

-- | @Selector@ for @groupName@
groupNameSelector :: Selector
groupNameSelector = mkSelector "groupName"

