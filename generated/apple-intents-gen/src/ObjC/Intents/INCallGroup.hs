{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INCallGroup@.
module ObjC.Intents.INCallGroup
  ( INCallGroup
  , IsINCallGroup(..)
  , init_
  , initWithGroupName_groupId
  , groupName
  , groupId
  , groupIdSelector
  , groupNameSelector
  , initSelector
  , initWithGroupName_groupIdSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINCallGroup inCallGroup => inCallGroup -> IO (Id INCallGroup)
init_ inCallGroup =
  sendOwnedMessage inCallGroup initSelector

-- | @- initWithGroupName:groupId:@
initWithGroupName_groupId :: (IsINCallGroup inCallGroup, IsNSString groupName, IsNSString groupId) => inCallGroup -> groupName -> groupId -> IO (Id INCallGroup)
initWithGroupName_groupId inCallGroup groupName groupId =
  sendOwnedMessage inCallGroup initWithGroupName_groupIdSelector (toNSString groupName) (toNSString groupId)

-- | @- groupName@
groupName :: IsINCallGroup inCallGroup => inCallGroup -> IO (Id NSString)
groupName inCallGroup =
  sendMessage inCallGroup groupNameSelector

-- | @- groupId@
groupId :: IsINCallGroup inCallGroup => inCallGroup -> IO (Id NSString)
groupId inCallGroup =
  sendMessage inCallGroup groupIdSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INCallGroup)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithGroupName:groupId:@
initWithGroupName_groupIdSelector :: Selector '[Id NSString, Id NSString] (Id INCallGroup)
initWithGroupName_groupIdSelector = mkSelector "initWithGroupName:groupId:"

-- | @Selector@ for @groupName@
groupNameSelector :: Selector '[] (Id NSString)
groupNameSelector = mkSelector "groupName"

-- | @Selector@ for @groupId@
groupIdSelector :: Selector '[] (Id NSString)
groupIdSelector = mkSelector "groupId"

