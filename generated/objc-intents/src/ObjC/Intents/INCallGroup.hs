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
  , initSelector
  , initWithGroupName_groupIdSelector
  , groupNameSelector
  , groupIdSelector


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

-- | @- init@
init_ :: IsINCallGroup inCallGroup => inCallGroup -> IO (Id INCallGroup)
init_ inCallGroup  =
  sendMsg inCallGroup (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithGroupName:groupId:@
initWithGroupName_groupId :: (IsINCallGroup inCallGroup, IsNSString groupName, IsNSString groupId) => inCallGroup -> groupName -> groupId -> IO (Id INCallGroup)
initWithGroupName_groupId inCallGroup  groupName groupId =
withObjCPtr groupName $ \raw_groupName ->
  withObjCPtr groupId $ \raw_groupId ->
      sendMsg inCallGroup (mkSelector "initWithGroupName:groupId:") (retPtr retVoid) [argPtr (castPtr raw_groupName :: Ptr ()), argPtr (castPtr raw_groupId :: Ptr ())] >>= ownedObject . castPtr

-- | @- groupName@
groupName :: IsINCallGroup inCallGroup => inCallGroup -> IO (Id NSString)
groupName inCallGroup  =
  sendMsg inCallGroup (mkSelector "groupName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- groupId@
groupId :: IsINCallGroup inCallGroup => inCallGroup -> IO (Id NSString)
groupId inCallGroup  =
  sendMsg inCallGroup (mkSelector "groupId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithGroupName:groupId:@
initWithGroupName_groupIdSelector :: Selector
initWithGroupName_groupIdSelector = mkSelector "initWithGroupName:groupId:"

-- | @Selector@ for @groupName@
groupNameSelector :: Selector
groupNameSelector = mkSelector "groupName"

-- | @Selector@ for @groupId@
groupIdSelector :: Selector
groupIdSelector = mkSelector "groupId"

