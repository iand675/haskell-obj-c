{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INCreateNoteIntent@.
module ObjC.Intents.INCreateNoteIntent
  ( INCreateNoteIntent
  , IsINCreateNoteIntent(..)
  , initWithTitle_content_groupName
  , title
  , content
  , groupName
  , initWithTitle_content_groupNameSelector
  , titleSelector
  , contentSelector
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

-- | @- initWithTitle:content:groupName:@
initWithTitle_content_groupName :: (IsINCreateNoteIntent inCreateNoteIntent, IsINSpeakableString title, IsINNoteContent content, IsINSpeakableString groupName) => inCreateNoteIntent -> title -> content -> groupName -> IO (Id INCreateNoteIntent)
initWithTitle_content_groupName inCreateNoteIntent  title content groupName =
withObjCPtr title $ \raw_title ->
  withObjCPtr content $ \raw_content ->
    withObjCPtr groupName $ \raw_groupName ->
        sendMsg inCreateNoteIntent (mkSelector "initWithTitle:content:groupName:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_content :: Ptr ()), argPtr (castPtr raw_groupName :: Ptr ())] >>= ownedObject . castPtr

-- | @- title@
title :: IsINCreateNoteIntent inCreateNoteIntent => inCreateNoteIntent -> IO (Id INSpeakableString)
title inCreateNoteIntent  =
  sendMsg inCreateNoteIntent (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- content@
content :: IsINCreateNoteIntent inCreateNoteIntent => inCreateNoteIntent -> IO (Id INNoteContent)
content inCreateNoteIntent  =
  sendMsg inCreateNoteIntent (mkSelector "content") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- groupName@
groupName :: IsINCreateNoteIntent inCreateNoteIntent => inCreateNoteIntent -> IO (Id INSpeakableString)
groupName inCreateNoteIntent  =
  sendMsg inCreateNoteIntent (mkSelector "groupName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTitle:content:groupName:@
initWithTitle_content_groupNameSelector :: Selector
initWithTitle_content_groupNameSelector = mkSelector "initWithTitle:content:groupName:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @content@
contentSelector :: Selector
contentSelector = mkSelector "content"

-- | @Selector@ for @groupName@
groupNameSelector :: Selector
groupNameSelector = mkSelector "groupName"

