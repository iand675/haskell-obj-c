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
  , initWithTitle_contents_groupName_createdDateComponents_modifiedDateComponents_identifierSelector
  , titleSelector
  , contentsSelector
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

-- | @- initWithTitle:contents:groupName:createdDateComponents:modifiedDateComponents:identifier:@
initWithTitle_contents_groupName_createdDateComponents_modifiedDateComponents_identifier :: (IsINNote inNote, IsINSpeakableString title, IsNSArray contents, IsINSpeakableString groupName, IsNSDateComponents createdDateComponents, IsNSDateComponents modifiedDateComponents, IsNSString identifier) => inNote -> title -> contents -> groupName -> createdDateComponents -> modifiedDateComponents -> identifier -> IO (Id INNote)
initWithTitle_contents_groupName_createdDateComponents_modifiedDateComponents_identifier inNote  title contents groupName createdDateComponents modifiedDateComponents identifier =
withObjCPtr title $ \raw_title ->
  withObjCPtr contents $ \raw_contents ->
    withObjCPtr groupName $ \raw_groupName ->
      withObjCPtr createdDateComponents $ \raw_createdDateComponents ->
        withObjCPtr modifiedDateComponents $ \raw_modifiedDateComponents ->
          withObjCPtr identifier $ \raw_identifier ->
              sendMsg inNote (mkSelector "initWithTitle:contents:groupName:createdDateComponents:modifiedDateComponents:identifier:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_contents :: Ptr ()), argPtr (castPtr raw_groupName :: Ptr ()), argPtr (castPtr raw_createdDateComponents :: Ptr ()), argPtr (castPtr raw_modifiedDateComponents :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- title@
title :: IsINNote inNote => inNote -> IO (Id INSpeakableString)
title inNote  =
  sendMsg inNote (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- contents@
contents :: IsINNote inNote => inNote -> IO (Id NSArray)
contents inNote  =
  sendMsg inNote (mkSelector "contents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- groupName@
groupName :: IsINNote inNote => inNote -> IO (Id INSpeakableString)
groupName inNote  =
  sendMsg inNote (mkSelector "groupName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- createdDateComponents@
createdDateComponents :: IsINNote inNote => inNote -> IO (Id NSDateComponents)
createdDateComponents inNote  =
  sendMsg inNote (mkSelector "createdDateComponents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- modifiedDateComponents@
modifiedDateComponents :: IsINNote inNote => inNote -> IO (Id NSDateComponents)
modifiedDateComponents inNote  =
  sendMsg inNote (mkSelector "modifiedDateComponents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- identifier@
identifier :: IsINNote inNote => inNote -> IO (Id NSString)
identifier inNote  =
  sendMsg inNote (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTitle:contents:groupName:createdDateComponents:modifiedDateComponents:identifier:@
initWithTitle_contents_groupName_createdDateComponents_modifiedDateComponents_identifierSelector :: Selector
initWithTitle_contents_groupName_createdDateComponents_modifiedDateComponents_identifierSelector = mkSelector "initWithTitle:contents:groupName:createdDateComponents:modifiedDateComponents:identifier:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @contents@
contentsSelector :: Selector
contentsSelector = mkSelector "contents"

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

