{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MLMediaGroup@.
module ObjC.MediaLibrary.MLMediaGroup
  ( MLMediaGroup
  , IsMLMediaGroup(..)
  , mediaLibrary
  , parent
  , mediaSourceIdentifier
  , name
  , identifier
  , typeIdentifier
  , attributes
  , childGroups
  , url
  , modificationDate
  , mediaObjects
  , mediaLibrarySelector
  , parentSelector
  , mediaSourceIdentifierSelector
  , nameSelector
  , identifierSelector
  , typeIdentifierSelector
  , attributesSelector
  , childGroupsSelector
  , urlSelector
  , modificationDateSelector
  , mediaObjectsSelector


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

import ObjC.MediaLibrary.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- mediaLibrary@
mediaLibrary :: IsMLMediaGroup mlMediaGroup => mlMediaGroup -> IO (Id MLMediaLibrary)
mediaLibrary mlMediaGroup  =
  sendMsg mlMediaGroup (mkSelector "mediaLibrary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- parent@
parent :: IsMLMediaGroup mlMediaGroup => mlMediaGroup -> IO (Id MLMediaGroup)
parent mlMediaGroup  =
  sendMsg mlMediaGroup (mkSelector "parent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- mediaSourceIdentifier@
mediaSourceIdentifier :: IsMLMediaGroup mlMediaGroup => mlMediaGroup -> IO (Id NSString)
mediaSourceIdentifier mlMediaGroup  =
  sendMsg mlMediaGroup (mkSelector "mediaSourceIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- name@
name :: IsMLMediaGroup mlMediaGroup => mlMediaGroup -> IO (Id NSString)
name mlMediaGroup  =
  sendMsg mlMediaGroup (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- identifier@
identifier :: IsMLMediaGroup mlMediaGroup => mlMediaGroup -> IO (Id NSString)
identifier mlMediaGroup  =
  sendMsg mlMediaGroup (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- typeIdentifier@
typeIdentifier :: IsMLMediaGroup mlMediaGroup => mlMediaGroup -> IO (Id NSString)
typeIdentifier mlMediaGroup  =
  sendMsg mlMediaGroup (mkSelector "typeIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- attributes@
attributes :: IsMLMediaGroup mlMediaGroup => mlMediaGroup -> IO (Id NSDictionary)
attributes mlMediaGroup  =
  sendMsg mlMediaGroup (mkSelector "attributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- childGroups@
childGroups :: IsMLMediaGroup mlMediaGroup => mlMediaGroup -> IO (Id NSArray)
childGroups mlMediaGroup  =
  sendMsg mlMediaGroup (mkSelector "childGroups") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- URL@
url :: IsMLMediaGroup mlMediaGroup => mlMediaGroup -> IO (Id NSURL)
url mlMediaGroup  =
  sendMsg mlMediaGroup (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- modificationDate@
modificationDate :: IsMLMediaGroup mlMediaGroup => mlMediaGroup -> IO (Id NSDate)
modificationDate mlMediaGroup  =
  sendMsg mlMediaGroup (mkSelector "modificationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- mediaObjects@
mediaObjects :: IsMLMediaGroup mlMediaGroup => mlMediaGroup -> IO (Id NSArray)
mediaObjects mlMediaGroup  =
  sendMsg mlMediaGroup (mkSelector "mediaObjects") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mediaLibrary@
mediaLibrarySelector :: Selector
mediaLibrarySelector = mkSelector "mediaLibrary"

-- | @Selector@ for @parent@
parentSelector :: Selector
parentSelector = mkSelector "parent"

-- | @Selector@ for @mediaSourceIdentifier@
mediaSourceIdentifierSelector :: Selector
mediaSourceIdentifierSelector = mkSelector "mediaSourceIdentifier"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @typeIdentifier@
typeIdentifierSelector :: Selector
typeIdentifierSelector = mkSelector "typeIdentifier"

-- | @Selector@ for @attributes@
attributesSelector :: Selector
attributesSelector = mkSelector "attributes"

-- | @Selector@ for @childGroups@
childGroupsSelector :: Selector
childGroupsSelector = mkSelector "childGroups"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @modificationDate@
modificationDateSelector :: Selector
modificationDateSelector = mkSelector "modificationDate"

-- | @Selector@ for @mediaObjects@
mediaObjectsSelector :: Selector
mediaObjectsSelector = mkSelector "mediaObjects"

