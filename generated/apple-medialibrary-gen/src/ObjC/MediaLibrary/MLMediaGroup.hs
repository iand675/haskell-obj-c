{-# LANGUAGE DataKinds #-}
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
  , attributesSelector
  , childGroupsSelector
  , identifierSelector
  , mediaLibrarySelector
  , mediaObjectsSelector
  , mediaSourceIdentifierSelector
  , modificationDateSelector
  , nameSelector
  , parentSelector
  , typeIdentifierSelector
  , urlSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaLibrary.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- mediaLibrary@
mediaLibrary :: IsMLMediaGroup mlMediaGroup => mlMediaGroup -> IO (Id MLMediaLibrary)
mediaLibrary mlMediaGroup =
  sendMessage mlMediaGroup mediaLibrarySelector

-- | @- parent@
parent :: IsMLMediaGroup mlMediaGroup => mlMediaGroup -> IO (Id MLMediaGroup)
parent mlMediaGroup =
  sendMessage mlMediaGroup parentSelector

-- | @- mediaSourceIdentifier@
mediaSourceIdentifier :: IsMLMediaGroup mlMediaGroup => mlMediaGroup -> IO (Id NSString)
mediaSourceIdentifier mlMediaGroup =
  sendMessage mlMediaGroup mediaSourceIdentifierSelector

-- | @- name@
name :: IsMLMediaGroup mlMediaGroup => mlMediaGroup -> IO (Id NSString)
name mlMediaGroup =
  sendMessage mlMediaGroup nameSelector

-- | @- identifier@
identifier :: IsMLMediaGroup mlMediaGroup => mlMediaGroup -> IO (Id NSString)
identifier mlMediaGroup =
  sendMessage mlMediaGroup identifierSelector

-- | @- typeIdentifier@
typeIdentifier :: IsMLMediaGroup mlMediaGroup => mlMediaGroup -> IO (Id NSString)
typeIdentifier mlMediaGroup =
  sendMessage mlMediaGroup typeIdentifierSelector

-- | @- attributes@
attributes :: IsMLMediaGroup mlMediaGroup => mlMediaGroup -> IO (Id NSDictionary)
attributes mlMediaGroup =
  sendMessage mlMediaGroup attributesSelector

-- | @- childGroups@
childGroups :: IsMLMediaGroup mlMediaGroup => mlMediaGroup -> IO (Id NSArray)
childGroups mlMediaGroup =
  sendMessage mlMediaGroup childGroupsSelector

-- | @- URL@
url :: IsMLMediaGroup mlMediaGroup => mlMediaGroup -> IO (Id NSURL)
url mlMediaGroup =
  sendMessage mlMediaGroup urlSelector

-- | @- modificationDate@
modificationDate :: IsMLMediaGroup mlMediaGroup => mlMediaGroup -> IO (Id NSDate)
modificationDate mlMediaGroup =
  sendMessage mlMediaGroup modificationDateSelector

-- | @- mediaObjects@
mediaObjects :: IsMLMediaGroup mlMediaGroup => mlMediaGroup -> IO (Id NSArray)
mediaObjects mlMediaGroup =
  sendMessage mlMediaGroup mediaObjectsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mediaLibrary@
mediaLibrarySelector :: Selector '[] (Id MLMediaLibrary)
mediaLibrarySelector = mkSelector "mediaLibrary"

-- | @Selector@ for @parent@
parentSelector :: Selector '[] (Id MLMediaGroup)
parentSelector = mkSelector "parent"

-- | @Selector@ for @mediaSourceIdentifier@
mediaSourceIdentifierSelector :: Selector '[] (Id NSString)
mediaSourceIdentifierSelector = mkSelector "mediaSourceIdentifier"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @typeIdentifier@
typeIdentifierSelector :: Selector '[] (Id NSString)
typeIdentifierSelector = mkSelector "typeIdentifier"

-- | @Selector@ for @attributes@
attributesSelector :: Selector '[] (Id NSDictionary)
attributesSelector = mkSelector "attributes"

-- | @Selector@ for @childGroups@
childGroupsSelector :: Selector '[] (Id NSArray)
childGroupsSelector = mkSelector "childGroups"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

-- | @Selector@ for @modificationDate@
modificationDateSelector :: Selector '[] (Id NSDate)
modificationDateSelector = mkSelector "modificationDate"

-- | @Selector@ for @mediaObjects@
mediaObjectsSelector :: Selector '[] (Id NSArray)
mediaObjectsSelector = mkSelector "mediaObjects"

