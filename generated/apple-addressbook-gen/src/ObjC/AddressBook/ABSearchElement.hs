{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ABSearchElement@.
module ObjC.AddressBook.ABSearchElement
  ( ABSearchElement
  , IsABSearchElement(..)
  , searchElementForConjunction_children
  , matchesRecord
  , matchesRecordSelector
  , searchElementForConjunction_childrenSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AddressBook.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ searchElementForConjunction:children:@
searchElementForConjunction_children :: IsNSArray children => CLong -> children -> IO (Id ABSearchElement)
searchElementForConjunction_children conjuction children =
  do
    cls' <- getRequiredClass "ABSearchElement"
    sendClassMessage cls' searchElementForConjunction_childrenSelector conjuction (toNSArray children)

-- | @- matchesRecord:@
matchesRecord :: (IsABSearchElement abSearchElement, IsABRecord record) => abSearchElement -> record -> IO Bool
matchesRecord abSearchElement record =
  sendMessage abSearchElement matchesRecordSelector (toABRecord record)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @searchElementForConjunction:children:@
searchElementForConjunction_childrenSelector :: Selector '[CLong, Id NSArray] (Id ABSearchElement)
searchElementForConjunction_childrenSelector = mkSelector "searchElementForConjunction:children:"

-- | @Selector@ for @matchesRecord:@
matchesRecordSelector :: Selector '[Id ABRecord] Bool
matchesRecordSelector = mkSelector "matchesRecord:"

