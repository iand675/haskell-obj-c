{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ABSearchElement@.
module ObjC.AddressBook.ABSearchElement
  ( ABSearchElement
  , IsABSearchElement(..)
  , searchElementForConjunction_children
  , matchesRecord
  , searchElementForConjunction_childrenSelector
  , matchesRecordSelector


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

import ObjC.AddressBook.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ searchElementForConjunction:children:@
searchElementForConjunction_children :: IsNSArray children => CLong -> children -> IO (Id ABSearchElement)
searchElementForConjunction_children conjuction children =
  do
    cls' <- getRequiredClass "ABSearchElement"
    withObjCPtr children $ \raw_children ->
      sendClassMsg cls' (mkSelector "searchElementForConjunction:children:") (retPtr retVoid) [argCLong (fromIntegral conjuction), argPtr (castPtr raw_children :: Ptr ())] >>= retainedObject . castPtr

-- | @- matchesRecord:@
matchesRecord :: (IsABSearchElement abSearchElement, IsABRecord record) => abSearchElement -> record -> IO Bool
matchesRecord abSearchElement  record =
withObjCPtr record $ \raw_record ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg abSearchElement (mkSelector "matchesRecord:") retCULong [argPtr (castPtr raw_record :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @searchElementForConjunction:children:@
searchElementForConjunction_childrenSelector :: Selector
searchElementForConjunction_childrenSelector = mkSelector "searchElementForConjunction:children:"

-- | @Selector@ for @matchesRecord:@
matchesRecordSelector :: Selector
matchesRecordSelector = mkSelector "matchesRecord:"

