{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKDocumentQuery
--
-- A concrete subclass of HKQuery that provides an interface to retrieve documents from the Health store.
--
-- Generated bindings for @HKDocumentQuery@.
module ObjC.HealthKit.HKDocumentQuery
  ( HKDocumentQuery
  , IsHKDocumentQuery(..)
  , limit
  , sortDescriptors
  , includeDocumentData
  , includeDocumentDataSelector
  , limitSelector
  , sortDescriptorsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | limit
--
-- The maximum number of documents the receiver will return upon completion.
--
-- ObjC selector: @- limit@
limit :: IsHKDocumentQuery hkDocumentQuery => hkDocumentQuery -> IO CULong
limit hkDocumentQuery =
  sendMessage hkDocumentQuery limitSelector

-- | sortDescriptors
--
-- An array of NSSortDescriptors.
--
-- ObjC selector: @- sortDescriptors@
sortDescriptors :: IsHKDocumentQuery hkDocumentQuery => hkDocumentQuery -> IO (Id NSArray)
sortDescriptors hkDocumentQuery =
  sendMessage hkDocumentQuery sortDescriptorsSelector

-- | includeDocumentData
--
-- The XML content for documents may be large.  This property can be used to control whether the query                returns the XML content for each record.
--
-- ObjC selector: @- includeDocumentData@
includeDocumentData :: IsHKDocumentQuery hkDocumentQuery => hkDocumentQuery -> IO Bool
includeDocumentData hkDocumentQuery =
  sendMessage hkDocumentQuery includeDocumentDataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @limit@
limitSelector :: Selector '[] CULong
limitSelector = mkSelector "limit"

-- | @Selector@ for @sortDescriptors@
sortDescriptorsSelector :: Selector '[] (Id NSArray)
sortDescriptorsSelector = mkSelector "sortDescriptors"

-- | @Selector@ for @includeDocumentData@
includeDocumentDataSelector :: Selector '[] Bool
includeDocumentDataSelector = mkSelector "includeDocumentData"

