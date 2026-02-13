{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKDocumentSample
--
-- An abstract class representing a health document.
--
-- Generated bindings for @HKDocumentSample@.
module ObjC.HealthKit.HKDocumentSample
  ( HKDocumentSample
  , IsHKDocumentSample(..)
  , documentType
  , documentTypeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- documentType@
documentType :: IsHKDocumentSample hkDocumentSample => hkDocumentSample -> IO (Id HKDocumentType)
documentType hkDocumentSample =
  sendMessage hkDocumentSample documentTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @documentType@
documentTypeSelector :: Selector '[] (Id HKDocumentType)
documentTypeSelector = mkSelector "documentType"

