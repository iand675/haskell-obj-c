{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKCDADocument@.
module ObjC.HealthKit.HKCDADocument
  ( HKCDADocument
  , IsHKCDADocument(..)
  , documentData
  , title
  , patientName
  , authorName
  , custodianName
  , authorNameSelector
  , custodianNameSelector
  , documentDataSelector
  , patientNameSelector
  , titleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | documentData
--
-- The CDA document content in XML format as specified in the CDA standard. This may be nil if the            includeDocumentData option in HKDocumentQuery is specified as NO.
--
-- ObjC selector: @- documentData@
documentData :: IsHKCDADocument hkcdaDocument => hkcdaDocument -> IO (Id NSData)
documentData hkcdaDocument =
  sendMessage hkcdaDocument documentDataSelector

-- | title
--
-- The title of the document.
--
-- This property is extracted automatically from the document.
--
-- ObjC selector: @- title@
title :: IsHKCDADocument hkcdaDocument => hkcdaDocument -> IO (Id NSString)
title hkcdaDocument =
  sendMessage hkcdaDocument titleSelector

-- | patientName
--
-- The name of the patient receiving treatment.
--
-- This property is extracted automatically from the document.
--
-- ObjC selector: @- patientName@
patientName :: IsHKCDADocument hkcdaDocument => hkcdaDocument -> IO (Id NSString)
patientName hkcdaDocument =
  sendMessage hkcdaDocument patientNameSelector

-- | authorName
--
-- The person responsible for authoring the document.  Usually, this is the treating physician.
--
-- This property is extracted automatically from the document.
--
-- ObjC selector: @- authorName@
authorName :: IsHKCDADocument hkcdaDocument => hkcdaDocument -> IO (Id NSString)
authorName hkcdaDocument =
  sendMessage hkcdaDocument authorNameSelector

-- | custodianName
--
-- The organization responsible for the document.  This is usually the treating institution name.
--
-- This property is extracted automatically from the document.
--
-- ObjC selector: @- custodianName@
custodianName :: IsHKCDADocument hkcdaDocument => hkcdaDocument -> IO (Id NSString)
custodianName hkcdaDocument =
  sendMessage hkcdaDocument custodianNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @documentData@
documentDataSelector :: Selector '[] (Id NSData)
documentDataSelector = mkSelector "documentData"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @patientName@
patientNameSelector :: Selector '[] (Id NSString)
patientNameSelector = mkSelector "patientName"

-- | @Selector@ for @authorName@
authorNameSelector :: Selector '[] (Id NSString)
authorNameSelector = mkSelector "authorName"

-- | @Selector@ for @custodianName@
custodianNameSelector :: Selector '[] (Id NSString)
custodianNameSelector = mkSelector "custodianName"

