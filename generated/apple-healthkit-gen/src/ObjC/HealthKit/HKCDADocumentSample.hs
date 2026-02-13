{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKCDADocumentSample
--
-- A sample object representing a CDA document.
--
-- Generated bindings for @HKCDADocumentSample@.
module ObjC.HealthKit.HKCDADocumentSample
  ( HKCDADocumentSample
  , IsHKCDADocumentSample(..)
  , cdaDocumentSampleWithData_startDate_endDate_metadata_validationError
  , document
  , cdaDocumentSampleWithData_startDate_endDate_metadata_validationErrorSelector
  , documentSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | CDADocumentSampleWithData:startDate:endDate:device:metadata:validationError:
--
-- Creates a new document sample with the specified attributes.
--
-- @documentData@ — Document contents in an XML format that meets the CDA standard.
--
-- @startDate@ — The start date for the document.
--
-- @endDate@ — The end date for the document.
--
-- @metadata@ — Metadata for the document.
--
-- @validationError@ — The XML content will be validated against the standard for CDA content.  If that validation                        fails, then this parameter will be set with the relavant error.  Detailed information about the                        failure may be obtained by examining the value for the HKDetailedCDAValidationErrorKey key of                        the NSError's userInfo dictionary.
--
-- Returns: The new instance or nil if the documentData does not pass validation.
--
-- Attributes of the document, such as title, patient name, etc. will be extracted automatically                        from the document content.
--
-- ObjC selector: @+ CDADocumentSampleWithData:startDate:endDate:metadata:validationError:@
cdaDocumentSampleWithData_startDate_endDate_metadata_validationError :: (IsNSData documentData, IsNSDate startDate, IsNSDate endDate, IsNSDictionary metadata, IsNSError validationError) => documentData -> startDate -> endDate -> metadata -> validationError -> IO (Id HKCDADocumentSample)
cdaDocumentSampleWithData_startDate_endDate_metadata_validationError documentData startDate endDate metadata validationError =
  do
    cls' <- getRequiredClass "HKCDADocumentSample"
    sendClassMessage cls' cdaDocumentSampleWithData_startDate_endDate_metadata_validationErrorSelector (toNSData documentData) (toNSDate startDate) (toNSDate endDate) (toNSDictionary metadata) (toNSError validationError)

-- | document
--
-- The contents of the document.
--
-- Access to each CDA instance must be authorized by the user in order for the document data to be                accessible to an app.  The authorization request occurs the first time a document matches the predicate                of an executed HKDocumentQuery.  This property will always be nil if the sample is returned by an                HKSampleQuery or an HKAnchoredObjectQuery.
--
-- ObjC selector: @- document@
document :: IsHKCDADocumentSample hkcdaDocumentSample => hkcdaDocumentSample -> IO (Id HKCDADocument)
document hkcdaDocumentSample =
  sendMessage hkcdaDocumentSample documentSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @CDADocumentSampleWithData:startDate:endDate:metadata:validationError:@
cdaDocumentSampleWithData_startDate_endDate_metadata_validationErrorSelector :: Selector '[Id NSData, Id NSDate, Id NSDate, Id NSDictionary, Id NSError] (Id HKCDADocumentSample)
cdaDocumentSampleWithData_startDate_endDate_metadata_validationErrorSelector = mkSelector "CDADocumentSampleWithData:startDate:endDate:metadata:validationError:"

-- | @Selector@ for @document@
documentSelector :: Selector '[] (Id HKCDADocument)
documentSelector = mkSelector "document"

