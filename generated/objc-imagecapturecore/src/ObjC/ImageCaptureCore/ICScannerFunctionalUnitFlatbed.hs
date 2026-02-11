{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | ICScannerFunctionalUnitFlatbed
--
-- ICScannerFunctionalUnitFlatbed is a concrete subclass of ICScannerFunctionalUnit class. ICScannerDevice creates instances of this class.
--
-- This represents the flatbed  unit on the scanner.
--
-- Generated bindings for @ICScannerFunctionalUnitFlatbed@.
module ObjC.ImageCaptureCore.ICScannerFunctionalUnitFlatbed
  ( ICScannerFunctionalUnitFlatbed
  , IsICScannerFunctionalUnitFlatbed(..)
  , supportedDocumentTypes
  , documentType
  , setDocumentType
  , documentSize
  , supportedDocumentTypesSelector
  , documentTypeSelector
  , setDocumentTypeSelector
  , documentSizeSelector

  -- * Enum types
  , ICScannerDocumentType(ICScannerDocumentType)
  , pattern ICScannerDocumentTypeDefault
  , pattern ICScannerDocumentTypeA4
  , pattern ICScannerDocumentTypeB5
  , pattern ICScannerDocumentTypeUSLetter
  , pattern ICScannerDocumentTypeUSLegal
  , pattern ICScannerDocumentTypeA5
  , pattern ICScannerDocumentTypeISOB4
  , pattern ICScannerDocumentTypeISOB6
  , pattern ICScannerDocumentTypeUSLedger
  , pattern ICScannerDocumentTypeUSExecutive
  , pattern ICScannerDocumentTypeA3
  , pattern ICScannerDocumentTypeISOB3
  , pattern ICScannerDocumentTypeA6
  , pattern ICScannerDocumentTypeC4
  , pattern ICScannerDocumentTypeC5
  , pattern ICScannerDocumentTypeC6
  , pattern ICScannerDocumentType4A0
  , pattern ICScannerDocumentType2A0
  , pattern ICScannerDocumentTypeA0
  , pattern ICScannerDocumentTypeA1
  , pattern ICScannerDocumentTypeA2
  , pattern ICScannerDocumentTypeA7
  , pattern ICScannerDocumentTypeA8
  , pattern ICScannerDocumentTypeA9
  , pattern ICScannerDocumentType10
  , pattern ICScannerDocumentTypeISOB0
  , pattern ICScannerDocumentTypeISOB1
  , pattern ICScannerDocumentTypeISOB2
  , pattern ICScannerDocumentTypeISOB5
  , pattern ICScannerDocumentTypeISOB7
  , pattern ICScannerDocumentTypeISOB8
  , pattern ICScannerDocumentTypeISOB9
  , pattern ICScannerDocumentTypeISOB10
  , pattern ICScannerDocumentTypeJISB0
  , pattern ICScannerDocumentTypeJISB1
  , pattern ICScannerDocumentTypeJISB2
  , pattern ICScannerDocumentTypeJISB3
  , pattern ICScannerDocumentTypeJISB4
  , pattern ICScannerDocumentTypeJISB6
  , pattern ICScannerDocumentTypeJISB7
  , pattern ICScannerDocumentTypeJISB8
  , pattern ICScannerDocumentTypeJISB9
  , pattern ICScannerDocumentTypeJISB10
  , pattern ICScannerDocumentTypeC0
  , pattern ICScannerDocumentTypeC1
  , pattern ICScannerDocumentTypeC2
  , pattern ICScannerDocumentTypeC3
  , pattern ICScannerDocumentTypeC7
  , pattern ICScannerDocumentTypeC8
  , pattern ICScannerDocumentTypeC9
  , pattern ICScannerDocumentTypeC10
  , pattern ICScannerDocumentTypeUSStatement
  , pattern ICScannerDocumentTypeBusinessCard
  , pattern ICScannerDocumentTypeE
  , pattern ICScannerDocumentType3R
  , pattern ICScannerDocumentType4R
  , pattern ICScannerDocumentType5R
  , pattern ICScannerDocumentType6R
  , pattern ICScannerDocumentType8R
  , pattern ICScannerDocumentTypeS8R
  , pattern ICScannerDocumentType10R
  , pattern ICScannerDocumentTypeS10R
  , pattern ICScannerDocumentType11R
  , pattern ICScannerDocumentType12R
  , pattern ICScannerDocumentTypeS12R
  , pattern ICScannerDocumentType110
  , pattern ICScannerDocumentTypeAPSH
  , pattern ICScannerDocumentTypeAPSC
  , pattern ICScannerDocumentTypeAPSP
  , pattern ICScannerDocumentType135
  , pattern ICScannerDocumentTypeMF
  , pattern ICScannerDocumentTypeLF

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ImageCaptureCore.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.ImageCaptureCore.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | supportedDocumentTypes
--
-- ￼Supported document types. The values in this set are valid values defined by ICScannerDocumentType.
--
-- ObjC selector: @- supportedDocumentTypes@
supportedDocumentTypes :: IsICScannerFunctionalUnitFlatbed icScannerFunctionalUnitFlatbed => icScannerFunctionalUnitFlatbed -> IO (Id NSIndexSet)
supportedDocumentTypes icScannerFunctionalUnitFlatbed  =
  sendMsg icScannerFunctionalUnitFlatbed (mkSelector "supportedDocumentTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | documentType
--
-- ￼Current document type. This will always be one of the supported document types.
--
-- ObjC selector: @- documentType@
documentType :: IsICScannerFunctionalUnitFlatbed icScannerFunctionalUnitFlatbed => icScannerFunctionalUnitFlatbed -> IO ICScannerDocumentType
documentType icScannerFunctionalUnitFlatbed  =
  fmap (coerce :: CULong -> ICScannerDocumentType) $ sendMsg icScannerFunctionalUnitFlatbed (mkSelector "documentType") retCULong []

-- | documentType
--
-- ￼Current document type. This will always be one of the supported document types.
--
-- ObjC selector: @- setDocumentType:@
setDocumentType :: IsICScannerFunctionalUnitFlatbed icScannerFunctionalUnitFlatbed => icScannerFunctionalUnitFlatbed -> ICScannerDocumentType -> IO ()
setDocumentType icScannerFunctionalUnitFlatbed  value =
  sendMsg icScannerFunctionalUnitFlatbed (mkSelector "setDocumentType:") retVoid [argCULong (coerce value)]

-- | documentSize
--
-- ￼Document size of the current document type expressed in current measurement unit.
--
-- ObjC selector: @- documentSize@
documentSize :: IsICScannerFunctionalUnitFlatbed icScannerFunctionalUnitFlatbed => icScannerFunctionalUnitFlatbed -> IO NSSize
documentSize icScannerFunctionalUnitFlatbed  =
  sendMsgStret icScannerFunctionalUnitFlatbed (mkSelector "documentSize") retNSSize []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @supportedDocumentTypes@
supportedDocumentTypesSelector :: Selector
supportedDocumentTypesSelector = mkSelector "supportedDocumentTypes"

-- | @Selector@ for @documentType@
documentTypeSelector :: Selector
documentTypeSelector = mkSelector "documentType"

-- | @Selector@ for @setDocumentType:@
setDocumentTypeSelector :: Selector
setDocumentTypeSelector = mkSelector "setDocumentType:"

-- | @Selector@ for @documentSize@
documentSizeSelector :: Selector
documentSizeSelector = mkSelector "documentSize"

