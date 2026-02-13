{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | ICScannerFunctionalUnitDocumentFeeder
--
-- ICScannerFunctionalUnitDocumentFeeder is a concrete subclass of ICScannerFunctionalUnit class. ICScannerDevice creates instances of this class.
--
-- This represents the document feeder unit on the scanner.
--
-- Generated bindings for @ICScannerFunctionalUnitDocumentFeeder@.
module ObjC.ImageCaptureCore.ICScannerFunctionalUnitDocumentFeeder
  ( ICScannerFunctionalUnitDocumentFeeder
  , IsICScannerFunctionalUnitDocumentFeeder(..)
  , supportedDocumentTypes
  , documentType
  , setDocumentType
  , documentSize
  , supportsDuplexScanning
  , duplexScanningEnabled
  , setDuplexScanningEnabled
  , documentLoaded
  , oddPageOrientation
  , setOddPageOrientation
  , evenPageOrientation
  , setEvenPageOrientation
  , reverseFeederPageOrder
  , documentLoadedSelector
  , documentSizeSelector
  , documentTypeSelector
  , duplexScanningEnabledSelector
  , evenPageOrientationSelector
  , oddPageOrientationSelector
  , reverseFeederPageOrderSelector
  , setDocumentTypeSelector
  , setDuplexScanningEnabledSelector
  , setEvenPageOrientationSelector
  , setOddPageOrientationSelector
  , supportedDocumentTypesSelector
  , supportsDuplexScanningSelector

  -- * Enum types
  , ICEXIFOrientationType(ICEXIFOrientationType)
  , pattern ICEXIFOrientation1
  , pattern ICEXIFOrientation2
  , pattern ICEXIFOrientation3
  , pattern ICEXIFOrientation4
  , pattern ICEXIFOrientation5
  , pattern ICEXIFOrientation6
  , pattern ICEXIFOrientation7
  , pattern ICEXIFOrientation8
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
supportedDocumentTypes :: IsICScannerFunctionalUnitDocumentFeeder icScannerFunctionalUnitDocumentFeeder => icScannerFunctionalUnitDocumentFeeder -> IO (Id NSIndexSet)
supportedDocumentTypes icScannerFunctionalUnitDocumentFeeder =
  sendMessage icScannerFunctionalUnitDocumentFeeder supportedDocumentTypesSelector

-- | documentType
--
-- ￼Current document type. This will always be one of the supported document types.
--
-- ObjC selector: @- documentType@
documentType :: IsICScannerFunctionalUnitDocumentFeeder icScannerFunctionalUnitDocumentFeeder => icScannerFunctionalUnitDocumentFeeder -> IO ICScannerDocumentType
documentType icScannerFunctionalUnitDocumentFeeder =
  sendMessage icScannerFunctionalUnitDocumentFeeder documentTypeSelector

-- | documentType
--
-- ￼Current document type. This will always be one of the supported document types.
--
-- ObjC selector: @- setDocumentType:@
setDocumentType :: IsICScannerFunctionalUnitDocumentFeeder icScannerFunctionalUnitDocumentFeeder => icScannerFunctionalUnitDocumentFeeder -> ICScannerDocumentType -> IO ()
setDocumentType icScannerFunctionalUnitDocumentFeeder value =
  sendMessage icScannerFunctionalUnitDocumentFeeder setDocumentTypeSelector value

-- | documentSize
--
-- ￼Document size of the current document type expressed in current measurement unit.
--
-- ObjC selector: @- documentSize@
documentSize :: IsICScannerFunctionalUnitDocumentFeeder icScannerFunctionalUnitDocumentFeeder => icScannerFunctionalUnitDocumentFeeder -> IO NSSize
documentSize icScannerFunctionalUnitDocumentFeeder =
  sendMessage icScannerFunctionalUnitDocumentFeeder documentSizeSelector

-- | supportsDuplexScanning
--
-- ￼Indicates whether duplex scanning is supported.
--
-- ObjC selector: @- supportsDuplexScanning@
supportsDuplexScanning :: IsICScannerFunctionalUnitDocumentFeeder icScannerFunctionalUnitDocumentFeeder => icScannerFunctionalUnitDocumentFeeder -> IO Bool
supportsDuplexScanning icScannerFunctionalUnitDocumentFeeder =
  sendMessage icScannerFunctionalUnitDocumentFeeder supportsDuplexScanningSelector

-- | duplexScanningEnabled
--
-- ￼Indicates whether duplex scanning is enabled.
--
-- ObjC selector: @- duplexScanningEnabled@
duplexScanningEnabled :: IsICScannerFunctionalUnitDocumentFeeder icScannerFunctionalUnitDocumentFeeder => icScannerFunctionalUnitDocumentFeeder -> IO Bool
duplexScanningEnabled icScannerFunctionalUnitDocumentFeeder =
  sendMessage icScannerFunctionalUnitDocumentFeeder duplexScanningEnabledSelector

-- | duplexScanningEnabled
--
-- ￼Indicates whether duplex scanning is enabled.
--
-- ObjC selector: @- setDuplexScanningEnabled:@
setDuplexScanningEnabled :: IsICScannerFunctionalUnitDocumentFeeder icScannerFunctionalUnitDocumentFeeder => icScannerFunctionalUnitDocumentFeeder -> Bool -> IO ()
setDuplexScanningEnabled icScannerFunctionalUnitDocumentFeeder value =
  sendMessage icScannerFunctionalUnitDocumentFeeder setDuplexScanningEnabledSelector value

-- | documentLoaded
--
-- ￼Indicates whether the feeder has documents to scan.
--
-- This value will change when the document is loaded or removed from the feeder, if the scanner module has the capability to detect this state.
--
-- ObjC selector: @- documentLoaded@
documentLoaded :: IsICScannerFunctionalUnitDocumentFeeder icScannerFunctionalUnitDocumentFeeder => icScannerFunctionalUnitDocumentFeeder -> IO Bool
documentLoaded icScannerFunctionalUnitDocumentFeeder =
  sendMessage icScannerFunctionalUnitDocumentFeeder documentLoadedSelector

-- | oddPageOrientation
--
-- ￼Desired orientation of the odd pages of the scanned document.
--
-- This property is set to ICEXIFOrientation1 initially.
--
-- ObjC selector: @- oddPageOrientation@
oddPageOrientation :: IsICScannerFunctionalUnitDocumentFeeder icScannerFunctionalUnitDocumentFeeder => icScannerFunctionalUnitDocumentFeeder -> IO ICEXIFOrientationType
oddPageOrientation icScannerFunctionalUnitDocumentFeeder =
  sendMessage icScannerFunctionalUnitDocumentFeeder oddPageOrientationSelector

-- | oddPageOrientation
--
-- ￼Desired orientation of the odd pages of the scanned document.
--
-- This property is set to ICEXIFOrientation1 initially.
--
-- ObjC selector: @- setOddPageOrientation:@
setOddPageOrientation :: IsICScannerFunctionalUnitDocumentFeeder icScannerFunctionalUnitDocumentFeeder => icScannerFunctionalUnitDocumentFeeder -> ICEXIFOrientationType -> IO ()
setOddPageOrientation icScannerFunctionalUnitDocumentFeeder value =
  sendMessage icScannerFunctionalUnitDocumentFeeder setOddPageOrientationSelector value

-- | evenPageOrientation
--
-- ￼Desired orientation of the even pages of the scanned document.
--
-- This property is set to ICEXIFOrientation1 initially.
--
-- ObjC selector: @- evenPageOrientation@
evenPageOrientation :: IsICScannerFunctionalUnitDocumentFeeder icScannerFunctionalUnitDocumentFeeder => icScannerFunctionalUnitDocumentFeeder -> IO ICEXIFOrientationType
evenPageOrientation icScannerFunctionalUnitDocumentFeeder =
  sendMessage icScannerFunctionalUnitDocumentFeeder evenPageOrientationSelector

-- | evenPageOrientation
--
-- ￼Desired orientation of the even pages of the scanned document.
--
-- This property is set to ICEXIFOrientation1 initially.
--
-- ObjC selector: @- setEvenPageOrientation:@
setEvenPageOrientation :: IsICScannerFunctionalUnitDocumentFeeder icScannerFunctionalUnitDocumentFeeder => icScannerFunctionalUnitDocumentFeeder -> ICEXIFOrientationType -> IO ()
setEvenPageOrientation icScannerFunctionalUnitDocumentFeeder value =
  sendMessage icScannerFunctionalUnitDocumentFeeder setEvenPageOrientationSelector value

-- | reverseFeederPageOrder
--
-- ￼Indicates whether the document feeder reads pages from back to front.
--
-- ObjC selector: @- reverseFeederPageOrder@
reverseFeederPageOrder :: IsICScannerFunctionalUnitDocumentFeeder icScannerFunctionalUnitDocumentFeeder => icScannerFunctionalUnitDocumentFeeder -> IO Bool
reverseFeederPageOrder icScannerFunctionalUnitDocumentFeeder =
  sendMessage icScannerFunctionalUnitDocumentFeeder reverseFeederPageOrderSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @supportedDocumentTypes@
supportedDocumentTypesSelector :: Selector '[] (Id NSIndexSet)
supportedDocumentTypesSelector = mkSelector "supportedDocumentTypes"

-- | @Selector@ for @documentType@
documentTypeSelector :: Selector '[] ICScannerDocumentType
documentTypeSelector = mkSelector "documentType"

-- | @Selector@ for @setDocumentType:@
setDocumentTypeSelector :: Selector '[ICScannerDocumentType] ()
setDocumentTypeSelector = mkSelector "setDocumentType:"

-- | @Selector@ for @documentSize@
documentSizeSelector :: Selector '[] NSSize
documentSizeSelector = mkSelector "documentSize"

-- | @Selector@ for @supportsDuplexScanning@
supportsDuplexScanningSelector :: Selector '[] Bool
supportsDuplexScanningSelector = mkSelector "supportsDuplexScanning"

-- | @Selector@ for @duplexScanningEnabled@
duplexScanningEnabledSelector :: Selector '[] Bool
duplexScanningEnabledSelector = mkSelector "duplexScanningEnabled"

-- | @Selector@ for @setDuplexScanningEnabled:@
setDuplexScanningEnabledSelector :: Selector '[Bool] ()
setDuplexScanningEnabledSelector = mkSelector "setDuplexScanningEnabled:"

-- | @Selector@ for @documentLoaded@
documentLoadedSelector :: Selector '[] Bool
documentLoadedSelector = mkSelector "documentLoaded"

-- | @Selector@ for @oddPageOrientation@
oddPageOrientationSelector :: Selector '[] ICEXIFOrientationType
oddPageOrientationSelector = mkSelector "oddPageOrientation"

-- | @Selector@ for @setOddPageOrientation:@
setOddPageOrientationSelector :: Selector '[ICEXIFOrientationType] ()
setOddPageOrientationSelector = mkSelector "setOddPageOrientation:"

-- | @Selector@ for @evenPageOrientation@
evenPageOrientationSelector :: Selector '[] ICEXIFOrientationType
evenPageOrientationSelector = mkSelector "evenPageOrientation"

-- | @Selector@ for @setEvenPageOrientation:@
setEvenPageOrientationSelector :: Selector '[ICEXIFOrientationType] ()
setEvenPageOrientationSelector = mkSelector "setEvenPageOrientation:"

-- | @Selector@ for @reverseFeederPageOrder@
reverseFeederPageOrderSelector :: Selector '[] Bool
reverseFeederPageOrderSelector = mkSelector "reverseFeederPageOrder"

