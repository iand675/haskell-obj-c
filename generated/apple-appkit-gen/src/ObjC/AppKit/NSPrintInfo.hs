{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPrintInfo@.
module ObjC.AppKit.NSPrintInfo
  ( NSPrintInfo
  , IsNSPrintInfo(..)
  , initWithDictionary
  , initWithCoder
  , init_
  , dictionary
  , setUpPrintOperationDefaultValues
  , pmPrintSession
  , pmPageFormat
  , pmPrintSettings
  , updateFromPMPageFormat
  , updateFromPMPrintSettings
  , takeSettingsFromPDFInfo
  , setDefaultPrinter
  , sizeForPaperName
  , sharedPrintInfo
  , setSharedPrintInfo
  , paperName
  , setPaperName
  , paperSize
  , setPaperSize
  , orientation
  , setOrientation
  , scalingFactor
  , setScalingFactor
  , leftMargin
  , setLeftMargin
  , rightMargin
  , setRightMargin
  , topMargin
  , setTopMargin
  , bottomMargin
  , setBottomMargin
  , horizontallyCentered
  , setHorizontallyCentered
  , verticallyCentered
  , setVerticallyCentered
  , horizontalPagination
  , setHorizontalPagination
  , verticalPagination
  , setVerticalPagination
  , jobDisposition
  , setJobDisposition
  , printer
  , setPrinter
  , imageablePageBounds
  , localizedPaperName
  , defaultPrinter
  , printSettings
  , selectionOnly
  , setSelectionOnly
  , bottomMarginSelector
  , defaultPrinterSelector
  , dictionarySelector
  , horizontalPaginationSelector
  , horizontallyCenteredSelector
  , imageablePageBoundsSelector
  , initSelector
  , initWithCoderSelector
  , initWithDictionarySelector
  , jobDispositionSelector
  , leftMarginSelector
  , localizedPaperNameSelector
  , orientationSelector
  , paperNameSelector
  , paperSizeSelector
  , pmPageFormatSelector
  , pmPrintSessionSelector
  , pmPrintSettingsSelector
  , printSettingsSelector
  , printerSelector
  , rightMarginSelector
  , scalingFactorSelector
  , selectionOnlySelector
  , setBottomMarginSelector
  , setDefaultPrinterSelector
  , setHorizontalPaginationSelector
  , setHorizontallyCenteredSelector
  , setJobDispositionSelector
  , setLeftMarginSelector
  , setOrientationSelector
  , setPaperNameSelector
  , setPaperSizeSelector
  , setPrinterSelector
  , setRightMarginSelector
  , setScalingFactorSelector
  , setSelectionOnlySelector
  , setSharedPrintInfoSelector
  , setTopMarginSelector
  , setUpPrintOperationDefaultValuesSelector
  , setVerticalPaginationSelector
  , setVerticallyCenteredSelector
  , sharedPrintInfoSelector
  , sizeForPaperNameSelector
  , takeSettingsFromPDFInfoSelector
  , topMarginSelector
  , updateFromPMPageFormatSelector
  , updateFromPMPrintSettingsSelector
  , verticalPaginationSelector
  , verticallyCenteredSelector

  -- * Enum types
  , NSPaperOrientation(NSPaperOrientation)
  , pattern NSPaperOrientationPortrait
  , pattern NSPaperOrientationLandscape
  , NSPrintingPaginationMode(NSPrintingPaginationMode)
  , pattern NSPrintingPaginationModeAutomatic
  , pattern NSPrintingPaginationModeFit
  , pattern NSPrintingPaginationModeClip

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithDictionary:@
initWithDictionary :: (IsNSPrintInfo nsPrintInfo, IsNSDictionary attributes) => nsPrintInfo -> attributes -> IO (Id NSPrintInfo)
initWithDictionary nsPrintInfo attributes =
  sendOwnedMessage nsPrintInfo initWithDictionarySelector (toNSDictionary attributes)

-- | @- initWithCoder:@
initWithCoder :: (IsNSPrintInfo nsPrintInfo, IsNSCoder coder) => nsPrintInfo -> coder -> IO (Id NSPrintInfo)
initWithCoder nsPrintInfo coder =
  sendOwnedMessage nsPrintInfo initWithCoderSelector (toNSCoder coder)

-- | @- init@
init_ :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO (Id NSPrintInfo)
init_ nsPrintInfo =
  sendOwnedMessage nsPrintInfo initSelector

-- | @- dictionary@
dictionary :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO (Id NSMutableDictionary)
dictionary nsPrintInfo =
  sendMessage nsPrintInfo dictionarySelector

-- | @- setUpPrintOperationDefaultValues@
setUpPrintOperationDefaultValues :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO ()
setUpPrintOperationDefaultValues nsPrintInfo =
  sendMessage nsPrintInfo setUpPrintOperationDefaultValuesSelector

-- | @- PMPrintSession@
pmPrintSession :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO (Ptr ())
pmPrintSession nsPrintInfo =
  sendMessage nsPrintInfo pmPrintSessionSelector

-- | @- PMPageFormat@
pmPageFormat :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO (Ptr ())
pmPageFormat nsPrintInfo =
  sendMessage nsPrintInfo pmPageFormatSelector

-- | @- PMPrintSettings@
pmPrintSettings :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO (Ptr ())
pmPrintSettings nsPrintInfo =
  sendMessage nsPrintInfo pmPrintSettingsSelector

-- | @- updateFromPMPageFormat@
updateFromPMPageFormat :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO ()
updateFromPMPageFormat nsPrintInfo =
  sendMessage nsPrintInfo updateFromPMPageFormatSelector

-- | @- updateFromPMPrintSettings@
updateFromPMPrintSettings :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO ()
updateFromPMPrintSettings nsPrintInfo =
  sendMessage nsPrintInfo updateFromPMPrintSettingsSelector

-- | @- takeSettingsFromPDFInfo:@
takeSettingsFromPDFInfo :: (IsNSPrintInfo nsPrintInfo, IsNSPDFInfo inPDFInfo) => nsPrintInfo -> inPDFInfo -> IO ()
takeSettingsFromPDFInfo nsPrintInfo inPDFInfo =
  sendMessage nsPrintInfo takeSettingsFromPDFInfoSelector (toNSPDFInfo inPDFInfo)

-- | @+ setDefaultPrinter:@
setDefaultPrinter :: IsNSPrinter printer => printer -> IO ()
setDefaultPrinter printer =
  do
    cls' <- getRequiredClass "NSPrintInfo"
    sendClassMessage cls' setDefaultPrinterSelector (toNSPrinter printer)

-- | @+ sizeForPaperName:@
sizeForPaperName :: IsNSString name => name -> IO NSSize
sizeForPaperName name =
  do
    cls' <- getRequiredClass "NSPrintInfo"
    sendClassMessage cls' sizeForPaperNameSelector (toNSString name)

-- | @+ sharedPrintInfo@
sharedPrintInfo :: IO (Id NSPrintInfo)
sharedPrintInfo  =
  do
    cls' <- getRequiredClass "NSPrintInfo"
    sendClassMessage cls' sharedPrintInfoSelector

-- | @+ setSharedPrintInfo:@
setSharedPrintInfo :: IsNSPrintInfo value => value -> IO ()
setSharedPrintInfo value =
  do
    cls' <- getRequiredClass "NSPrintInfo"
    sendClassMessage cls' setSharedPrintInfoSelector (toNSPrintInfo value)

-- | @- paperName@
paperName :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO (Id NSString)
paperName nsPrintInfo =
  sendMessage nsPrintInfo paperNameSelector

-- | @- setPaperName:@
setPaperName :: (IsNSPrintInfo nsPrintInfo, IsNSString value) => nsPrintInfo -> value -> IO ()
setPaperName nsPrintInfo value =
  sendMessage nsPrintInfo setPaperNameSelector (toNSString value)

-- | @- paperSize@
paperSize :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO NSSize
paperSize nsPrintInfo =
  sendMessage nsPrintInfo paperSizeSelector

-- | @- setPaperSize:@
setPaperSize :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> NSSize -> IO ()
setPaperSize nsPrintInfo value =
  sendMessage nsPrintInfo setPaperSizeSelector value

-- | @- orientation@
orientation :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO NSPaperOrientation
orientation nsPrintInfo =
  sendMessage nsPrintInfo orientationSelector

-- | @- setOrientation:@
setOrientation :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> NSPaperOrientation -> IO ()
setOrientation nsPrintInfo value =
  sendMessage nsPrintInfo setOrientationSelector value

-- | @- scalingFactor@
scalingFactor :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO CDouble
scalingFactor nsPrintInfo =
  sendMessage nsPrintInfo scalingFactorSelector

-- | @- setScalingFactor:@
setScalingFactor :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> CDouble -> IO ()
setScalingFactor nsPrintInfo value =
  sendMessage nsPrintInfo setScalingFactorSelector value

-- | @- leftMargin@
leftMargin :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO CDouble
leftMargin nsPrintInfo =
  sendMessage nsPrintInfo leftMarginSelector

-- | @- setLeftMargin:@
setLeftMargin :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> CDouble -> IO ()
setLeftMargin nsPrintInfo value =
  sendMessage nsPrintInfo setLeftMarginSelector value

-- | @- rightMargin@
rightMargin :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO CDouble
rightMargin nsPrintInfo =
  sendMessage nsPrintInfo rightMarginSelector

-- | @- setRightMargin:@
setRightMargin :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> CDouble -> IO ()
setRightMargin nsPrintInfo value =
  sendMessage nsPrintInfo setRightMarginSelector value

-- | @- topMargin@
topMargin :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO CDouble
topMargin nsPrintInfo =
  sendMessage nsPrintInfo topMarginSelector

-- | @- setTopMargin:@
setTopMargin :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> CDouble -> IO ()
setTopMargin nsPrintInfo value =
  sendMessage nsPrintInfo setTopMarginSelector value

-- | @- bottomMargin@
bottomMargin :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO CDouble
bottomMargin nsPrintInfo =
  sendMessage nsPrintInfo bottomMarginSelector

-- | @- setBottomMargin:@
setBottomMargin :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> CDouble -> IO ()
setBottomMargin nsPrintInfo value =
  sendMessage nsPrintInfo setBottomMarginSelector value

-- | @- horizontallyCentered@
horizontallyCentered :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO Bool
horizontallyCentered nsPrintInfo =
  sendMessage nsPrintInfo horizontallyCenteredSelector

-- | @- setHorizontallyCentered:@
setHorizontallyCentered :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> Bool -> IO ()
setHorizontallyCentered nsPrintInfo value =
  sendMessage nsPrintInfo setHorizontallyCenteredSelector value

-- | @- verticallyCentered@
verticallyCentered :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO Bool
verticallyCentered nsPrintInfo =
  sendMessage nsPrintInfo verticallyCenteredSelector

-- | @- setVerticallyCentered:@
setVerticallyCentered :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> Bool -> IO ()
setVerticallyCentered nsPrintInfo value =
  sendMessage nsPrintInfo setVerticallyCenteredSelector value

-- | @- horizontalPagination@
horizontalPagination :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO NSPrintingPaginationMode
horizontalPagination nsPrintInfo =
  sendMessage nsPrintInfo horizontalPaginationSelector

-- | @- setHorizontalPagination:@
setHorizontalPagination :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> NSPrintingPaginationMode -> IO ()
setHorizontalPagination nsPrintInfo value =
  sendMessage nsPrintInfo setHorizontalPaginationSelector value

-- | @- verticalPagination@
verticalPagination :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO NSPrintingPaginationMode
verticalPagination nsPrintInfo =
  sendMessage nsPrintInfo verticalPaginationSelector

-- | @- setVerticalPagination:@
setVerticalPagination :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> NSPrintingPaginationMode -> IO ()
setVerticalPagination nsPrintInfo value =
  sendMessage nsPrintInfo setVerticalPaginationSelector value

-- | @- jobDisposition@
jobDisposition :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO (Id NSString)
jobDisposition nsPrintInfo =
  sendMessage nsPrintInfo jobDispositionSelector

-- | @- setJobDisposition:@
setJobDisposition :: (IsNSPrintInfo nsPrintInfo, IsNSString value) => nsPrintInfo -> value -> IO ()
setJobDisposition nsPrintInfo value =
  sendMessage nsPrintInfo setJobDispositionSelector (toNSString value)

-- | @- printer@
printer :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO (Id NSPrinter)
printer nsPrintInfo =
  sendMessage nsPrintInfo printerSelector

-- | @- setPrinter:@
setPrinter :: (IsNSPrintInfo nsPrintInfo, IsNSPrinter value) => nsPrintInfo -> value -> IO ()
setPrinter nsPrintInfo value =
  sendMessage nsPrintInfo setPrinterSelector (toNSPrinter value)

-- | @- imageablePageBounds@
imageablePageBounds :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO NSRect
imageablePageBounds nsPrintInfo =
  sendMessage nsPrintInfo imageablePageBoundsSelector

-- | @- localizedPaperName@
localizedPaperName :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO (Id NSString)
localizedPaperName nsPrintInfo =
  sendMessage nsPrintInfo localizedPaperNameSelector

-- | @+ defaultPrinter@
defaultPrinter :: IO (Id NSPrinter)
defaultPrinter  =
  do
    cls' <- getRequiredClass "NSPrintInfo"
    sendClassMessage cls' defaultPrinterSelector

-- | @- printSettings@
printSettings :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO (Id NSMutableDictionary)
printSettings nsPrintInfo =
  sendMessage nsPrintInfo printSettingsSelector

-- | @- selectionOnly@
selectionOnly :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO Bool
selectionOnly nsPrintInfo =
  sendMessage nsPrintInfo selectionOnlySelector

-- | @- setSelectionOnly:@
setSelectionOnly :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> Bool -> IO ()
setSelectionOnly nsPrintInfo value =
  sendMessage nsPrintInfo setSelectionOnlySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDictionary:@
initWithDictionarySelector :: Selector '[Id NSDictionary] (Id NSPrintInfo)
initWithDictionarySelector = mkSelector "initWithDictionary:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSPrintInfo)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSPrintInfo)
initSelector = mkSelector "init"

-- | @Selector@ for @dictionary@
dictionarySelector :: Selector '[] (Id NSMutableDictionary)
dictionarySelector = mkSelector "dictionary"

-- | @Selector@ for @setUpPrintOperationDefaultValues@
setUpPrintOperationDefaultValuesSelector :: Selector '[] ()
setUpPrintOperationDefaultValuesSelector = mkSelector "setUpPrintOperationDefaultValues"

-- | @Selector@ for @PMPrintSession@
pmPrintSessionSelector :: Selector '[] (Ptr ())
pmPrintSessionSelector = mkSelector "PMPrintSession"

-- | @Selector@ for @PMPageFormat@
pmPageFormatSelector :: Selector '[] (Ptr ())
pmPageFormatSelector = mkSelector "PMPageFormat"

-- | @Selector@ for @PMPrintSettings@
pmPrintSettingsSelector :: Selector '[] (Ptr ())
pmPrintSettingsSelector = mkSelector "PMPrintSettings"

-- | @Selector@ for @updateFromPMPageFormat@
updateFromPMPageFormatSelector :: Selector '[] ()
updateFromPMPageFormatSelector = mkSelector "updateFromPMPageFormat"

-- | @Selector@ for @updateFromPMPrintSettings@
updateFromPMPrintSettingsSelector :: Selector '[] ()
updateFromPMPrintSettingsSelector = mkSelector "updateFromPMPrintSettings"

-- | @Selector@ for @takeSettingsFromPDFInfo:@
takeSettingsFromPDFInfoSelector :: Selector '[Id NSPDFInfo] ()
takeSettingsFromPDFInfoSelector = mkSelector "takeSettingsFromPDFInfo:"

-- | @Selector@ for @setDefaultPrinter:@
setDefaultPrinterSelector :: Selector '[Id NSPrinter] ()
setDefaultPrinterSelector = mkSelector "setDefaultPrinter:"

-- | @Selector@ for @sizeForPaperName:@
sizeForPaperNameSelector :: Selector '[Id NSString] NSSize
sizeForPaperNameSelector = mkSelector "sizeForPaperName:"

-- | @Selector@ for @sharedPrintInfo@
sharedPrintInfoSelector :: Selector '[] (Id NSPrintInfo)
sharedPrintInfoSelector = mkSelector "sharedPrintInfo"

-- | @Selector@ for @setSharedPrintInfo:@
setSharedPrintInfoSelector :: Selector '[Id NSPrintInfo] ()
setSharedPrintInfoSelector = mkSelector "setSharedPrintInfo:"

-- | @Selector@ for @paperName@
paperNameSelector :: Selector '[] (Id NSString)
paperNameSelector = mkSelector "paperName"

-- | @Selector@ for @setPaperName:@
setPaperNameSelector :: Selector '[Id NSString] ()
setPaperNameSelector = mkSelector "setPaperName:"

-- | @Selector@ for @paperSize@
paperSizeSelector :: Selector '[] NSSize
paperSizeSelector = mkSelector "paperSize"

-- | @Selector@ for @setPaperSize:@
setPaperSizeSelector :: Selector '[NSSize] ()
setPaperSizeSelector = mkSelector "setPaperSize:"

-- | @Selector@ for @orientation@
orientationSelector :: Selector '[] NSPaperOrientation
orientationSelector = mkSelector "orientation"

-- | @Selector@ for @setOrientation:@
setOrientationSelector :: Selector '[NSPaperOrientation] ()
setOrientationSelector = mkSelector "setOrientation:"

-- | @Selector@ for @scalingFactor@
scalingFactorSelector :: Selector '[] CDouble
scalingFactorSelector = mkSelector "scalingFactor"

-- | @Selector@ for @setScalingFactor:@
setScalingFactorSelector :: Selector '[CDouble] ()
setScalingFactorSelector = mkSelector "setScalingFactor:"

-- | @Selector@ for @leftMargin@
leftMarginSelector :: Selector '[] CDouble
leftMarginSelector = mkSelector "leftMargin"

-- | @Selector@ for @setLeftMargin:@
setLeftMarginSelector :: Selector '[CDouble] ()
setLeftMarginSelector = mkSelector "setLeftMargin:"

-- | @Selector@ for @rightMargin@
rightMarginSelector :: Selector '[] CDouble
rightMarginSelector = mkSelector "rightMargin"

-- | @Selector@ for @setRightMargin:@
setRightMarginSelector :: Selector '[CDouble] ()
setRightMarginSelector = mkSelector "setRightMargin:"

-- | @Selector@ for @topMargin@
topMarginSelector :: Selector '[] CDouble
topMarginSelector = mkSelector "topMargin"

-- | @Selector@ for @setTopMargin:@
setTopMarginSelector :: Selector '[CDouble] ()
setTopMarginSelector = mkSelector "setTopMargin:"

-- | @Selector@ for @bottomMargin@
bottomMarginSelector :: Selector '[] CDouble
bottomMarginSelector = mkSelector "bottomMargin"

-- | @Selector@ for @setBottomMargin:@
setBottomMarginSelector :: Selector '[CDouble] ()
setBottomMarginSelector = mkSelector "setBottomMargin:"

-- | @Selector@ for @horizontallyCentered@
horizontallyCenteredSelector :: Selector '[] Bool
horizontallyCenteredSelector = mkSelector "horizontallyCentered"

-- | @Selector@ for @setHorizontallyCentered:@
setHorizontallyCenteredSelector :: Selector '[Bool] ()
setHorizontallyCenteredSelector = mkSelector "setHorizontallyCentered:"

-- | @Selector@ for @verticallyCentered@
verticallyCenteredSelector :: Selector '[] Bool
verticallyCenteredSelector = mkSelector "verticallyCentered"

-- | @Selector@ for @setVerticallyCentered:@
setVerticallyCenteredSelector :: Selector '[Bool] ()
setVerticallyCenteredSelector = mkSelector "setVerticallyCentered:"

-- | @Selector@ for @horizontalPagination@
horizontalPaginationSelector :: Selector '[] NSPrintingPaginationMode
horizontalPaginationSelector = mkSelector "horizontalPagination"

-- | @Selector@ for @setHorizontalPagination:@
setHorizontalPaginationSelector :: Selector '[NSPrintingPaginationMode] ()
setHorizontalPaginationSelector = mkSelector "setHorizontalPagination:"

-- | @Selector@ for @verticalPagination@
verticalPaginationSelector :: Selector '[] NSPrintingPaginationMode
verticalPaginationSelector = mkSelector "verticalPagination"

-- | @Selector@ for @setVerticalPagination:@
setVerticalPaginationSelector :: Selector '[NSPrintingPaginationMode] ()
setVerticalPaginationSelector = mkSelector "setVerticalPagination:"

-- | @Selector@ for @jobDisposition@
jobDispositionSelector :: Selector '[] (Id NSString)
jobDispositionSelector = mkSelector "jobDisposition"

-- | @Selector@ for @setJobDisposition:@
setJobDispositionSelector :: Selector '[Id NSString] ()
setJobDispositionSelector = mkSelector "setJobDisposition:"

-- | @Selector@ for @printer@
printerSelector :: Selector '[] (Id NSPrinter)
printerSelector = mkSelector "printer"

-- | @Selector@ for @setPrinter:@
setPrinterSelector :: Selector '[Id NSPrinter] ()
setPrinterSelector = mkSelector "setPrinter:"

-- | @Selector@ for @imageablePageBounds@
imageablePageBoundsSelector :: Selector '[] NSRect
imageablePageBoundsSelector = mkSelector "imageablePageBounds"

-- | @Selector@ for @localizedPaperName@
localizedPaperNameSelector :: Selector '[] (Id NSString)
localizedPaperNameSelector = mkSelector "localizedPaperName"

-- | @Selector@ for @defaultPrinter@
defaultPrinterSelector :: Selector '[] (Id NSPrinter)
defaultPrinterSelector = mkSelector "defaultPrinter"

-- | @Selector@ for @printSettings@
printSettingsSelector :: Selector '[] (Id NSMutableDictionary)
printSettingsSelector = mkSelector "printSettings"

-- | @Selector@ for @selectionOnly@
selectionOnlySelector :: Selector '[] Bool
selectionOnlySelector = mkSelector "selectionOnly"

-- | @Selector@ for @setSelectionOnly:@
setSelectionOnlySelector :: Selector '[Bool] ()
setSelectionOnlySelector = mkSelector "setSelectionOnly:"

