{-# LANGUAGE PatternSynonyms #-}
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
  , initWithDictionarySelector
  , initWithCoderSelector
  , initSelector
  , dictionarySelector
  , setUpPrintOperationDefaultValuesSelector
  , pmPrintSessionSelector
  , pmPageFormatSelector
  , pmPrintSettingsSelector
  , updateFromPMPageFormatSelector
  , updateFromPMPrintSettingsSelector
  , takeSettingsFromPDFInfoSelector
  , setDefaultPrinterSelector
  , sizeForPaperNameSelector
  , sharedPrintInfoSelector
  , setSharedPrintInfoSelector
  , paperNameSelector
  , setPaperNameSelector
  , paperSizeSelector
  , setPaperSizeSelector
  , orientationSelector
  , setOrientationSelector
  , scalingFactorSelector
  , setScalingFactorSelector
  , leftMarginSelector
  , setLeftMarginSelector
  , rightMarginSelector
  , setRightMarginSelector
  , topMarginSelector
  , setTopMarginSelector
  , bottomMarginSelector
  , setBottomMarginSelector
  , horizontallyCenteredSelector
  , setHorizontallyCenteredSelector
  , verticallyCenteredSelector
  , setVerticallyCenteredSelector
  , horizontalPaginationSelector
  , setHorizontalPaginationSelector
  , verticalPaginationSelector
  , setVerticalPaginationSelector
  , jobDispositionSelector
  , setJobDispositionSelector
  , printerSelector
  , setPrinterSelector
  , imageablePageBoundsSelector
  , localizedPaperNameSelector
  , defaultPrinterSelector
  , printSettingsSelector
  , selectionOnlySelector
  , setSelectionOnlySelector

  -- * Enum types
  , NSPaperOrientation(NSPaperOrientation)
  , pattern NSPaperOrientationPortrait
  , pattern NSPaperOrientationLandscape
  , NSPrintingPaginationMode(NSPrintingPaginationMode)
  , pattern NSPrintingPaginationModeAutomatic
  , pattern NSPrintingPaginationModeFit
  , pattern NSPrintingPaginationModeClip

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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithDictionary:@
initWithDictionary :: (IsNSPrintInfo nsPrintInfo, IsNSDictionary attributes) => nsPrintInfo -> attributes -> IO (Id NSPrintInfo)
initWithDictionary nsPrintInfo  attributes =
  withObjCPtr attributes $ \raw_attributes ->
      sendMsg nsPrintInfo (mkSelector "initWithDictionary:") (retPtr retVoid) [argPtr (castPtr raw_attributes :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSPrintInfo nsPrintInfo, IsNSCoder coder) => nsPrintInfo -> coder -> IO (Id NSPrintInfo)
initWithCoder nsPrintInfo  coder =
  withObjCPtr coder $ \raw_coder ->
      sendMsg nsPrintInfo (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO (Id NSPrintInfo)
init_ nsPrintInfo  =
    sendMsg nsPrintInfo (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- dictionary@
dictionary :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO (Id NSMutableDictionary)
dictionary nsPrintInfo  =
    sendMsg nsPrintInfo (mkSelector "dictionary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUpPrintOperationDefaultValues@
setUpPrintOperationDefaultValues :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO ()
setUpPrintOperationDefaultValues nsPrintInfo  =
    sendMsg nsPrintInfo (mkSelector "setUpPrintOperationDefaultValues") retVoid []

-- | @- PMPrintSession@
pmPrintSession :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO (Ptr ())
pmPrintSession nsPrintInfo  =
    fmap castPtr $ sendMsg nsPrintInfo (mkSelector "PMPrintSession") (retPtr retVoid) []

-- | @- PMPageFormat@
pmPageFormat :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO (Ptr ())
pmPageFormat nsPrintInfo  =
    fmap castPtr $ sendMsg nsPrintInfo (mkSelector "PMPageFormat") (retPtr retVoid) []

-- | @- PMPrintSettings@
pmPrintSettings :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO (Ptr ())
pmPrintSettings nsPrintInfo  =
    fmap castPtr $ sendMsg nsPrintInfo (mkSelector "PMPrintSettings") (retPtr retVoid) []

-- | @- updateFromPMPageFormat@
updateFromPMPageFormat :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO ()
updateFromPMPageFormat nsPrintInfo  =
    sendMsg nsPrintInfo (mkSelector "updateFromPMPageFormat") retVoid []

-- | @- updateFromPMPrintSettings@
updateFromPMPrintSettings :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO ()
updateFromPMPrintSettings nsPrintInfo  =
    sendMsg nsPrintInfo (mkSelector "updateFromPMPrintSettings") retVoid []

-- | @- takeSettingsFromPDFInfo:@
takeSettingsFromPDFInfo :: (IsNSPrintInfo nsPrintInfo, IsNSPDFInfo inPDFInfo) => nsPrintInfo -> inPDFInfo -> IO ()
takeSettingsFromPDFInfo nsPrintInfo  inPDFInfo =
  withObjCPtr inPDFInfo $ \raw_inPDFInfo ->
      sendMsg nsPrintInfo (mkSelector "takeSettingsFromPDFInfo:") retVoid [argPtr (castPtr raw_inPDFInfo :: Ptr ())]

-- | @+ setDefaultPrinter:@
setDefaultPrinter :: IsNSPrinter printer => printer -> IO ()
setDefaultPrinter printer =
  do
    cls' <- getRequiredClass "NSPrintInfo"
    withObjCPtr printer $ \raw_printer ->
      sendClassMsg cls' (mkSelector "setDefaultPrinter:") retVoid [argPtr (castPtr raw_printer :: Ptr ())]

-- | @+ sizeForPaperName:@
sizeForPaperName :: IsNSString name => name -> IO NSSize
sizeForPaperName name =
  do
    cls' <- getRequiredClass "NSPrintInfo"
    withObjCPtr name $ \raw_name ->
      sendClassMsgStret cls' (mkSelector "sizeForPaperName:") retNSSize [argPtr (castPtr raw_name :: Ptr ())]

-- | @+ sharedPrintInfo@
sharedPrintInfo :: IO (Id NSPrintInfo)
sharedPrintInfo  =
  do
    cls' <- getRequiredClass "NSPrintInfo"
    sendClassMsg cls' (mkSelector "sharedPrintInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ setSharedPrintInfo:@
setSharedPrintInfo :: IsNSPrintInfo value => value -> IO ()
setSharedPrintInfo value =
  do
    cls' <- getRequiredClass "NSPrintInfo"
    withObjCPtr value $ \raw_value ->
      sendClassMsg cls' (mkSelector "setSharedPrintInfo:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- paperName@
paperName :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO (Id NSString)
paperName nsPrintInfo  =
    sendMsg nsPrintInfo (mkSelector "paperName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPaperName:@
setPaperName :: (IsNSPrintInfo nsPrintInfo, IsNSString value) => nsPrintInfo -> value -> IO ()
setPaperName nsPrintInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsPrintInfo (mkSelector "setPaperName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- paperSize@
paperSize :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO NSSize
paperSize nsPrintInfo  =
    sendMsgStret nsPrintInfo (mkSelector "paperSize") retNSSize []

-- | @- setPaperSize:@
setPaperSize :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> NSSize -> IO ()
setPaperSize nsPrintInfo  value =
    sendMsg nsPrintInfo (mkSelector "setPaperSize:") retVoid [argNSSize value]

-- | @- orientation@
orientation :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO NSPaperOrientation
orientation nsPrintInfo  =
    fmap (coerce :: CLong -> NSPaperOrientation) $ sendMsg nsPrintInfo (mkSelector "orientation") retCLong []

-- | @- setOrientation:@
setOrientation :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> NSPaperOrientation -> IO ()
setOrientation nsPrintInfo  value =
    sendMsg nsPrintInfo (mkSelector "setOrientation:") retVoid [argCLong (coerce value)]

-- | @- scalingFactor@
scalingFactor :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO CDouble
scalingFactor nsPrintInfo  =
    sendMsg nsPrintInfo (mkSelector "scalingFactor") retCDouble []

-- | @- setScalingFactor:@
setScalingFactor :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> CDouble -> IO ()
setScalingFactor nsPrintInfo  value =
    sendMsg nsPrintInfo (mkSelector "setScalingFactor:") retVoid [argCDouble value]

-- | @- leftMargin@
leftMargin :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO CDouble
leftMargin nsPrintInfo  =
    sendMsg nsPrintInfo (mkSelector "leftMargin") retCDouble []

-- | @- setLeftMargin:@
setLeftMargin :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> CDouble -> IO ()
setLeftMargin nsPrintInfo  value =
    sendMsg nsPrintInfo (mkSelector "setLeftMargin:") retVoid [argCDouble value]

-- | @- rightMargin@
rightMargin :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO CDouble
rightMargin nsPrintInfo  =
    sendMsg nsPrintInfo (mkSelector "rightMargin") retCDouble []

-- | @- setRightMargin:@
setRightMargin :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> CDouble -> IO ()
setRightMargin nsPrintInfo  value =
    sendMsg nsPrintInfo (mkSelector "setRightMargin:") retVoid [argCDouble value]

-- | @- topMargin@
topMargin :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO CDouble
topMargin nsPrintInfo  =
    sendMsg nsPrintInfo (mkSelector "topMargin") retCDouble []

-- | @- setTopMargin:@
setTopMargin :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> CDouble -> IO ()
setTopMargin nsPrintInfo  value =
    sendMsg nsPrintInfo (mkSelector "setTopMargin:") retVoid [argCDouble value]

-- | @- bottomMargin@
bottomMargin :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO CDouble
bottomMargin nsPrintInfo  =
    sendMsg nsPrintInfo (mkSelector "bottomMargin") retCDouble []

-- | @- setBottomMargin:@
setBottomMargin :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> CDouble -> IO ()
setBottomMargin nsPrintInfo  value =
    sendMsg nsPrintInfo (mkSelector "setBottomMargin:") retVoid [argCDouble value]

-- | @- horizontallyCentered@
horizontallyCentered :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO Bool
horizontallyCentered nsPrintInfo  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPrintInfo (mkSelector "horizontallyCentered") retCULong []

-- | @- setHorizontallyCentered:@
setHorizontallyCentered :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> Bool -> IO ()
setHorizontallyCentered nsPrintInfo  value =
    sendMsg nsPrintInfo (mkSelector "setHorizontallyCentered:") retVoid [argCULong (if value then 1 else 0)]

-- | @- verticallyCentered@
verticallyCentered :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO Bool
verticallyCentered nsPrintInfo  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPrintInfo (mkSelector "verticallyCentered") retCULong []

-- | @- setVerticallyCentered:@
setVerticallyCentered :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> Bool -> IO ()
setVerticallyCentered nsPrintInfo  value =
    sendMsg nsPrintInfo (mkSelector "setVerticallyCentered:") retVoid [argCULong (if value then 1 else 0)]

-- | @- horizontalPagination@
horizontalPagination :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO NSPrintingPaginationMode
horizontalPagination nsPrintInfo  =
    fmap (coerce :: CULong -> NSPrintingPaginationMode) $ sendMsg nsPrintInfo (mkSelector "horizontalPagination") retCULong []

-- | @- setHorizontalPagination:@
setHorizontalPagination :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> NSPrintingPaginationMode -> IO ()
setHorizontalPagination nsPrintInfo  value =
    sendMsg nsPrintInfo (mkSelector "setHorizontalPagination:") retVoid [argCULong (coerce value)]

-- | @- verticalPagination@
verticalPagination :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO NSPrintingPaginationMode
verticalPagination nsPrintInfo  =
    fmap (coerce :: CULong -> NSPrintingPaginationMode) $ sendMsg nsPrintInfo (mkSelector "verticalPagination") retCULong []

-- | @- setVerticalPagination:@
setVerticalPagination :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> NSPrintingPaginationMode -> IO ()
setVerticalPagination nsPrintInfo  value =
    sendMsg nsPrintInfo (mkSelector "setVerticalPagination:") retVoid [argCULong (coerce value)]

-- | @- jobDisposition@
jobDisposition :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO (Id NSString)
jobDisposition nsPrintInfo  =
    sendMsg nsPrintInfo (mkSelector "jobDisposition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setJobDisposition:@
setJobDisposition :: (IsNSPrintInfo nsPrintInfo, IsNSString value) => nsPrintInfo -> value -> IO ()
setJobDisposition nsPrintInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsPrintInfo (mkSelector "setJobDisposition:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- printer@
printer :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO (Id NSPrinter)
printer nsPrintInfo  =
    sendMsg nsPrintInfo (mkSelector "printer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPrinter:@
setPrinter :: (IsNSPrintInfo nsPrintInfo, IsNSPrinter value) => nsPrintInfo -> value -> IO ()
setPrinter nsPrintInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsPrintInfo (mkSelector "setPrinter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- imageablePageBounds@
imageablePageBounds :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO NSRect
imageablePageBounds nsPrintInfo  =
    sendMsgStret nsPrintInfo (mkSelector "imageablePageBounds") retNSRect []

-- | @- localizedPaperName@
localizedPaperName :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO (Id NSString)
localizedPaperName nsPrintInfo  =
    sendMsg nsPrintInfo (mkSelector "localizedPaperName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ defaultPrinter@
defaultPrinter :: IO (Id NSPrinter)
defaultPrinter  =
  do
    cls' <- getRequiredClass "NSPrintInfo"
    sendClassMsg cls' (mkSelector "defaultPrinter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- printSettings@
printSettings :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO (Id NSMutableDictionary)
printSettings nsPrintInfo  =
    sendMsg nsPrintInfo (mkSelector "printSettings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- selectionOnly@
selectionOnly :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> IO Bool
selectionOnly nsPrintInfo  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPrintInfo (mkSelector "selectionOnly") retCULong []

-- | @- setSelectionOnly:@
setSelectionOnly :: IsNSPrintInfo nsPrintInfo => nsPrintInfo -> Bool -> IO ()
setSelectionOnly nsPrintInfo  value =
    sendMsg nsPrintInfo (mkSelector "setSelectionOnly:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDictionary:@
initWithDictionarySelector :: Selector
initWithDictionarySelector = mkSelector "initWithDictionary:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @dictionary@
dictionarySelector :: Selector
dictionarySelector = mkSelector "dictionary"

-- | @Selector@ for @setUpPrintOperationDefaultValues@
setUpPrintOperationDefaultValuesSelector :: Selector
setUpPrintOperationDefaultValuesSelector = mkSelector "setUpPrintOperationDefaultValues"

-- | @Selector@ for @PMPrintSession@
pmPrintSessionSelector :: Selector
pmPrintSessionSelector = mkSelector "PMPrintSession"

-- | @Selector@ for @PMPageFormat@
pmPageFormatSelector :: Selector
pmPageFormatSelector = mkSelector "PMPageFormat"

-- | @Selector@ for @PMPrintSettings@
pmPrintSettingsSelector :: Selector
pmPrintSettingsSelector = mkSelector "PMPrintSettings"

-- | @Selector@ for @updateFromPMPageFormat@
updateFromPMPageFormatSelector :: Selector
updateFromPMPageFormatSelector = mkSelector "updateFromPMPageFormat"

-- | @Selector@ for @updateFromPMPrintSettings@
updateFromPMPrintSettingsSelector :: Selector
updateFromPMPrintSettingsSelector = mkSelector "updateFromPMPrintSettings"

-- | @Selector@ for @takeSettingsFromPDFInfo:@
takeSettingsFromPDFInfoSelector :: Selector
takeSettingsFromPDFInfoSelector = mkSelector "takeSettingsFromPDFInfo:"

-- | @Selector@ for @setDefaultPrinter:@
setDefaultPrinterSelector :: Selector
setDefaultPrinterSelector = mkSelector "setDefaultPrinter:"

-- | @Selector@ for @sizeForPaperName:@
sizeForPaperNameSelector :: Selector
sizeForPaperNameSelector = mkSelector "sizeForPaperName:"

-- | @Selector@ for @sharedPrintInfo@
sharedPrintInfoSelector :: Selector
sharedPrintInfoSelector = mkSelector "sharedPrintInfo"

-- | @Selector@ for @setSharedPrintInfo:@
setSharedPrintInfoSelector :: Selector
setSharedPrintInfoSelector = mkSelector "setSharedPrintInfo:"

-- | @Selector@ for @paperName@
paperNameSelector :: Selector
paperNameSelector = mkSelector "paperName"

-- | @Selector@ for @setPaperName:@
setPaperNameSelector :: Selector
setPaperNameSelector = mkSelector "setPaperName:"

-- | @Selector@ for @paperSize@
paperSizeSelector :: Selector
paperSizeSelector = mkSelector "paperSize"

-- | @Selector@ for @setPaperSize:@
setPaperSizeSelector :: Selector
setPaperSizeSelector = mkSelector "setPaperSize:"

-- | @Selector@ for @orientation@
orientationSelector :: Selector
orientationSelector = mkSelector "orientation"

-- | @Selector@ for @setOrientation:@
setOrientationSelector :: Selector
setOrientationSelector = mkSelector "setOrientation:"

-- | @Selector@ for @scalingFactor@
scalingFactorSelector :: Selector
scalingFactorSelector = mkSelector "scalingFactor"

-- | @Selector@ for @setScalingFactor:@
setScalingFactorSelector :: Selector
setScalingFactorSelector = mkSelector "setScalingFactor:"

-- | @Selector@ for @leftMargin@
leftMarginSelector :: Selector
leftMarginSelector = mkSelector "leftMargin"

-- | @Selector@ for @setLeftMargin:@
setLeftMarginSelector :: Selector
setLeftMarginSelector = mkSelector "setLeftMargin:"

-- | @Selector@ for @rightMargin@
rightMarginSelector :: Selector
rightMarginSelector = mkSelector "rightMargin"

-- | @Selector@ for @setRightMargin:@
setRightMarginSelector :: Selector
setRightMarginSelector = mkSelector "setRightMargin:"

-- | @Selector@ for @topMargin@
topMarginSelector :: Selector
topMarginSelector = mkSelector "topMargin"

-- | @Selector@ for @setTopMargin:@
setTopMarginSelector :: Selector
setTopMarginSelector = mkSelector "setTopMargin:"

-- | @Selector@ for @bottomMargin@
bottomMarginSelector :: Selector
bottomMarginSelector = mkSelector "bottomMargin"

-- | @Selector@ for @setBottomMargin:@
setBottomMarginSelector :: Selector
setBottomMarginSelector = mkSelector "setBottomMargin:"

-- | @Selector@ for @horizontallyCentered@
horizontallyCenteredSelector :: Selector
horizontallyCenteredSelector = mkSelector "horizontallyCentered"

-- | @Selector@ for @setHorizontallyCentered:@
setHorizontallyCenteredSelector :: Selector
setHorizontallyCenteredSelector = mkSelector "setHorizontallyCentered:"

-- | @Selector@ for @verticallyCentered@
verticallyCenteredSelector :: Selector
verticallyCenteredSelector = mkSelector "verticallyCentered"

-- | @Selector@ for @setVerticallyCentered:@
setVerticallyCenteredSelector :: Selector
setVerticallyCenteredSelector = mkSelector "setVerticallyCentered:"

-- | @Selector@ for @horizontalPagination@
horizontalPaginationSelector :: Selector
horizontalPaginationSelector = mkSelector "horizontalPagination"

-- | @Selector@ for @setHorizontalPagination:@
setHorizontalPaginationSelector :: Selector
setHorizontalPaginationSelector = mkSelector "setHorizontalPagination:"

-- | @Selector@ for @verticalPagination@
verticalPaginationSelector :: Selector
verticalPaginationSelector = mkSelector "verticalPagination"

-- | @Selector@ for @setVerticalPagination:@
setVerticalPaginationSelector :: Selector
setVerticalPaginationSelector = mkSelector "setVerticalPagination:"

-- | @Selector@ for @jobDisposition@
jobDispositionSelector :: Selector
jobDispositionSelector = mkSelector "jobDisposition"

-- | @Selector@ for @setJobDisposition:@
setJobDispositionSelector :: Selector
setJobDispositionSelector = mkSelector "setJobDisposition:"

-- | @Selector@ for @printer@
printerSelector :: Selector
printerSelector = mkSelector "printer"

-- | @Selector@ for @setPrinter:@
setPrinterSelector :: Selector
setPrinterSelector = mkSelector "setPrinter:"

-- | @Selector@ for @imageablePageBounds@
imageablePageBoundsSelector :: Selector
imageablePageBoundsSelector = mkSelector "imageablePageBounds"

-- | @Selector@ for @localizedPaperName@
localizedPaperNameSelector :: Selector
localizedPaperNameSelector = mkSelector "localizedPaperName"

-- | @Selector@ for @defaultPrinter@
defaultPrinterSelector :: Selector
defaultPrinterSelector = mkSelector "defaultPrinter"

-- | @Selector@ for @printSettings@
printSettingsSelector :: Selector
printSettingsSelector = mkSelector "printSettings"

-- | @Selector@ for @selectionOnly@
selectionOnlySelector :: Selector
selectionOnlySelector = mkSelector "selectionOnly"

-- | @Selector@ for @setSelectionOnly:@
setSelectionOnlySelector :: Selector
setSelectionOnlySelector = mkSelector "setSelectionOnly:"

