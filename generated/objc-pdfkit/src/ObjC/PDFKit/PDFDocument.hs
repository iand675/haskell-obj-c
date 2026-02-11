{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PDFDocument@.
module ObjC.PDFKit.PDFDocument
  ( PDFDocument
  , IsPDFDocument(..)
  , init_
  , initWithURL
  , initWithData
  , unlockWithPassword
  , dataRepresentation
  , dataRepresentationWithOptions
  , writeToFile
  , writeToFile_withOptions
  , writeToURL
  , writeToURL_withOptions
  , outlineItemForSelection
  , pageAtIndex
  , indexForPage
  , insertPage_atIndex
  , removePageAtIndex
  , exchangePageAtIndex_withPageAtIndex
  , findString_withOptions
  , beginFindString_withOptions
  , beginFindStrings_withOptions
  , findString_fromSelection_withOptions
  , cancelFindString
  , printOperationForPrintInfo_scalingMode_autoRotate
  , selectionFromPage_atPoint_toPage_atPoint
  , selectionFromPage_atPoint_toPage_atPoint_withGranularity
  , selectionFromPage_atCharacterIndex_toPage_atCharacterIndex
  , documentURL
  , documentRef
  , documentAttributes
  , setDocumentAttributes
  , majorVersion
  , minorVersion
  , isEncrypted
  , isLocked
  , allowsPrinting
  , allowsCopying
  , allowsDocumentChanges
  , allowsDocumentAssembly
  , allowsContentAccessibility
  , allowsCommenting
  , allowsFormFieldEntry
  , accessPermissions
  , permissionsStatus
  , string
  , pageCount
  , pageClass
  , isFinding
  , selectionForEntireDocument
  , initSelector
  , initWithURLSelector
  , initWithDataSelector
  , unlockWithPasswordSelector
  , dataRepresentationSelector
  , dataRepresentationWithOptionsSelector
  , writeToFileSelector
  , writeToFile_withOptionsSelector
  , writeToURLSelector
  , writeToURL_withOptionsSelector
  , outlineItemForSelectionSelector
  , pageAtIndexSelector
  , indexForPageSelector
  , insertPage_atIndexSelector
  , removePageAtIndexSelector
  , exchangePageAtIndex_withPageAtIndexSelector
  , findString_withOptionsSelector
  , beginFindString_withOptionsSelector
  , beginFindStrings_withOptionsSelector
  , findString_fromSelection_withOptionsSelector
  , cancelFindStringSelector
  , printOperationForPrintInfo_scalingMode_autoRotateSelector
  , selectionFromPage_atPoint_toPage_atPointSelector
  , selectionFromPage_atPoint_toPage_atPoint_withGranularitySelector
  , selectionFromPage_atCharacterIndex_toPage_atCharacterIndexSelector
  , documentURLSelector
  , documentRefSelector
  , documentAttributesSelector
  , setDocumentAttributesSelector
  , majorVersionSelector
  , minorVersionSelector
  , isEncryptedSelector
  , isLockedSelector
  , allowsPrintingSelector
  , allowsCopyingSelector
  , allowsDocumentChangesSelector
  , allowsDocumentAssemblySelector
  , allowsContentAccessibilitySelector
  , allowsCommentingSelector
  , allowsFormFieldEntrySelector
  , accessPermissionsSelector
  , permissionsStatusSelector
  , stringSelector
  , pageCountSelector
  , pageClassSelector
  , isFindingSelector
  , selectionForEntireDocumentSelector

  -- * Enum types
  , NSStringCompareOptions(NSStringCompareOptions)
  , pattern NSCaseInsensitiveSearch
  , pattern NSLiteralSearch
  , pattern NSBackwardsSearch
  , pattern NSAnchoredSearch
  , pattern NSNumericSearch
  , pattern NSDiacriticInsensitiveSearch
  , pattern NSWidthInsensitiveSearch
  , pattern NSForcedOrderingSearch
  , pattern NSRegularExpressionSearch
  , PDFAccessPermissions(PDFAccessPermissions)
  , pattern PDFAllowsLowQualityPrinting
  , pattern PDFAllowsHighQualityPrinting
  , pattern PDFAllowsDocumentChanges
  , pattern PDFAllowsDocumentAssembly
  , pattern PDFAllowsContentCopying
  , pattern PDFAllowsContentAccessibility
  , pattern PDFAllowsCommenting
  , pattern PDFAllowsFormFieldEntry
  , PDFDocumentPermissions(PDFDocumentPermissions)
  , pattern KPDFDocumentPermissionsNone
  , pattern KPDFDocumentPermissionsUser
  , pattern KPDFDocumentPermissionsOwner
  , PDFPrintScalingMode(PDFPrintScalingMode)
  , pattern KPDFPrintPageScaleNone
  , pattern KPDFPrintPageScaleToFit
  , pattern KPDFPrintPageScaleDownToFit
  , PDFSelectionGranularity(PDFSelectionGranularity)
  , pattern PDFSelectionGranularityCharacter
  , pattern PDFSelectionGranularityWord
  , pattern PDFSelectionGranularityLine

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

import ObjC.PDFKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.PDFKit.Internal.Enums
import ObjC.Foundation.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPDFDocument pdfDocument => pdfDocument -> IO (Id PDFDocument)
init_ pdfDocument  =
  sendMsg pdfDocument (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithURL:@
initWithURL :: (IsPDFDocument pdfDocument, IsNSURL url) => pdfDocument -> url -> IO (Id PDFDocument)
initWithURL pdfDocument  url =
withObjCPtr url $ \raw_url ->
    sendMsg pdfDocument (mkSelector "initWithURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithData:@
initWithData :: (IsPDFDocument pdfDocument, IsNSData data_) => pdfDocument -> data_ -> IO (Id PDFDocument)
initWithData pdfDocument  data_ =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg pdfDocument (mkSelector "initWithData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- unlockWithPassword:@
unlockWithPassword :: (IsPDFDocument pdfDocument, IsNSString password) => pdfDocument -> password -> IO Bool
unlockWithPassword pdfDocument  password =
withObjCPtr password $ \raw_password ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfDocument (mkSelector "unlockWithPassword:") retCULong [argPtr (castPtr raw_password :: Ptr ())]

-- | @- dataRepresentation@
dataRepresentation :: IsPDFDocument pdfDocument => pdfDocument -> IO (Id NSData)
dataRepresentation pdfDocument  =
  sendMsg pdfDocument (mkSelector "dataRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- dataRepresentationWithOptions:@
dataRepresentationWithOptions :: (IsPDFDocument pdfDocument, IsNSDictionary options) => pdfDocument -> options -> IO (Id NSData)
dataRepresentationWithOptions pdfDocument  options =
withObjCPtr options $ \raw_options ->
    sendMsg pdfDocument (mkSelector "dataRepresentationWithOptions:") (retPtr retVoid) [argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeToFile:@
writeToFile :: (IsPDFDocument pdfDocument, IsNSString path) => pdfDocument -> path -> IO Bool
writeToFile pdfDocument  path =
withObjCPtr path $ \raw_path ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfDocument (mkSelector "writeToFile:") retCULong [argPtr (castPtr raw_path :: Ptr ())]

-- | @- writeToFile:withOptions:@
writeToFile_withOptions :: (IsPDFDocument pdfDocument, IsNSString path, IsNSDictionary options) => pdfDocument -> path -> options -> IO Bool
writeToFile_withOptions pdfDocument  path options =
withObjCPtr path $ \raw_path ->
  withObjCPtr options $ \raw_options ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfDocument (mkSelector "writeToFile:withOptions:") retCULong [argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())]

-- | @- writeToURL:@
writeToURL :: (IsPDFDocument pdfDocument, IsNSURL url) => pdfDocument -> url -> IO Bool
writeToURL pdfDocument  url =
withObjCPtr url $ \raw_url ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfDocument (mkSelector "writeToURL:") retCULong [argPtr (castPtr raw_url :: Ptr ())]

-- | @- writeToURL:withOptions:@
writeToURL_withOptions :: (IsPDFDocument pdfDocument, IsNSURL url, IsNSDictionary options) => pdfDocument -> url -> options -> IO Bool
writeToURL_withOptions pdfDocument  url options =
withObjCPtr url $ \raw_url ->
  withObjCPtr options $ \raw_options ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfDocument (mkSelector "writeToURL:withOptions:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())]

-- | @- outlineItemForSelection:@
outlineItemForSelection :: (IsPDFDocument pdfDocument, IsPDFSelection selection) => pdfDocument -> selection -> IO (Id PDFOutline)
outlineItemForSelection pdfDocument  selection =
withObjCPtr selection $ \raw_selection ->
    sendMsg pdfDocument (mkSelector "outlineItemForSelection:") (retPtr retVoid) [argPtr (castPtr raw_selection :: Ptr ())] >>= retainedObject . castPtr

-- | @- pageAtIndex:@
pageAtIndex :: IsPDFDocument pdfDocument => pdfDocument -> CULong -> IO (Id PDFPage)
pageAtIndex pdfDocument  index =
  sendMsg pdfDocument (mkSelector "pageAtIndex:") (retPtr retVoid) [argCULong (fromIntegral index)] >>= retainedObject . castPtr

-- | @- indexForPage:@
indexForPage :: (IsPDFDocument pdfDocument, IsPDFPage page) => pdfDocument -> page -> IO CULong
indexForPage pdfDocument  page =
withObjCPtr page $ \raw_page ->
    sendMsg pdfDocument (mkSelector "indexForPage:") retCULong [argPtr (castPtr raw_page :: Ptr ())]

-- | @- insertPage:atIndex:@
insertPage_atIndex :: (IsPDFDocument pdfDocument, IsPDFPage page) => pdfDocument -> page -> CULong -> IO ()
insertPage_atIndex pdfDocument  page index =
withObjCPtr page $ \raw_page ->
    sendMsg pdfDocument (mkSelector "insertPage:atIndex:") retVoid [argPtr (castPtr raw_page :: Ptr ()), argCULong (fromIntegral index)]

-- | @- removePageAtIndex:@
removePageAtIndex :: IsPDFDocument pdfDocument => pdfDocument -> CULong -> IO ()
removePageAtIndex pdfDocument  index =
  sendMsg pdfDocument (mkSelector "removePageAtIndex:") retVoid [argCULong (fromIntegral index)]

-- | @- exchangePageAtIndex:withPageAtIndex:@
exchangePageAtIndex_withPageAtIndex :: IsPDFDocument pdfDocument => pdfDocument -> CULong -> CULong -> IO ()
exchangePageAtIndex_withPageAtIndex pdfDocument  indexA indexB =
  sendMsg pdfDocument (mkSelector "exchangePageAtIndex:withPageAtIndex:") retVoid [argCULong (fromIntegral indexA), argCULong (fromIntegral indexB)]

-- | @- findString:withOptions:@
findString_withOptions :: (IsPDFDocument pdfDocument, IsNSString string) => pdfDocument -> string -> NSStringCompareOptions -> IO (Id NSArray)
findString_withOptions pdfDocument  string options =
withObjCPtr string $ \raw_string ->
    sendMsg pdfDocument (mkSelector "findString:withOptions:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ()), argCULong (coerce options)] >>= retainedObject . castPtr

-- | @- beginFindString:withOptions:@
beginFindString_withOptions :: (IsPDFDocument pdfDocument, IsNSString string) => pdfDocument -> string -> NSStringCompareOptions -> IO ()
beginFindString_withOptions pdfDocument  string options =
withObjCPtr string $ \raw_string ->
    sendMsg pdfDocument (mkSelector "beginFindString:withOptions:") retVoid [argPtr (castPtr raw_string :: Ptr ()), argCULong (coerce options)]

-- | @- beginFindStrings:withOptions:@
beginFindStrings_withOptions :: (IsPDFDocument pdfDocument, IsNSArray strings) => pdfDocument -> strings -> NSStringCompareOptions -> IO ()
beginFindStrings_withOptions pdfDocument  strings options =
withObjCPtr strings $ \raw_strings ->
    sendMsg pdfDocument (mkSelector "beginFindStrings:withOptions:") retVoid [argPtr (castPtr raw_strings :: Ptr ()), argCULong (coerce options)]

-- | @- findString:fromSelection:withOptions:@
findString_fromSelection_withOptions :: (IsPDFDocument pdfDocument, IsNSString string, IsPDFSelection selection) => pdfDocument -> string -> selection -> NSStringCompareOptions -> IO (Id PDFSelection)
findString_fromSelection_withOptions pdfDocument  string selection options =
withObjCPtr string $ \raw_string ->
  withObjCPtr selection $ \raw_selection ->
      sendMsg pdfDocument (mkSelector "findString:fromSelection:withOptions:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ()), argPtr (castPtr raw_selection :: Ptr ()), argCULong (coerce options)] >>= retainedObject . castPtr

-- | @- cancelFindString@
cancelFindString :: IsPDFDocument pdfDocument => pdfDocument -> IO ()
cancelFindString pdfDocument  =
  sendMsg pdfDocument (mkSelector "cancelFindString") retVoid []

-- | @- printOperationForPrintInfo:scalingMode:autoRotate:@
printOperationForPrintInfo_scalingMode_autoRotate :: (IsPDFDocument pdfDocument, IsNSPrintInfo printInfo) => pdfDocument -> printInfo -> PDFPrintScalingMode -> Bool -> IO (Id NSPrintOperation)
printOperationForPrintInfo_scalingMode_autoRotate pdfDocument  printInfo scaleMode doRotate =
withObjCPtr printInfo $ \raw_printInfo ->
    sendMsg pdfDocument (mkSelector "printOperationForPrintInfo:scalingMode:autoRotate:") (retPtr retVoid) [argPtr (castPtr raw_printInfo :: Ptr ()), argCLong (coerce scaleMode), argCULong (if doRotate then 1 else 0)] >>= retainedObject . castPtr

-- | @- selectionFromPage:atPoint:toPage:atPoint:@
selectionFromPage_atPoint_toPage_atPoint :: (IsPDFDocument pdfDocument, IsPDFPage startPage, IsPDFPage endPage) => pdfDocument -> startPage -> NSPoint -> endPage -> NSPoint -> IO (Id PDFSelection)
selectionFromPage_atPoint_toPage_atPoint pdfDocument  startPage startPoint endPage endPoint =
withObjCPtr startPage $ \raw_startPage ->
  withObjCPtr endPage $ \raw_endPage ->
      sendMsg pdfDocument (mkSelector "selectionFromPage:atPoint:toPage:atPoint:") (retPtr retVoid) [argPtr (castPtr raw_startPage :: Ptr ()), argNSPoint startPoint, argPtr (castPtr raw_endPage :: Ptr ()), argNSPoint endPoint] >>= retainedObject . castPtr

-- | @- selectionFromPage:atPoint:toPage:atPoint:withGranularity:@
selectionFromPage_atPoint_toPage_atPoint_withGranularity :: (IsPDFDocument pdfDocument, IsPDFPage startPage, IsPDFPage endPage) => pdfDocument -> startPage -> NSPoint -> endPage -> NSPoint -> PDFSelectionGranularity -> IO (Id PDFSelection)
selectionFromPage_atPoint_toPage_atPoint_withGranularity pdfDocument  startPage startPoint endPage endPoint granularity =
withObjCPtr startPage $ \raw_startPage ->
  withObjCPtr endPage $ \raw_endPage ->
      sendMsg pdfDocument (mkSelector "selectionFromPage:atPoint:toPage:atPoint:withGranularity:") (retPtr retVoid) [argPtr (castPtr raw_startPage :: Ptr ()), argNSPoint startPoint, argPtr (castPtr raw_endPage :: Ptr ()), argNSPoint endPoint, argCULong (coerce granularity)] >>= retainedObject . castPtr

-- | @- selectionFromPage:atCharacterIndex:toPage:atCharacterIndex:@
selectionFromPage_atCharacterIndex_toPage_atCharacterIndex :: (IsPDFDocument pdfDocument, IsPDFPage startPage, IsPDFPage endPage) => pdfDocument -> startPage -> CULong -> endPage -> CULong -> IO (Id PDFSelection)
selectionFromPage_atCharacterIndex_toPage_atCharacterIndex pdfDocument  startPage startCharacter endPage endCharacter =
withObjCPtr startPage $ \raw_startPage ->
  withObjCPtr endPage $ \raw_endPage ->
      sendMsg pdfDocument (mkSelector "selectionFromPage:atCharacterIndex:toPage:atCharacterIndex:") (retPtr retVoid) [argPtr (castPtr raw_startPage :: Ptr ()), argCULong (fromIntegral startCharacter), argPtr (castPtr raw_endPage :: Ptr ()), argCULong (fromIntegral endCharacter)] >>= retainedObject . castPtr

-- | @- documentURL@
documentURL :: IsPDFDocument pdfDocument => pdfDocument -> IO (Id NSURL)
documentURL pdfDocument  =
  sendMsg pdfDocument (mkSelector "documentURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- documentRef@
documentRef :: IsPDFDocument pdfDocument => pdfDocument -> IO (Ptr ())
documentRef pdfDocument  =
  fmap castPtr $ sendMsg pdfDocument (mkSelector "documentRef") (retPtr retVoid) []

-- | @- documentAttributes@
documentAttributes :: IsPDFDocument pdfDocument => pdfDocument -> IO (Id NSDictionary)
documentAttributes pdfDocument  =
  sendMsg pdfDocument (mkSelector "documentAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDocumentAttributes:@
setDocumentAttributes :: (IsPDFDocument pdfDocument, IsNSDictionary value) => pdfDocument -> value -> IO ()
setDocumentAttributes pdfDocument  value =
withObjCPtr value $ \raw_value ->
    sendMsg pdfDocument (mkSelector "setDocumentAttributes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- majorVersion@
majorVersion :: IsPDFDocument pdfDocument => pdfDocument -> IO CLong
majorVersion pdfDocument  =
  sendMsg pdfDocument (mkSelector "majorVersion") retCLong []

-- | @- minorVersion@
minorVersion :: IsPDFDocument pdfDocument => pdfDocument -> IO CLong
minorVersion pdfDocument  =
  sendMsg pdfDocument (mkSelector "minorVersion") retCLong []

-- | @- isEncrypted@
isEncrypted :: IsPDFDocument pdfDocument => pdfDocument -> IO Bool
isEncrypted pdfDocument  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfDocument (mkSelector "isEncrypted") retCULong []

-- | @- isLocked@
isLocked :: IsPDFDocument pdfDocument => pdfDocument -> IO Bool
isLocked pdfDocument  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfDocument (mkSelector "isLocked") retCULong []

-- | @- allowsPrinting@
allowsPrinting :: IsPDFDocument pdfDocument => pdfDocument -> IO Bool
allowsPrinting pdfDocument  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfDocument (mkSelector "allowsPrinting") retCULong []

-- | @- allowsCopying@
allowsCopying :: IsPDFDocument pdfDocument => pdfDocument -> IO Bool
allowsCopying pdfDocument  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfDocument (mkSelector "allowsCopying") retCULong []

-- | @- allowsDocumentChanges@
allowsDocumentChanges :: IsPDFDocument pdfDocument => pdfDocument -> IO Bool
allowsDocumentChanges pdfDocument  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfDocument (mkSelector "allowsDocumentChanges") retCULong []

-- | @- allowsDocumentAssembly@
allowsDocumentAssembly :: IsPDFDocument pdfDocument => pdfDocument -> IO Bool
allowsDocumentAssembly pdfDocument  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfDocument (mkSelector "allowsDocumentAssembly") retCULong []

-- | @- allowsContentAccessibility@
allowsContentAccessibility :: IsPDFDocument pdfDocument => pdfDocument -> IO Bool
allowsContentAccessibility pdfDocument  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfDocument (mkSelector "allowsContentAccessibility") retCULong []

-- | @- allowsCommenting@
allowsCommenting :: IsPDFDocument pdfDocument => pdfDocument -> IO Bool
allowsCommenting pdfDocument  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfDocument (mkSelector "allowsCommenting") retCULong []

-- | @- allowsFormFieldEntry@
allowsFormFieldEntry :: IsPDFDocument pdfDocument => pdfDocument -> IO Bool
allowsFormFieldEntry pdfDocument  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfDocument (mkSelector "allowsFormFieldEntry") retCULong []

-- | @- accessPermissions@
accessPermissions :: IsPDFDocument pdfDocument => pdfDocument -> IO PDFAccessPermissions
accessPermissions pdfDocument  =
  fmap (coerce :: CULong -> PDFAccessPermissions) $ sendMsg pdfDocument (mkSelector "accessPermissions") retCULong []

-- | @- permissionsStatus@
permissionsStatus :: IsPDFDocument pdfDocument => pdfDocument -> IO PDFDocumentPermissions
permissionsStatus pdfDocument  =
  fmap (coerce :: CLong -> PDFDocumentPermissions) $ sendMsg pdfDocument (mkSelector "permissionsStatus") retCLong []

-- | @- string@
string :: IsPDFDocument pdfDocument => pdfDocument -> IO (Id NSString)
string pdfDocument  =
  sendMsg pdfDocument (mkSelector "string") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- pageCount@
pageCount :: IsPDFDocument pdfDocument => pdfDocument -> IO CULong
pageCount pdfDocument  =
  sendMsg pdfDocument (mkSelector "pageCount") retCULong []

-- | @- pageClass@
pageClass :: IsPDFDocument pdfDocument => pdfDocument -> IO Class
pageClass pdfDocument  =
  fmap (Class . castPtr) $ sendMsg pdfDocument (mkSelector "pageClass") (retPtr retVoid) []

-- | @- isFinding@
isFinding :: IsPDFDocument pdfDocument => pdfDocument -> IO Bool
isFinding pdfDocument  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfDocument (mkSelector "isFinding") retCULong []

-- | @- selectionForEntireDocument@
selectionForEntireDocument :: IsPDFDocument pdfDocument => pdfDocument -> IO (Id PDFSelection)
selectionForEntireDocument pdfDocument  =
  sendMsg pdfDocument (mkSelector "selectionForEntireDocument") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @unlockWithPassword:@
unlockWithPasswordSelector :: Selector
unlockWithPasswordSelector = mkSelector "unlockWithPassword:"

-- | @Selector@ for @dataRepresentation@
dataRepresentationSelector :: Selector
dataRepresentationSelector = mkSelector "dataRepresentation"

-- | @Selector@ for @dataRepresentationWithOptions:@
dataRepresentationWithOptionsSelector :: Selector
dataRepresentationWithOptionsSelector = mkSelector "dataRepresentationWithOptions:"

-- | @Selector@ for @writeToFile:@
writeToFileSelector :: Selector
writeToFileSelector = mkSelector "writeToFile:"

-- | @Selector@ for @writeToFile:withOptions:@
writeToFile_withOptionsSelector :: Selector
writeToFile_withOptionsSelector = mkSelector "writeToFile:withOptions:"

-- | @Selector@ for @writeToURL:@
writeToURLSelector :: Selector
writeToURLSelector = mkSelector "writeToURL:"

-- | @Selector@ for @writeToURL:withOptions:@
writeToURL_withOptionsSelector :: Selector
writeToURL_withOptionsSelector = mkSelector "writeToURL:withOptions:"

-- | @Selector@ for @outlineItemForSelection:@
outlineItemForSelectionSelector :: Selector
outlineItemForSelectionSelector = mkSelector "outlineItemForSelection:"

-- | @Selector@ for @pageAtIndex:@
pageAtIndexSelector :: Selector
pageAtIndexSelector = mkSelector "pageAtIndex:"

-- | @Selector@ for @indexForPage:@
indexForPageSelector :: Selector
indexForPageSelector = mkSelector "indexForPage:"

-- | @Selector@ for @insertPage:atIndex:@
insertPage_atIndexSelector :: Selector
insertPage_atIndexSelector = mkSelector "insertPage:atIndex:"

-- | @Selector@ for @removePageAtIndex:@
removePageAtIndexSelector :: Selector
removePageAtIndexSelector = mkSelector "removePageAtIndex:"

-- | @Selector@ for @exchangePageAtIndex:withPageAtIndex:@
exchangePageAtIndex_withPageAtIndexSelector :: Selector
exchangePageAtIndex_withPageAtIndexSelector = mkSelector "exchangePageAtIndex:withPageAtIndex:"

-- | @Selector@ for @findString:withOptions:@
findString_withOptionsSelector :: Selector
findString_withOptionsSelector = mkSelector "findString:withOptions:"

-- | @Selector@ for @beginFindString:withOptions:@
beginFindString_withOptionsSelector :: Selector
beginFindString_withOptionsSelector = mkSelector "beginFindString:withOptions:"

-- | @Selector@ for @beginFindStrings:withOptions:@
beginFindStrings_withOptionsSelector :: Selector
beginFindStrings_withOptionsSelector = mkSelector "beginFindStrings:withOptions:"

-- | @Selector@ for @findString:fromSelection:withOptions:@
findString_fromSelection_withOptionsSelector :: Selector
findString_fromSelection_withOptionsSelector = mkSelector "findString:fromSelection:withOptions:"

-- | @Selector@ for @cancelFindString@
cancelFindStringSelector :: Selector
cancelFindStringSelector = mkSelector "cancelFindString"

-- | @Selector@ for @printOperationForPrintInfo:scalingMode:autoRotate:@
printOperationForPrintInfo_scalingMode_autoRotateSelector :: Selector
printOperationForPrintInfo_scalingMode_autoRotateSelector = mkSelector "printOperationForPrintInfo:scalingMode:autoRotate:"

-- | @Selector@ for @selectionFromPage:atPoint:toPage:atPoint:@
selectionFromPage_atPoint_toPage_atPointSelector :: Selector
selectionFromPage_atPoint_toPage_atPointSelector = mkSelector "selectionFromPage:atPoint:toPage:atPoint:"

-- | @Selector@ for @selectionFromPage:atPoint:toPage:atPoint:withGranularity:@
selectionFromPage_atPoint_toPage_atPoint_withGranularitySelector :: Selector
selectionFromPage_atPoint_toPage_atPoint_withGranularitySelector = mkSelector "selectionFromPage:atPoint:toPage:atPoint:withGranularity:"

-- | @Selector@ for @selectionFromPage:atCharacterIndex:toPage:atCharacterIndex:@
selectionFromPage_atCharacterIndex_toPage_atCharacterIndexSelector :: Selector
selectionFromPage_atCharacterIndex_toPage_atCharacterIndexSelector = mkSelector "selectionFromPage:atCharacterIndex:toPage:atCharacterIndex:"

-- | @Selector@ for @documentURL@
documentURLSelector :: Selector
documentURLSelector = mkSelector "documentURL"

-- | @Selector@ for @documentRef@
documentRefSelector :: Selector
documentRefSelector = mkSelector "documentRef"

-- | @Selector@ for @documentAttributes@
documentAttributesSelector :: Selector
documentAttributesSelector = mkSelector "documentAttributes"

-- | @Selector@ for @setDocumentAttributes:@
setDocumentAttributesSelector :: Selector
setDocumentAttributesSelector = mkSelector "setDocumentAttributes:"

-- | @Selector@ for @majorVersion@
majorVersionSelector :: Selector
majorVersionSelector = mkSelector "majorVersion"

-- | @Selector@ for @minorVersion@
minorVersionSelector :: Selector
minorVersionSelector = mkSelector "minorVersion"

-- | @Selector@ for @isEncrypted@
isEncryptedSelector :: Selector
isEncryptedSelector = mkSelector "isEncrypted"

-- | @Selector@ for @isLocked@
isLockedSelector :: Selector
isLockedSelector = mkSelector "isLocked"

-- | @Selector@ for @allowsPrinting@
allowsPrintingSelector :: Selector
allowsPrintingSelector = mkSelector "allowsPrinting"

-- | @Selector@ for @allowsCopying@
allowsCopyingSelector :: Selector
allowsCopyingSelector = mkSelector "allowsCopying"

-- | @Selector@ for @allowsDocumentChanges@
allowsDocumentChangesSelector :: Selector
allowsDocumentChangesSelector = mkSelector "allowsDocumentChanges"

-- | @Selector@ for @allowsDocumentAssembly@
allowsDocumentAssemblySelector :: Selector
allowsDocumentAssemblySelector = mkSelector "allowsDocumentAssembly"

-- | @Selector@ for @allowsContentAccessibility@
allowsContentAccessibilitySelector :: Selector
allowsContentAccessibilitySelector = mkSelector "allowsContentAccessibility"

-- | @Selector@ for @allowsCommenting@
allowsCommentingSelector :: Selector
allowsCommentingSelector = mkSelector "allowsCommenting"

-- | @Selector@ for @allowsFormFieldEntry@
allowsFormFieldEntrySelector :: Selector
allowsFormFieldEntrySelector = mkSelector "allowsFormFieldEntry"

-- | @Selector@ for @accessPermissions@
accessPermissionsSelector :: Selector
accessPermissionsSelector = mkSelector "accessPermissions"

-- | @Selector@ for @permissionsStatus@
permissionsStatusSelector :: Selector
permissionsStatusSelector = mkSelector "permissionsStatus"

-- | @Selector@ for @string@
stringSelector :: Selector
stringSelector = mkSelector "string"

-- | @Selector@ for @pageCount@
pageCountSelector :: Selector
pageCountSelector = mkSelector "pageCount"

-- | @Selector@ for @pageClass@
pageClassSelector :: Selector
pageClassSelector = mkSelector "pageClass"

-- | @Selector@ for @isFinding@
isFindingSelector :: Selector
isFindingSelector = mkSelector "isFinding"

-- | @Selector@ for @selectionForEntireDocument@
selectionForEntireDocumentSelector :: Selector
selectionForEntireDocumentSelector = mkSelector "selectionForEntireDocument"

