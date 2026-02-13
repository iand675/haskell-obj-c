{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , delegate
  , setDelegate
  , outlineRoot
  , setOutlineRoot
  , pageCount
  , pageClass
  , isFinding
  , selectionForEntireDocument
  , accessPermissionsSelector
  , allowsCommentingSelector
  , allowsContentAccessibilitySelector
  , allowsCopyingSelector
  , allowsDocumentAssemblySelector
  , allowsDocumentChangesSelector
  , allowsFormFieldEntrySelector
  , allowsPrintingSelector
  , beginFindString_withOptionsSelector
  , beginFindStrings_withOptionsSelector
  , cancelFindStringSelector
  , dataRepresentationSelector
  , dataRepresentationWithOptionsSelector
  , delegateSelector
  , documentAttributesSelector
  , documentRefSelector
  , documentURLSelector
  , exchangePageAtIndex_withPageAtIndexSelector
  , findString_fromSelection_withOptionsSelector
  , findString_withOptionsSelector
  , indexForPageSelector
  , initSelector
  , initWithDataSelector
  , initWithURLSelector
  , insertPage_atIndexSelector
  , isEncryptedSelector
  , isFindingSelector
  , isLockedSelector
  , majorVersionSelector
  , minorVersionSelector
  , outlineItemForSelectionSelector
  , outlineRootSelector
  , pageAtIndexSelector
  , pageClassSelector
  , pageCountSelector
  , permissionsStatusSelector
  , printOperationForPrintInfo_scalingMode_autoRotateSelector
  , removePageAtIndexSelector
  , selectionForEntireDocumentSelector
  , selectionFromPage_atCharacterIndex_toPage_atCharacterIndexSelector
  , selectionFromPage_atPoint_toPage_atPointSelector
  , selectionFromPage_atPoint_toPage_atPoint_withGranularitySelector
  , setDelegateSelector
  , setDocumentAttributesSelector
  , setOutlineRootSelector
  , stringSelector
  , unlockWithPasswordSelector
  , writeToFileSelector
  , writeToFile_withOptionsSelector
  , writeToURLSelector
  , writeToURL_withOptionsSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
init_ pdfDocument =
  sendOwnedMessage pdfDocument initSelector

-- | @- initWithURL:@
initWithURL :: (IsPDFDocument pdfDocument, IsNSURL url) => pdfDocument -> url -> IO (Id PDFDocument)
initWithURL pdfDocument url =
  sendOwnedMessage pdfDocument initWithURLSelector (toNSURL url)

-- | @- initWithData:@
initWithData :: (IsPDFDocument pdfDocument, IsNSData data_) => pdfDocument -> data_ -> IO (Id PDFDocument)
initWithData pdfDocument data_ =
  sendOwnedMessage pdfDocument initWithDataSelector (toNSData data_)

-- | @- unlockWithPassword:@
unlockWithPassword :: (IsPDFDocument pdfDocument, IsNSString password) => pdfDocument -> password -> IO Bool
unlockWithPassword pdfDocument password =
  sendMessage pdfDocument unlockWithPasswordSelector (toNSString password)

-- | @- dataRepresentation@
dataRepresentation :: IsPDFDocument pdfDocument => pdfDocument -> IO (Id NSData)
dataRepresentation pdfDocument =
  sendMessage pdfDocument dataRepresentationSelector

-- | @- dataRepresentationWithOptions:@
dataRepresentationWithOptions :: (IsPDFDocument pdfDocument, IsNSDictionary options) => pdfDocument -> options -> IO (Id NSData)
dataRepresentationWithOptions pdfDocument options =
  sendMessage pdfDocument dataRepresentationWithOptionsSelector (toNSDictionary options)

-- | @- writeToFile:@
writeToFile :: (IsPDFDocument pdfDocument, IsNSString path) => pdfDocument -> path -> IO Bool
writeToFile pdfDocument path =
  sendMessage pdfDocument writeToFileSelector (toNSString path)

-- | @- writeToFile:withOptions:@
writeToFile_withOptions :: (IsPDFDocument pdfDocument, IsNSString path, IsNSDictionary options) => pdfDocument -> path -> options -> IO Bool
writeToFile_withOptions pdfDocument path options =
  sendMessage pdfDocument writeToFile_withOptionsSelector (toNSString path) (toNSDictionary options)

-- | @- writeToURL:@
writeToURL :: (IsPDFDocument pdfDocument, IsNSURL url) => pdfDocument -> url -> IO Bool
writeToURL pdfDocument url =
  sendMessage pdfDocument writeToURLSelector (toNSURL url)

-- | @- writeToURL:withOptions:@
writeToURL_withOptions :: (IsPDFDocument pdfDocument, IsNSURL url, IsNSDictionary options) => pdfDocument -> url -> options -> IO Bool
writeToURL_withOptions pdfDocument url options =
  sendMessage pdfDocument writeToURL_withOptionsSelector (toNSURL url) (toNSDictionary options)

-- | @- outlineItemForSelection:@
outlineItemForSelection :: (IsPDFDocument pdfDocument, IsPDFSelection selection) => pdfDocument -> selection -> IO (Id PDFOutline)
outlineItemForSelection pdfDocument selection =
  sendMessage pdfDocument outlineItemForSelectionSelector (toPDFSelection selection)

-- | @- pageAtIndex:@
pageAtIndex :: IsPDFDocument pdfDocument => pdfDocument -> CULong -> IO (Id PDFPage)
pageAtIndex pdfDocument index =
  sendMessage pdfDocument pageAtIndexSelector index

-- | @- indexForPage:@
indexForPage :: (IsPDFDocument pdfDocument, IsPDFPage page) => pdfDocument -> page -> IO CULong
indexForPage pdfDocument page =
  sendMessage pdfDocument indexForPageSelector (toPDFPage page)

-- | @- insertPage:atIndex:@
insertPage_atIndex :: (IsPDFDocument pdfDocument, IsPDFPage page) => pdfDocument -> page -> CULong -> IO ()
insertPage_atIndex pdfDocument page index =
  sendMessage pdfDocument insertPage_atIndexSelector (toPDFPage page) index

-- | @- removePageAtIndex:@
removePageAtIndex :: IsPDFDocument pdfDocument => pdfDocument -> CULong -> IO ()
removePageAtIndex pdfDocument index =
  sendMessage pdfDocument removePageAtIndexSelector index

-- | @- exchangePageAtIndex:withPageAtIndex:@
exchangePageAtIndex_withPageAtIndex :: IsPDFDocument pdfDocument => pdfDocument -> CULong -> CULong -> IO ()
exchangePageAtIndex_withPageAtIndex pdfDocument indexA indexB =
  sendMessage pdfDocument exchangePageAtIndex_withPageAtIndexSelector indexA indexB

-- | @- findString:withOptions:@
findString_withOptions :: (IsPDFDocument pdfDocument, IsNSString string) => pdfDocument -> string -> NSStringCompareOptions -> IO (Id NSArray)
findString_withOptions pdfDocument string options =
  sendMessage pdfDocument findString_withOptionsSelector (toNSString string) options

-- | @- beginFindString:withOptions:@
beginFindString_withOptions :: (IsPDFDocument pdfDocument, IsNSString string) => pdfDocument -> string -> NSStringCompareOptions -> IO ()
beginFindString_withOptions pdfDocument string options =
  sendMessage pdfDocument beginFindString_withOptionsSelector (toNSString string) options

-- | @- beginFindStrings:withOptions:@
beginFindStrings_withOptions :: (IsPDFDocument pdfDocument, IsNSArray strings) => pdfDocument -> strings -> NSStringCompareOptions -> IO ()
beginFindStrings_withOptions pdfDocument strings options =
  sendMessage pdfDocument beginFindStrings_withOptionsSelector (toNSArray strings) options

-- | @- findString:fromSelection:withOptions:@
findString_fromSelection_withOptions :: (IsPDFDocument pdfDocument, IsNSString string, IsPDFSelection selection) => pdfDocument -> string -> selection -> NSStringCompareOptions -> IO (Id PDFSelection)
findString_fromSelection_withOptions pdfDocument string selection options =
  sendMessage pdfDocument findString_fromSelection_withOptionsSelector (toNSString string) (toPDFSelection selection) options

-- | @- cancelFindString@
cancelFindString :: IsPDFDocument pdfDocument => pdfDocument -> IO ()
cancelFindString pdfDocument =
  sendMessage pdfDocument cancelFindStringSelector

-- | @- printOperationForPrintInfo:scalingMode:autoRotate:@
printOperationForPrintInfo_scalingMode_autoRotate :: (IsPDFDocument pdfDocument, IsNSPrintInfo printInfo) => pdfDocument -> printInfo -> PDFPrintScalingMode -> Bool -> IO (Id NSPrintOperation)
printOperationForPrintInfo_scalingMode_autoRotate pdfDocument printInfo scaleMode doRotate =
  sendMessage pdfDocument printOperationForPrintInfo_scalingMode_autoRotateSelector (toNSPrintInfo printInfo) scaleMode doRotate

-- | @- selectionFromPage:atPoint:toPage:atPoint:@
selectionFromPage_atPoint_toPage_atPoint :: (IsPDFDocument pdfDocument, IsPDFPage startPage, IsPDFPage endPage) => pdfDocument -> startPage -> NSPoint -> endPage -> NSPoint -> IO (Id PDFSelection)
selectionFromPage_atPoint_toPage_atPoint pdfDocument startPage startPoint endPage endPoint =
  sendMessage pdfDocument selectionFromPage_atPoint_toPage_atPointSelector (toPDFPage startPage) startPoint (toPDFPage endPage) endPoint

-- | @- selectionFromPage:atPoint:toPage:atPoint:withGranularity:@
selectionFromPage_atPoint_toPage_atPoint_withGranularity :: (IsPDFDocument pdfDocument, IsPDFPage startPage, IsPDFPage endPage) => pdfDocument -> startPage -> NSPoint -> endPage -> NSPoint -> PDFSelectionGranularity -> IO (Id PDFSelection)
selectionFromPage_atPoint_toPage_atPoint_withGranularity pdfDocument startPage startPoint endPage endPoint granularity =
  sendMessage pdfDocument selectionFromPage_atPoint_toPage_atPoint_withGranularitySelector (toPDFPage startPage) startPoint (toPDFPage endPage) endPoint granularity

-- | @- selectionFromPage:atCharacterIndex:toPage:atCharacterIndex:@
selectionFromPage_atCharacterIndex_toPage_atCharacterIndex :: (IsPDFDocument pdfDocument, IsPDFPage startPage, IsPDFPage endPage) => pdfDocument -> startPage -> CULong -> endPage -> CULong -> IO (Id PDFSelection)
selectionFromPage_atCharacterIndex_toPage_atCharacterIndex pdfDocument startPage startCharacter endPage endCharacter =
  sendMessage pdfDocument selectionFromPage_atCharacterIndex_toPage_atCharacterIndexSelector (toPDFPage startPage) startCharacter (toPDFPage endPage) endCharacter

-- | @- documentURL@
documentURL :: IsPDFDocument pdfDocument => pdfDocument -> IO (Id NSURL)
documentURL pdfDocument =
  sendMessage pdfDocument documentURLSelector

-- | @- documentRef@
documentRef :: IsPDFDocument pdfDocument => pdfDocument -> IO (Ptr ())
documentRef pdfDocument =
  sendMessage pdfDocument documentRefSelector

-- | @- documentAttributes@
documentAttributes :: IsPDFDocument pdfDocument => pdfDocument -> IO (Id NSDictionary)
documentAttributes pdfDocument =
  sendMessage pdfDocument documentAttributesSelector

-- | @- setDocumentAttributes:@
setDocumentAttributes :: (IsPDFDocument pdfDocument, IsNSDictionary value) => pdfDocument -> value -> IO ()
setDocumentAttributes pdfDocument value =
  sendMessage pdfDocument setDocumentAttributesSelector (toNSDictionary value)

-- | @- majorVersion@
majorVersion :: IsPDFDocument pdfDocument => pdfDocument -> IO CLong
majorVersion pdfDocument =
  sendMessage pdfDocument majorVersionSelector

-- | @- minorVersion@
minorVersion :: IsPDFDocument pdfDocument => pdfDocument -> IO CLong
minorVersion pdfDocument =
  sendMessage pdfDocument minorVersionSelector

-- | @- isEncrypted@
isEncrypted :: IsPDFDocument pdfDocument => pdfDocument -> IO Bool
isEncrypted pdfDocument =
  sendMessage pdfDocument isEncryptedSelector

-- | @- isLocked@
isLocked :: IsPDFDocument pdfDocument => pdfDocument -> IO Bool
isLocked pdfDocument =
  sendMessage pdfDocument isLockedSelector

-- | @- allowsPrinting@
allowsPrinting :: IsPDFDocument pdfDocument => pdfDocument -> IO Bool
allowsPrinting pdfDocument =
  sendMessage pdfDocument allowsPrintingSelector

-- | @- allowsCopying@
allowsCopying :: IsPDFDocument pdfDocument => pdfDocument -> IO Bool
allowsCopying pdfDocument =
  sendMessage pdfDocument allowsCopyingSelector

-- | @- allowsDocumentChanges@
allowsDocumentChanges :: IsPDFDocument pdfDocument => pdfDocument -> IO Bool
allowsDocumentChanges pdfDocument =
  sendMessage pdfDocument allowsDocumentChangesSelector

-- | @- allowsDocumentAssembly@
allowsDocumentAssembly :: IsPDFDocument pdfDocument => pdfDocument -> IO Bool
allowsDocumentAssembly pdfDocument =
  sendMessage pdfDocument allowsDocumentAssemblySelector

-- | @- allowsContentAccessibility@
allowsContentAccessibility :: IsPDFDocument pdfDocument => pdfDocument -> IO Bool
allowsContentAccessibility pdfDocument =
  sendMessage pdfDocument allowsContentAccessibilitySelector

-- | @- allowsCommenting@
allowsCommenting :: IsPDFDocument pdfDocument => pdfDocument -> IO Bool
allowsCommenting pdfDocument =
  sendMessage pdfDocument allowsCommentingSelector

-- | @- allowsFormFieldEntry@
allowsFormFieldEntry :: IsPDFDocument pdfDocument => pdfDocument -> IO Bool
allowsFormFieldEntry pdfDocument =
  sendMessage pdfDocument allowsFormFieldEntrySelector

-- | @- accessPermissions@
accessPermissions :: IsPDFDocument pdfDocument => pdfDocument -> IO PDFAccessPermissions
accessPermissions pdfDocument =
  sendMessage pdfDocument accessPermissionsSelector

-- | @- permissionsStatus@
permissionsStatus :: IsPDFDocument pdfDocument => pdfDocument -> IO PDFDocumentPermissions
permissionsStatus pdfDocument =
  sendMessage pdfDocument permissionsStatusSelector

-- | @- string@
string :: IsPDFDocument pdfDocument => pdfDocument -> IO (Id NSString)
string pdfDocument =
  sendMessage pdfDocument stringSelector

-- | @- delegate@
delegate :: IsPDFDocument pdfDocument => pdfDocument -> IO RawId
delegate pdfDocument =
  sendMessage pdfDocument delegateSelector

-- | @- setDelegate:@
setDelegate :: IsPDFDocument pdfDocument => pdfDocument -> RawId -> IO ()
setDelegate pdfDocument value =
  sendMessage pdfDocument setDelegateSelector value

-- | @- outlineRoot@
outlineRoot :: IsPDFDocument pdfDocument => pdfDocument -> IO RawId
outlineRoot pdfDocument =
  sendMessage pdfDocument outlineRootSelector

-- | @- setOutlineRoot:@
setOutlineRoot :: IsPDFDocument pdfDocument => pdfDocument -> RawId -> IO ()
setOutlineRoot pdfDocument value =
  sendMessage pdfDocument setOutlineRootSelector value

-- | @- pageCount@
pageCount :: IsPDFDocument pdfDocument => pdfDocument -> IO CULong
pageCount pdfDocument =
  sendMessage pdfDocument pageCountSelector

-- | @- pageClass@
pageClass :: IsPDFDocument pdfDocument => pdfDocument -> IO Class
pageClass pdfDocument =
  sendMessage pdfDocument pageClassSelector

-- | @- isFinding@
isFinding :: IsPDFDocument pdfDocument => pdfDocument -> IO Bool
isFinding pdfDocument =
  sendMessage pdfDocument isFindingSelector

-- | @- selectionForEntireDocument@
selectionForEntireDocument :: IsPDFDocument pdfDocument => pdfDocument -> IO (Id PDFSelection)
selectionForEntireDocument pdfDocument =
  sendMessage pdfDocument selectionForEntireDocumentSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PDFDocument)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector '[Id NSURL] (Id PDFDocument)
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector '[Id NSData] (Id PDFDocument)
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @unlockWithPassword:@
unlockWithPasswordSelector :: Selector '[Id NSString] Bool
unlockWithPasswordSelector = mkSelector "unlockWithPassword:"

-- | @Selector@ for @dataRepresentation@
dataRepresentationSelector :: Selector '[] (Id NSData)
dataRepresentationSelector = mkSelector "dataRepresentation"

-- | @Selector@ for @dataRepresentationWithOptions:@
dataRepresentationWithOptionsSelector :: Selector '[Id NSDictionary] (Id NSData)
dataRepresentationWithOptionsSelector = mkSelector "dataRepresentationWithOptions:"

-- | @Selector@ for @writeToFile:@
writeToFileSelector :: Selector '[Id NSString] Bool
writeToFileSelector = mkSelector "writeToFile:"

-- | @Selector@ for @writeToFile:withOptions:@
writeToFile_withOptionsSelector :: Selector '[Id NSString, Id NSDictionary] Bool
writeToFile_withOptionsSelector = mkSelector "writeToFile:withOptions:"

-- | @Selector@ for @writeToURL:@
writeToURLSelector :: Selector '[Id NSURL] Bool
writeToURLSelector = mkSelector "writeToURL:"

-- | @Selector@ for @writeToURL:withOptions:@
writeToURL_withOptionsSelector :: Selector '[Id NSURL, Id NSDictionary] Bool
writeToURL_withOptionsSelector = mkSelector "writeToURL:withOptions:"

-- | @Selector@ for @outlineItemForSelection:@
outlineItemForSelectionSelector :: Selector '[Id PDFSelection] (Id PDFOutline)
outlineItemForSelectionSelector = mkSelector "outlineItemForSelection:"

-- | @Selector@ for @pageAtIndex:@
pageAtIndexSelector :: Selector '[CULong] (Id PDFPage)
pageAtIndexSelector = mkSelector "pageAtIndex:"

-- | @Selector@ for @indexForPage:@
indexForPageSelector :: Selector '[Id PDFPage] CULong
indexForPageSelector = mkSelector "indexForPage:"

-- | @Selector@ for @insertPage:atIndex:@
insertPage_atIndexSelector :: Selector '[Id PDFPage, CULong] ()
insertPage_atIndexSelector = mkSelector "insertPage:atIndex:"

-- | @Selector@ for @removePageAtIndex:@
removePageAtIndexSelector :: Selector '[CULong] ()
removePageAtIndexSelector = mkSelector "removePageAtIndex:"

-- | @Selector@ for @exchangePageAtIndex:withPageAtIndex:@
exchangePageAtIndex_withPageAtIndexSelector :: Selector '[CULong, CULong] ()
exchangePageAtIndex_withPageAtIndexSelector = mkSelector "exchangePageAtIndex:withPageAtIndex:"

-- | @Selector@ for @findString:withOptions:@
findString_withOptionsSelector :: Selector '[Id NSString, NSStringCompareOptions] (Id NSArray)
findString_withOptionsSelector = mkSelector "findString:withOptions:"

-- | @Selector@ for @beginFindString:withOptions:@
beginFindString_withOptionsSelector :: Selector '[Id NSString, NSStringCompareOptions] ()
beginFindString_withOptionsSelector = mkSelector "beginFindString:withOptions:"

-- | @Selector@ for @beginFindStrings:withOptions:@
beginFindStrings_withOptionsSelector :: Selector '[Id NSArray, NSStringCompareOptions] ()
beginFindStrings_withOptionsSelector = mkSelector "beginFindStrings:withOptions:"

-- | @Selector@ for @findString:fromSelection:withOptions:@
findString_fromSelection_withOptionsSelector :: Selector '[Id NSString, Id PDFSelection, NSStringCompareOptions] (Id PDFSelection)
findString_fromSelection_withOptionsSelector = mkSelector "findString:fromSelection:withOptions:"

-- | @Selector@ for @cancelFindString@
cancelFindStringSelector :: Selector '[] ()
cancelFindStringSelector = mkSelector "cancelFindString"

-- | @Selector@ for @printOperationForPrintInfo:scalingMode:autoRotate:@
printOperationForPrintInfo_scalingMode_autoRotateSelector :: Selector '[Id NSPrintInfo, PDFPrintScalingMode, Bool] (Id NSPrintOperation)
printOperationForPrintInfo_scalingMode_autoRotateSelector = mkSelector "printOperationForPrintInfo:scalingMode:autoRotate:"

-- | @Selector@ for @selectionFromPage:atPoint:toPage:atPoint:@
selectionFromPage_atPoint_toPage_atPointSelector :: Selector '[Id PDFPage, NSPoint, Id PDFPage, NSPoint] (Id PDFSelection)
selectionFromPage_atPoint_toPage_atPointSelector = mkSelector "selectionFromPage:atPoint:toPage:atPoint:"

-- | @Selector@ for @selectionFromPage:atPoint:toPage:atPoint:withGranularity:@
selectionFromPage_atPoint_toPage_atPoint_withGranularitySelector :: Selector '[Id PDFPage, NSPoint, Id PDFPage, NSPoint, PDFSelectionGranularity] (Id PDFSelection)
selectionFromPage_atPoint_toPage_atPoint_withGranularitySelector = mkSelector "selectionFromPage:atPoint:toPage:atPoint:withGranularity:"

-- | @Selector@ for @selectionFromPage:atCharacterIndex:toPage:atCharacterIndex:@
selectionFromPage_atCharacterIndex_toPage_atCharacterIndexSelector :: Selector '[Id PDFPage, CULong, Id PDFPage, CULong] (Id PDFSelection)
selectionFromPage_atCharacterIndex_toPage_atCharacterIndexSelector = mkSelector "selectionFromPage:atCharacterIndex:toPage:atCharacterIndex:"

-- | @Selector@ for @documentURL@
documentURLSelector :: Selector '[] (Id NSURL)
documentURLSelector = mkSelector "documentURL"

-- | @Selector@ for @documentRef@
documentRefSelector :: Selector '[] (Ptr ())
documentRefSelector = mkSelector "documentRef"

-- | @Selector@ for @documentAttributes@
documentAttributesSelector :: Selector '[] (Id NSDictionary)
documentAttributesSelector = mkSelector "documentAttributes"

-- | @Selector@ for @setDocumentAttributes:@
setDocumentAttributesSelector :: Selector '[Id NSDictionary] ()
setDocumentAttributesSelector = mkSelector "setDocumentAttributes:"

-- | @Selector@ for @majorVersion@
majorVersionSelector :: Selector '[] CLong
majorVersionSelector = mkSelector "majorVersion"

-- | @Selector@ for @minorVersion@
minorVersionSelector :: Selector '[] CLong
minorVersionSelector = mkSelector "minorVersion"

-- | @Selector@ for @isEncrypted@
isEncryptedSelector :: Selector '[] Bool
isEncryptedSelector = mkSelector "isEncrypted"

-- | @Selector@ for @isLocked@
isLockedSelector :: Selector '[] Bool
isLockedSelector = mkSelector "isLocked"

-- | @Selector@ for @allowsPrinting@
allowsPrintingSelector :: Selector '[] Bool
allowsPrintingSelector = mkSelector "allowsPrinting"

-- | @Selector@ for @allowsCopying@
allowsCopyingSelector :: Selector '[] Bool
allowsCopyingSelector = mkSelector "allowsCopying"

-- | @Selector@ for @allowsDocumentChanges@
allowsDocumentChangesSelector :: Selector '[] Bool
allowsDocumentChangesSelector = mkSelector "allowsDocumentChanges"

-- | @Selector@ for @allowsDocumentAssembly@
allowsDocumentAssemblySelector :: Selector '[] Bool
allowsDocumentAssemblySelector = mkSelector "allowsDocumentAssembly"

-- | @Selector@ for @allowsContentAccessibility@
allowsContentAccessibilitySelector :: Selector '[] Bool
allowsContentAccessibilitySelector = mkSelector "allowsContentAccessibility"

-- | @Selector@ for @allowsCommenting@
allowsCommentingSelector :: Selector '[] Bool
allowsCommentingSelector = mkSelector "allowsCommenting"

-- | @Selector@ for @allowsFormFieldEntry@
allowsFormFieldEntrySelector :: Selector '[] Bool
allowsFormFieldEntrySelector = mkSelector "allowsFormFieldEntry"

-- | @Selector@ for @accessPermissions@
accessPermissionsSelector :: Selector '[] PDFAccessPermissions
accessPermissionsSelector = mkSelector "accessPermissions"

-- | @Selector@ for @permissionsStatus@
permissionsStatusSelector :: Selector '[] PDFDocumentPermissions
permissionsStatusSelector = mkSelector "permissionsStatus"

-- | @Selector@ for @string@
stringSelector :: Selector '[] (Id NSString)
stringSelector = mkSelector "string"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @outlineRoot@
outlineRootSelector :: Selector '[] RawId
outlineRootSelector = mkSelector "outlineRoot"

-- | @Selector@ for @setOutlineRoot:@
setOutlineRootSelector :: Selector '[RawId] ()
setOutlineRootSelector = mkSelector "setOutlineRoot:"

-- | @Selector@ for @pageCount@
pageCountSelector :: Selector '[] CULong
pageCountSelector = mkSelector "pageCount"

-- | @Selector@ for @pageClass@
pageClassSelector :: Selector '[] Class
pageClassSelector = mkSelector "pageClass"

-- | @Selector@ for @isFinding@
isFindingSelector :: Selector '[] Bool
isFindingSelector = mkSelector "isFinding"

-- | @Selector@ for @selectionForEntireDocument@
selectionForEntireDocumentSelector :: Selector '[] (Id PDFSelection)
selectionForEntireDocumentSelector = mkSelector "selectionForEntireDocument"

