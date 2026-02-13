{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPrinter@.
module ObjC.AppKit.NSPrinter
  ( NSPrinter
  , IsNSPrinter(..)
  , printerWithName
  , printerWithType
  , pageSizeForPaper
  , statusForTable
  , isKey_inTable
  , booleanForKey_inTable
  , floatForKey_inTable
  , intForKey_inTable
  , rectForKey_inTable
  , sizeForKey_inTable
  , stringForKey_inTable
  , stringListForKey_inTable
  , imageRectForPaper
  , acceptsBinary
  , isColor
  , isFontAvailable
  , isOutputStackInReverseOrder
  , printerWithName_domain_includeUnavailable
  , domain
  , host
  , note
  , printerNames
  , printerTypes
  , name
  , type_
  , languageLevel
  , deviceDescription
  , acceptsBinarySelector
  , booleanForKey_inTableSelector
  , deviceDescriptionSelector
  , domainSelector
  , floatForKey_inTableSelector
  , hostSelector
  , imageRectForPaperSelector
  , intForKey_inTableSelector
  , isColorSelector
  , isFontAvailableSelector
  , isKey_inTableSelector
  , isOutputStackInReverseOrderSelector
  , languageLevelSelector
  , nameSelector
  , noteSelector
  , pageSizeForPaperSelector
  , printerNamesSelector
  , printerTypesSelector
  , printerWithNameSelector
  , printerWithName_domain_includeUnavailableSelector
  , printerWithTypeSelector
  , rectForKey_inTableSelector
  , sizeForKey_inTableSelector
  , statusForTableSelector
  , stringForKey_inTableSelector
  , stringListForKey_inTableSelector
  , typeSelector

  -- * Enum types
  , NSPrinterTableStatus(NSPrinterTableStatus)
  , pattern NSPrinterTableOK
  , pattern NSPrinterTableNotFound
  , pattern NSPrinterTableError

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

-- | @+ printerWithName:@
printerWithName :: IsNSString name => name -> IO (Id NSPrinter)
printerWithName name =
  do
    cls' <- getRequiredClass "NSPrinter"
    sendClassMessage cls' printerWithNameSelector (toNSString name)

-- | @+ printerWithType:@
printerWithType :: IsNSString type_ => type_ -> IO (Id NSPrinter)
printerWithType type_ =
  do
    cls' <- getRequiredClass "NSPrinter"
    sendClassMessage cls' printerWithTypeSelector (toNSString type_)

-- | @- pageSizeForPaper:@
pageSizeForPaper :: (IsNSPrinter nsPrinter, IsNSString paperName) => nsPrinter -> paperName -> IO NSSize
pageSizeForPaper nsPrinter paperName =
  sendMessage nsPrinter pageSizeForPaperSelector (toNSString paperName)

-- | @- statusForTable:@
statusForTable :: (IsNSPrinter nsPrinter, IsNSString tableName) => nsPrinter -> tableName -> IO NSPrinterTableStatus
statusForTable nsPrinter tableName =
  sendMessage nsPrinter statusForTableSelector (toNSString tableName)

-- | @- isKey:inTable:@
isKey_inTable :: (IsNSPrinter nsPrinter, IsNSString key, IsNSString table) => nsPrinter -> key -> table -> IO Bool
isKey_inTable nsPrinter key table =
  sendMessage nsPrinter isKey_inTableSelector (toNSString key) (toNSString table)

-- | @- booleanForKey:inTable:@
booleanForKey_inTable :: (IsNSPrinter nsPrinter, IsNSString key, IsNSString table) => nsPrinter -> key -> table -> IO Bool
booleanForKey_inTable nsPrinter key table =
  sendMessage nsPrinter booleanForKey_inTableSelector (toNSString key) (toNSString table)

-- | @- floatForKey:inTable:@
floatForKey_inTable :: (IsNSPrinter nsPrinter, IsNSString key, IsNSString table) => nsPrinter -> key -> table -> IO CFloat
floatForKey_inTable nsPrinter key table =
  sendMessage nsPrinter floatForKey_inTableSelector (toNSString key) (toNSString table)

-- | @- intForKey:inTable:@
intForKey_inTable :: (IsNSPrinter nsPrinter, IsNSString key, IsNSString table) => nsPrinter -> key -> table -> IO CInt
intForKey_inTable nsPrinter key table =
  sendMessage nsPrinter intForKey_inTableSelector (toNSString key) (toNSString table)

-- | @- rectForKey:inTable:@
rectForKey_inTable :: (IsNSPrinter nsPrinter, IsNSString key, IsNSString table) => nsPrinter -> key -> table -> IO NSRect
rectForKey_inTable nsPrinter key table =
  sendMessage nsPrinter rectForKey_inTableSelector (toNSString key) (toNSString table)

-- | @- sizeForKey:inTable:@
sizeForKey_inTable :: (IsNSPrinter nsPrinter, IsNSString key, IsNSString table) => nsPrinter -> key -> table -> IO NSSize
sizeForKey_inTable nsPrinter key table =
  sendMessage nsPrinter sizeForKey_inTableSelector (toNSString key) (toNSString table)

-- | @- stringForKey:inTable:@
stringForKey_inTable :: (IsNSPrinter nsPrinter, IsNSString key, IsNSString table) => nsPrinter -> key -> table -> IO (Id NSString)
stringForKey_inTable nsPrinter key table =
  sendMessage nsPrinter stringForKey_inTableSelector (toNSString key) (toNSString table)

-- | @- stringListForKey:inTable:@
stringListForKey_inTable :: (IsNSPrinter nsPrinter, IsNSString key, IsNSString table) => nsPrinter -> key -> table -> IO (Id NSArray)
stringListForKey_inTable nsPrinter key table =
  sendMessage nsPrinter stringListForKey_inTableSelector (toNSString key) (toNSString table)

-- | @- imageRectForPaper:@
imageRectForPaper :: (IsNSPrinter nsPrinter, IsNSString paperName) => nsPrinter -> paperName -> IO NSRect
imageRectForPaper nsPrinter paperName =
  sendMessage nsPrinter imageRectForPaperSelector (toNSString paperName)

-- | @- acceptsBinary@
acceptsBinary :: IsNSPrinter nsPrinter => nsPrinter -> IO Bool
acceptsBinary nsPrinter =
  sendMessage nsPrinter acceptsBinarySelector

-- | @- isColor@
isColor :: IsNSPrinter nsPrinter => nsPrinter -> IO Bool
isColor nsPrinter =
  sendMessage nsPrinter isColorSelector

-- | @- isFontAvailable:@
isFontAvailable :: (IsNSPrinter nsPrinter, IsNSString faceName) => nsPrinter -> faceName -> IO Bool
isFontAvailable nsPrinter faceName =
  sendMessage nsPrinter isFontAvailableSelector (toNSString faceName)

-- | @- isOutputStackInReverseOrder@
isOutputStackInReverseOrder :: IsNSPrinter nsPrinter => nsPrinter -> IO Bool
isOutputStackInReverseOrder nsPrinter =
  sendMessage nsPrinter isOutputStackInReverseOrderSelector

-- | @+ printerWithName:domain:includeUnavailable:@
printerWithName_domain_includeUnavailable :: (IsNSString name, IsNSString domain) => name -> domain -> Bool -> IO (Id NSPrinter)
printerWithName_domain_includeUnavailable name domain flag =
  do
    cls' <- getRequiredClass "NSPrinter"
    sendClassMessage cls' printerWithName_domain_includeUnavailableSelector (toNSString name) (toNSString domain) flag

-- | @- domain@
domain :: IsNSPrinter nsPrinter => nsPrinter -> IO (Id NSString)
domain nsPrinter =
  sendMessage nsPrinter domainSelector

-- | @- host@
host :: IsNSPrinter nsPrinter => nsPrinter -> IO (Id NSString)
host nsPrinter =
  sendMessage nsPrinter hostSelector

-- | @- note@
note :: IsNSPrinter nsPrinter => nsPrinter -> IO (Id NSString)
note nsPrinter =
  sendMessage nsPrinter noteSelector

-- | @+ printerNames@
printerNames :: IO (Id NSArray)
printerNames  =
  do
    cls' <- getRequiredClass "NSPrinter"
    sendClassMessage cls' printerNamesSelector

-- | @+ printerTypes@
printerTypes :: IO (Id NSArray)
printerTypes  =
  do
    cls' <- getRequiredClass "NSPrinter"
    sendClassMessage cls' printerTypesSelector

-- | @- name@
name :: IsNSPrinter nsPrinter => nsPrinter -> IO (Id NSString)
name nsPrinter =
  sendMessage nsPrinter nameSelector

-- | @- type@
type_ :: IsNSPrinter nsPrinter => nsPrinter -> IO (Id NSString)
type_ nsPrinter =
  sendMessage nsPrinter typeSelector

-- | @- languageLevel@
languageLevel :: IsNSPrinter nsPrinter => nsPrinter -> IO CLong
languageLevel nsPrinter =
  sendMessage nsPrinter languageLevelSelector

-- | @- deviceDescription@
deviceDescription :: IsNSPrinter nsPrinter => nsPrinter -> IO (Id NSDictionary)
deviceDescription nsPrinter =
  sendMessage nsPrinter deviceDescriptionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @printerWithName:@
printerWithNameSelector :: Selector '[Id NSString] (Id NSPrinter)
printerWithNameSelector = mkSelector "printerWithName:"

-- | @Selector@ for @printerWithType:@
printerWithTypeSelector :: Selector '[Id NSString] (Id NSPrinter)
printerWithTypeSelector = mkSelector "printerWithType:"

-- | @Selector@ for @pageSizeForPaper:@
pageSizeForPaperSelector :: Selector '[Id NSString] NSSize
pageSizeForPaperSelector = mkSelector "pageSizeForPaper:"

-- | @Selector@ for @statusForTable:@
statusForTableSelector :: Selector '[Id NSString] NSPrinterTableStatus
statusForTableSelector = mkSelector "statusForTable:"

-- | @Selector@ for @isKey:inTable:@
isKey_inTableSelector :: Selector '[Id NSString, Id NSString] Bool
isKey_inTableSelector = mkSelector "isKey:inTable:"

-- | @Selector@ for @booleanForKey:inTable:@
booleanForKey_inTableSelector :: Selector '[Id NSString, Id NSString] Bool
booleanForKey_inTableSelector = mkSelector "booleanForKey:inTable:"

-- | @Selector@ for @floatForKey:inTable:@
floatForKey_inTableSelector :: Selector '[Id NSString, Id NSString] CFloat
floatForKey_inTableSelector = mkSelector "floatForKey:inTable:"

-- | @Selector@ for @intForKey:inTable:@
intForKey_inTableSelector :: Selector '[Id NSString, Id NSString] CInt
intForKey_inTableSelector = mkSelector "intForKey:inTable:"

-- | @Selector@ for @rectForKey:inTable:@
rectForKey_inTableSelector :: Selector '[Id NSString, Id NSString] NSRect
rectForKey_inTableSelector = mkSelector "rectForKey:inTable:"

-- | @Selector@ for @sizeForKey:inTable:@
sizeForKey_inTableSelector :: Selector '[Id NSString, Id NSString] NSSize
sizeForKey_inTableSelector = mkSelector "sizeForKey:inTable:"

-- | @Selector@ for @stringForKey:inTable:@
stringForKey_inTableSelector :: Selector '[Id NSString, Id NSString] (Id NSString)
stringForKey_inTableSelector = mkSelector "stringForKey:inTable:"

-- | @Selector@ for @stringListForKey:inTable:@
stringListForKey_inTableSelector :: Selector '[Id NSString, Id NSString] (Id NSArray)
stringListForKey_inTableSelector = mkSelector "stringListForKey:inTable:"

-- | @Selector@ for @imageRectForPaper:@
imageRectForPaperSelector :: Selector '[Id NSString] NSRect
imageRectForPaperSelector = mkSelector "imageRectForPaper:"

-- | @Selector@ for @acceptsBinary@
acceptsBinarySelector :: Selector '[] Bool
acceptsBinarySelector = mkSelector "acceptsBinary"

-- | @Selector@ for @isColor@
isColorSelector :: Selector '[] Bool
isColorSelector = mkSelector "isColor"

-- | @Selector@ for @isFontAvailable:@
isFontAvailableSelector :: Selector '[Id NSString] Bool
isFontAvailableSelector = mkSelector "isFontAvailable:"

-- | @Selector@ for @isOutputStackInReverseOrder@
isOutputStackInReverseOrderSelector :: Selector '[] Bool
isOutputStackInReverseOrderSelector = mkSelector "isOutputStackInReverseOrder"

-- | @Selector@ for @printerWithName:domain:includeUnavailable:@
printerWithName_domain_includeUnavailableSelector :: Selector '[Id NSString, Id NSString, Bool] (Id NSPrinter)
printerWithName_domain_includeUnavailableSelector = mkSelector "printerWithName:domain:includeUnavailable:"

-- | @Selector@ for @domain@
domainSelector :: Selector '[] (Id NSString)
domainSelector = mkSelector "domain"

-- | @Selector@ for @host@
hostSelector :: Selector '[] (Id NSString)
hostSelector = mkSelector "host"

-- | @Selector@ for @note@
noteSelector :: Selector '[] (Id NSString)
noteSelector = mkSelector "note"

-- | @Selector@ for @printerNames@
printerNamesSelector :: Selector '[] (Id NSArray)
printerNamesSelector = mkSelector "printerNames"

-- | @Selector@ for @printerTypes@
printerTypesSelector :: Selector '[] (Id NSArray)
printerTypesSelector = mkSelector "printerTypes"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSString)
typeSelector = mkSelector "type"

-- | @Selector@ for @languageLevel@
languageLevelSelector :: Selector '[] CLong
languageLevelSelector = mkSelector "languageLevel"

-- | @Selector@ for @deviceDescription@
deviceDescriptionSelector :: Selector '[] (Id NSDictionary)
deviceDescriptionSelector = mkSelector "deviceDescription"

