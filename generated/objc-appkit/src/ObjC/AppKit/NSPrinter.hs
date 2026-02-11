{-# LANGUAGE PatternSynonyms #-}
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
  , printerWithNameSelector
  , printerWithTypeSelector
  , pageSizeForPaperSelector
  , statusForTableSelector
  , isKey_inTableSelector
  , booleanForKey_inTableSelector
  , floatForKey_inTableSelector
  , intForKey_inTableSelector
  , rectForKey_inTableSelector
  , sizeForKey_inTableSelector
  , stringForKey_inTableSelector
  , stringListForKey_inTableSelector
  , imageRectForPaperSelector
  , acceptsBinarySelector
  , isColorSelector
  , isFontAvailableSelector
  , isOutputStackInReverseOrderSelector
  , printerWithName_domain_includeUnavailableSelector
  , domainSelector
  , hostSelector
  , noteSelector
  , printerNamesSelector
  , printerTypesSelector
  , nameSelector
  , typeSelector
  , languageLevelSelector
  , deviceDescriptionSelector

  -- * Enum types
  , NSPrinterTableStatus(NSPrinterTableStatus)
  , pattern NSPrinterTableOK
  , pattern NSPrinterTableNotFound
  , pattern NSPrinterTableError

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

-- | @+ printerWithName:@
printerWithName :: IsNSString name => name -> IO (Id NSPrinter)
printerWithName name =
  do
    cls' <- getRequiredClass "NSPrinter"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "printerWithName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @+ printerWithType:@
printerWithType :: IsNSString type_ => type_ -> IO (Id NSPrinter)
printerWithType type_ =
  do
    cls' <- getRequiredClass "NSPrinter"
    withObjCPtr type_ $ \raw_type_ ->
      sendClassMsg cls' (mkSelector "printerWithType:") (retPtr retVoid) [argPtr (castPtr raw_type_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- pageSizeForPaper:@
pageSizeForPaper :: (IsNSPrinter nsPrinter, IsNSString paperName) => nsPrinter -> paperName -> IO NSSize
pageSizeForPaper nsPrinter  paperName =
withObjCPtr paperName $ \raw_paperName ->
    sendMsgStret nsPrinter (mkSelector "pageSizeForPaper:") retNSSize [argPtr (castPtr raw_paperName :: Ptr ())]

-- | @- statusForTable:@
statusForTable :: (IsNSPrinter nsPrinter, IsNSString tableName) => nsPrinter -> tableName -> IO NSPrinterTableStatus
statusForTable nsPrinter  tableName =
withObjCPtr tableName $ \raw_tableName ->
    fmap (coerce :: CULong -> NSPrinterTableStatus) $ sendMsg nsPrinter (mkSelector "statusForTable:") retCULong [argPtr (castPtr raw_tableName :: Ptr ())]

-- | @- isKey:inTable:@
isKey_inTable :: (IsNSPrinter nsPrinter, IsNSString key, IsNSString table) => nsPrinter -> key -> table -> IO Bool
isKey_inTable nsPrinter  key table =
withObjCPtr key $ \raw_key ->
  withObjCPtr table $ \raw_table ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPrinter (mkSelector "isKey:inTable:") retCULong [argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_table :: Ptr ())]

-- | @- booleanForKey:inTable:@
booleanForKey_inTable :: (IsNSPrinter nsPrinter, IsNSString key, IsNSString table) => nsPrinter -> key -> table -> IO Bool
booleanForKey_inTable nsPrinter  key table =
withObjCPtr key $ \raw_key ->
  withObjCPtr table $ \raw_table ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPrinter (mkSelector "booleanForKey:inTable:") retCULong [argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_table :: Ptr ())]

-- | @- floatForKey:inTable:@
floatForKey_inTable :: (IsNSPrinter nsPrinter, IsNSString key, IsNSString table) => nsPrinter -> key -> table -> IO CFloat
floatForKey_inTable nsPrinter  key table =
withObjCPtr key $ \raw_key ->
  withObjCPtr table $ \raw_table ->
      sendMsg nsPrinter (mkSelector "floatForKey:inTable:") retCFloat [argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_table :: Ptr ())]

-- | @- intForKey:inTable:@
intForKey_inTable :: (IsNSPrinter nsPrinter, IsNSString key, IsNSString table) => nsPrinter -> key -> table -> IO CInt
intForKey_inTable nsPrinter  key table =
withObjCPtr key $ \raw_key ->
  withObjCPtr table $ \raw_table ->
      sendMsg nsPrinter (mkSelector "intForKey:inTable:") retCInt [argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_table :: Ptr ())]

-- | @- rectForKey:inTable:@
rectForKey_inTable :: (IsNSPrinter nsPrinter, IsNSString key, IsNSString table) => nsPrinter -> key -> table -> IO NSRect
rectForKey_inTable nsPrinter  key table =
withObjCPtr key $ \raw_key ->
  withObjCPtr table $ \raw_table ->
      sendMsgStret nsPrinter (mkSelector "rectForKey:inTable:") retNSRect [argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_table :: Ptr ())]

-- | @- sizeForKey:inTable:@
sizeForKey_inTable :: (IsNSPrinter nsPrinter, IsNSString key, IsNSString table) => nsPrinter -> key -> table -> IO NSSize
sizeForKey_inTable nsPrinter  key table =
withObjCPtr key $ \raw_key ->
  withObjCPtr table $ \raw_table ->
      sendMsgStret nsPrinter (mkSelector "sizeForKey:inTable:") retNSSize [argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_table :: Ptr ())]

-- | @- stringForKey:inTable:@
stringForKey_inTable :: (IsNSPrinter nsPrinter, IsNSString key, IsNSString table) => nsPrinter -> key -> table -> IO (Id NSString)
stringForKey_inTable nsPrinter  key table =
withObjCPtr key $ \raw_key ->
  withObjCPtr table $ \raw_table ->
      sendMsg nsPrinter (mkSelector "stringForKey:inTable:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_table :: Ptr ())] >>= retainedObject . castPtr

-- | @- stringListForKey:inTable:@
stringListForKey_inTable :: (IsNSPrinter nsPrinter, IsNSString key, IsNSString table) => nsPrinter -> key -> table -> IO (Id NSArray)
stringListForKey_inTable nsPrinter  key table =
withObjCPtr key $ \raw_key ->
  withObjCPtr table $ \raw_table ->
      sendMsg nsPrinter (mkSelector "stringListForKey:inTable:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_table :: Ptr ())] >>= retainedObject . castPtr

-- | @- imageRectForPaper:@
imageRectForPaper :: (IsNSPrinter nsPrinter, IsNSString paperName) => nsPrinter -> paperName -> IO NSRect
imageRectForPaper nsPrinter  paperName =
withObjCPtr paperName $ \raw_paperName ->
    sendMsgStret nsPrinter (mkSelector "imageRectForPaper:") retNSRect [argPtr (castPtr raw_paperName :: Ptr ())]

-- | @- acceptsBinary@
acceptsBinary :: IsNSPrinter nsPrinter => nsPrinter -> IO Bool
acceptsBinary nsPrinter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPrinter (mkSelector "acceptsBinary") retCULong []

-- | @- isColor@
isColor :: IsNSPrinter nsPrinter => nsPrinter -> IO Bool
isColor nsPrinter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPrinter (mkSelector "isColor") retCULong []

-- | @- isFontAvailable:@
isFontAvailable :: (IsNSPrinter nsPrinter, IsNSString faceName) => nsPrinter -> faceName -> IO Bool
isFontAvailable nsPrinter  faceName =
withObjCPtr faceName $ \raw_faceName ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPrinter (mkSelector "isFontAvailable:") retCULong [argPtr (castPtr raw_faceName :: Ptr ())]

-- | @- isOutputStackInReverseOrder@
isOutputStackInReverseOrder :: IsNSPrinter nsPrinter => nsPrinter -> IO Bool
isOutputStackInReverseOrder nsPrinter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPrinter (mkSelector "isOutputStackInReverseOrder") retCULong []

-- | @+ printerWithName:domain:includeUnavailable:@
printerWithName_domain_includeUnavailable :: (IsNSString name, IsNSString domain) => name -> domain -> Bool -> IO (Id NSPrinter)
printerWithName_domain_includeUnavailable name domain flag =
  do
    cls' <- getRequiredClass "NSPrinter"
    withObjCPtr name $ \raw_name ->
      withObjCPtr domain $ \raw_domain ->
        sendClassMsg cls' (mkSelector "printerWithName:domain:includeUnavailable:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_domain :: Ptr ()), argCULong (if flag then 1 else 0)] >>= retainedObject . castPtr

-- | @- domain@
domain :: IsNSPrinter nsPrinter => nsPrinter -> IO (Id NSString)
domain nsPrinter  =
  sendMsg nsPrinter (mkSelector "domain") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- host@
host :: IsNSPrinter nsPrinter => nsPrinter -> IO (Id NSString)
host nsPrinter  =
  sendMsg nsPrinter (mkSelector "host") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- note@
note :: IsNSPrinter nsPrinter => nsPrinter -> IO (Id NSString)
note nsPrinter  =
  sendMsg nsPrinter (mkSelector "note") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ printerNames@
printerNames :: IO (Id NSArray)
printerNames  =
  do
    cls' <- getRequiredClass "NSPrinter"
    sendClassMsg cls' (mkSelector "printerNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ printerTypes@
printerTypes :: IO (Id NSArray)
printerTypes  =
  do
    cls' <- getRequiredClass "NSPrinter"
    sendClassMsg cls' (mkSelector "printerTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- name@
name :: IsNSPrinter nsPrinter => nsPrinter -> IO (Id NSString)
name nsPrinter  =
  sendMsg nsPrinter (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- type@
type_ :: IsNSPrinter nsPrinter => nsPrinter -> IO (Id NSString)
type_ nsPrinter  =
  sendMsg nsPrinter (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- languageLevel@
languageLevel :: IsNSPrinter nsPrinter => nsPrinter -> IO CLong
languageLevel nsPrinter  =
  sendMsg nsPrinter (mkSelector "languageLevel") retCLong []

-- | @- deviceDescription@
deviceDescription :: IsNSPrinter nsPrinter => nsPrinter -> IO (Id NSDictionary)
deviceDescription nsPrinter  =
  sendMsg nsPrinter (mkSelector "deviceDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @printerWithName:@
printerWithNameSelector :: Selector
printerWithNameSelector = mkSelector "printerWithName:"

-- | @Selector@ for @printerWithType:@
printerWithTypeSelector :: Selector
printerWithTypeSelector = mkSelector "printerWithType:"

-- | @Selector@ for @pageSizeForPaper:@
pageSizeForPaperSelector :: Selector
pageSizeForPaperSelector = mkSelector "pageSizeForPaper:"

-- | @Selector@ for @statusForTable:@
statusForTableSelector :: Selector
statusForTableSelector = mkSelector "statusForTable:"

-- | @Selector@ for @isKey:inTable:@
isKey_inTableSelector :: Selector
isKey_inTableSelector = mkSelector "isKey:inTable:"

-- | @Selector@ for @booleanForKey:inTable:@
booleanForKey_inTableSelector :: Selector
booleanForKey_inTableSelector = mkSelector "booleanForKey:inTable:"

-- | @Selector@ for @floatForKey:inTable:@
floatForKey_inTableSelector :: Selector
floatForKey_inTableSelector = mkSelector "floatForKey:inTable:"

-- | @Selector@ for @intForKey:inTable:@
intForKey_inTableSelector :: Selector
intForKey_inTableSelector = mkSelector "intForKey:inTable:"

-- | @Selector@ for @rectForKey:inTable:@
rectForKey_inTableSelector :: Selector
rectForKey_inTableSelector = mkSelector "rectForKey:inTable:"

-- | @Selector@ for @sizeForKey:inTable:@
sizeForKey_inTableSelector :: Selector
sizeForKey_inTableSelector = mkSelector "sizeForKey:inTable:"

-- | @Selector@ for @stringForKey:inTable:@
stringForKey_inTableSelector :: Selector
stringForKey_inTableSelector = mkSelector "stringForKey:inTable:"

-- | @Selector@ for @stringListForKey:inTable:@
stringListForKey_inTableSelector :: Selector
stringListForKey_inTableSelector = mkSelector "stringListForKey:inTable:"

-- | @Selector@ for @imageRectForPaper:@
imageRectForPaperSelector :: Selector
imageRectForPaperSelector = mkSelector "imageRectForPaper:"

-- | @Selector@ for @acceptsBinary@
acceptsBinarySelector :: Selector
acceptsBinarySelector = mkSelector "acceptsBinary"

-- | @Selector@ for @isColor@
isColorSelector :: Selector
isColorSelector = mkSelector "isColor"

-- | @Selector@ for @isFontAvailable:@
isFontAvailableSelector :: Selector
isFontAvailableSelector = mkSelector "isFontAvailable:"

-- | @Selector@ for @isOutputStackInReverseOrder@
isOutputStackInReverseOrderSelector :: Selector
isOutputStackInReverseOrderSelector = mkSelector "isOutputStackInReverseOrder"

-- | @Selector@ for @printerWithName:domain:includeUnavailable:@
printerWithName_domain_includeUnavailableSelector :: Selector
printerWithName_domain_includeUnavailableSelector = mkSelector "printerWithName:domain:includeUnavailable:"

-- | @Selector@ for @domain@
domainSelector :: Selector
domainSelector = mkSelector "domain"

-- | @Selector@ for @host@
hostSelector :: Selector
hostSelector = mkSelector "host"

-- | @Selector@ for @note@
noteSelector :: Selector
noteSelector = mkSelector "note"

-- | @Selector@ for @printerNames@
printerNamesSelector :: Selector
printerNamesSelector = mkSelector "printerNames"

-- | @Selector@ for @printerTypes@
printerTypesSelector :: Selector
printerTypesSelector = mkSelector "printerTypes"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @languageLevel@
languageLevelSelector :: Selector
languageLevelSelector = mkSelector "languageLevel"

-- | @Selector@ for @deviceDescription@
deviceDescriptionSelector :: Selector
deviceDescriptionSelector = mkSelector "deviceDescription"

