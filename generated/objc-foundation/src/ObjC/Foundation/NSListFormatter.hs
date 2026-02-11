{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSListFormatter@.
module ObjC.Foundation.NSListFormatter
  ( NSListFormatter
  , IsNSListFormatter(..)
  , localizedStringByJoiningStrings
  , stringFromItems
  , stringForObjectValue
  , locale
  , setLocale
  , itemFormatter
  , setItemFormatter
  , localizedStringByJoiningStringsSelector
  , stringFromItemsSelector
  , stringForObjectValueSelector
  , localeSelector
  , setLocaleSelector
  , itemFormatterSelector
  , setItemFormatterSelector


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

import ObjC.Foundation.Internal.Classes

-- | @+ localizedStringByJoiningStrings:@
localizedStringByJoiningStrings :: IsNSArray strings => strings -> IO (Id NSString)
localizedStringByJoiningStrings strings =
  do
    cls' <- getRequiredClass "NSListFormatter"
    withObjCPtr strings $ \raw_strings ->
      sendClassMsg cls' (mkSelector "localizedStringByJoiningStrings:") (retPtr retVoid) [argPtr (castPtr raw_strings :: Ptr ())] >>= retainedObject . castPtr

-- | @- stringFromItems:@
stringFromItems :: (IsNSListFormatter nsListFormatter, IsNSArray items) => nsListFormatter -> items -> IO (Id NSString)
stringFromItems nsListFormatter  items =
withObjCPtr items $ \raw_items ->
    sendMsg nsListFormatter (mkSelector "stringFromItems:") (retPtr retVoid) [argPtr (castPtr raw_items :: Ptr ())] >>= retainedObject . castPtr

-- | @- stringForObjectValue:@
stringForObjectValue :: IsNSListFormatter nsListFormatter => nsListFormatter -> RawId -> IO (Id NSString)
stringForObjectValue nsListFormatter  obj_ =
  sendMsg nsListFormatter (mkSelector "stringForObjectValue:") (retPtr retVoid) [argPtr (castPtr (unRawId obj_) :: Ptr ())] >>= retainedObject . castPtr

-- | @- locale@
locale :: IsNSListFormatter nsListFormatter => nsListFormatter -> IO (Id NSLocale)
locale nsListFormatter  =
  sendMsg nsListFormatter (mkSelector "locale") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocale:@
setLocale :: (IsNSListFormatter nsListFormatter, IsNSLocale value) => nsListFormatter -> value -> IO ()
setLocale nsListFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsListFormatter (mkSelector "setLocale:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- itemFormatter@
itemFormatter :: IsNSListFormatter nsListFormatter => nsListFormatter -> IO (Id NSFormatter)
itemFormatter nsListFormatter  =
  sendMsg nsListFormatter (mkSelector "itemFormatter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setItemFormatter:@
setItemFormatter :: (IsNSListFormatter nsListFormatter, IsNSFormatter value) => nsListFormatter -> value -> IO ()
setItemFormatter nsListFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsListFormatter (mkSelector "setItemFormatter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @localizedStringByJoiningStrings:@
localizedStringByJoiningStringsSelector :: Selector
localizedStringByJoiningStringsSelector = mkSelector "localizedStringByJoiningStrings:"

-- | @Selector@ for @stringFromItems:@
stringFromItemsSelector :: Selector
stringFromItemsSelector = mkSelector "stringFromItems:"

-- | @Selector@ for @stringForObjectValue:@
stringForObjectValueSelector :: Selector
stringForObjectValueSelector = mkSelector "stringForObjectValue:"

-- | @Selector@ for @locale@
localeSelector :: Selector
localeSelector = mkSelector "locale"

-- | @Selector@ for @setLocale:@
setLocaleSelector :: Selector
setLocaleSelector = mkSelector "setLocale:"

-- | @Selector@ for @itemFormatter@
itemFormatterSelector :: Selector
itemFormatterSelector = mkSelector "itemFormatter"

-- | @Selector@ for @setItemFormatter:@
setItemFormatterSelector :: Selector
setItemFormatterSelector = mkSelector "setItemFormatter:"

