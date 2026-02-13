{-# LANGUAGE DataKinds #-}
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
  , itemFormatterSelector
  , localeSelector
  , localizedStringByJoiningStringsSelector
  , setItemFormatterSelector
  , setLocaleSelector
  , stringForObjectValueSelector
  , stringFromItemsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ localizedStringByJoiningStrings:@
localizedStringByJoiningStrings :: IsNSArray strings => strings -> IO (Id NSString)
localizedStringByJoiningStrings strings =
  do
    cls' <- getRequiredClass "NSListFormatter"
    sendClassMessage cls' localizedStringByJoiningStringsSelector (toNSArray strings)

-- | @- stringFromItems:@
stringFromItems :: (IsNSListFormatter nsListFormatter, IsNSArray items) => nsListFormatter -> items -> IO (Id NSString)
stringFromItems nsListFormatter items =
  sendMessage nsListFormatter stringFromItemsSelector (toNSArray items)

-- | @- stringForObjectValue:@
stringForObjectValue :: IsNSListFormatter nsListFormatter => nsListFormatter -> RawId -> IO (Id NSString)
stringForObjectValue nsListFormatter obj_ =
  sendMessage nsListFormatter stringForObjectValueSelector obj_

-- | @- locale@
locale :: IsNSListFormatter nsListFormatter => nsListFormatter -> IO (Id NSLocale)
locale nsListFormatter =
  sendMessage nsListFormatter localeSelector

-- | @- setLocale:@
setLocale :: (IsNSListFormatter nsListFormatter, IsNSLocale value) => nsListFormatter -> value -> IO ()
setLocale nsListFormatter value =
  sendMessage nsListFormatter setLocaleSelector (toNSLocale value)

-- | @- itemFormatter@
itemFormatter :: IsNSListFormatter nsListFormatter => nsListFormatter -> IO (Id NSFormatter)
itemFormatter nsListFormatter =
  sendMessage nsListFormatter itemFormatterSelector

-- | @- setItemFormatter:@
setItemFormatter :: (IsNSListFormatter nsListFormatter, IsNSFormatter value) => nsListFormatter -> value -> IO ()
setItemFormatter nsListFormatter value =
  sendMessage nsListFormatter setItemFormatterSelector (toNSFormatter value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @localizedStringByJoiningStrings:@
localizedStringByJoiningStringsSelector :: Selector '[Id NSArray] (Id NSString)
localizedStringByJoiningStringsSelector = mkSelector "localizedStringByJoiningStrings:"

-- | @Selector@ for @stringFromItems:@
stringFromItemsSelector :: Selector '[Id NSArray] (Id NSString)
stringFromItemsSelector = mkSelector "stringFromItems:"

-- | @Selector@ for @stringForObjectValue:@
stringForObjectValueSelector :: Selector '[RawId] (Id NSString)
stringForObjectValueSelector = mkSelector "stringForObjectValue:"

-- | @Selector@ for @locale@
localeSelector :: Selector '[] (Id NSLocale)
localeSelector = mkSelector "locale"

-- | @Selector@ for @setLocale:@
setLocaleSelector :: Selector '[Id NSLocale] ()
setLocaleSelector = mkSelector "setLocale:"

-- | @Selector@ for @itemFormatter@
itemFormatterSelector :: Selector '[] (Id NSFormatter)
itemFormatterSelector = mkSelector "itemFormatter"

-- | @Selector@ for @setItemFormatter:@
setItemFormatterSelector :: Selector '[Id NSFormatter] ()
setItemFormatterSelector = mkSelector "setItemFormatter:"

