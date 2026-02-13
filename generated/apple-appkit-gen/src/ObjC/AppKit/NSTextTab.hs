{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTextTab@.
module ObjC.AppKit.NSTextTab
  ( NSTextTab
  , IsNSTextTab(..)
  , columnTerminatorsForLocale
  , initWithType_location
  , initWithTextAlignment_location_options
  , location
  , options
  , tabStopType
  , alignment
  , alignmentSelector
  , columnTerminatorsForLocaleSelector
  , initWithTextAlignment_location_optionsSelector
  , initWithType_locationSelector
  , locationSelector
  , optionsSelector
  , tabStopTypeSelector

  -- * Enum types
  , NSTextAlignment(NSTextAlignment)
  , pattern NSTextAlignmentLeft
  , pattern NSTextAlignmentCenter
  , pattern NSTextAlignmentRight
  , pattern NSTextAlignmentJustified
  , pattern NSTextAlignmentNatural
  , NSTextTabType(NSTextTabType)
  , pattern NSLeftTabStopType
  , pattern NSRightTabStopType
  , pattern NSCenterTabStopType
  , pattern NSDecimalTabStopType

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ columnTerminatorsForLocale:@
columnTerminatorsForLocale :: IsNSLocale aLocale => aLocale -> IO (Id NSCharacterSet)
columnTerminatorsForLocale aLocale =
  do
    cls' <- getRequiredClass "NSTextTab"
    sendClassMessage cls' columnTerminatorsForLocaleSelector (toNSLocale aLocale)

-- | @- initWithType:location:@
initWithType_location :: IsNSTextTab nsTextTab => nsTextTab -> NSTextTabType -> CDouble -> IO (Id NSTextTab)
initWithType_location nsTextTab type_ loc =
  sendOwnedMessage nsTextTab initWithType_locationSelector type_ loc

-- | @- initWithTextAlignment:location:options:@
initWithTextAlignment_location_options :: (IsNSTextTab nsTextTab, IsNSDictionary options) => nsTextTab -> NSTextAlignment -> CDouble -> options -> IO (Id NSTextTab)
initWithTextAlignment_location_options nsTextTab alignment loc options =
  sendOwnedMessage nsTextTab initWithTextAlignment_location_optionsSelector alignment loc (toNSDictionary options)

-- | @- location@
location :: IsNSTextTab nsTextTab => nsTextTab -> IO CDouble
location nsTextTab =
  sendMessage nsTextTab locationSelector

-- | @- options@
options :: IsNSTextTab nsTextTab => nsTextTab -> IO (Id NSDictionary)
options nsTextTab =
  sendMessage nsTextTab optionsSelector

-- | @- tabStopType@
tabStopType :: IsNSTextTab nsTextTab => nsTextTab -> IO NSTextTabType
tabStopType nsTextTab =
  sendMessage nsTextTab tabStopTypeSelector

-- | @- alignment@
alignment :: IsNSTextTab nsTextTab => nsTextTab -> IO NSTextAlignment
alignment nsTextTab =
  sendMessage nsTextTab alignmentSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @columnTerminatorsForLocale:@
columnTerminatorsForLocaleSelector :: Selector '[Id NSLocale] (Id NSCharacterSet)
columnTerminatorsForLocaleSelector = mkSelector "columnTerminatorsForLocale:"

-- | @Selector@ for @initWithType:location:@
initWithType_locationSelector :: Selector '[NSTextTabType, CDouble] (Id NSTextTab)
initWithType_locationSelector = mkSelector "initWithType:location:"

-- | @Selector@ for @initWithTextAlignment:location:options:@
initWithTextAlignment_location_optionsSelector :: Selector '[NSTextAlignment, CDouble, Id NSDictionary] (Id NSTextTab)
initWithTextAlignment_location_optionsSelector = mkSelector "initWithTextAlignment:location:options:"

-- | @Selector@ for @location@
locationSelector :: Selector '[] CDouble
locationSelector = mkSelector "location"

-- | @Selector@ for @options@
optionsSelector :: Selector '[] (Id NSDictionary)
optionsSelector = mkSelector "options"

-- | @Selector@ for @tabStopType@
tabStopTypeSelector :: Selector '[] NSTextTabType
tabStopTypeSelector = mkSelector "tabStopType"

-- | @Selector@ for @alignment@
alignmentSelector :: Selector '[] NSTextAlignment
alignmentSelector = mkSelector "alignment"

