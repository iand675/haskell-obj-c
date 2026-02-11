{-# LANGUAGE PatternSynonyms #-}
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
  , columnTerminatorsForLocaleSelector
  , initWithType_locationSelector
  , initWithTextAlignment_location_optionsSelector
  , locationSelector
  , optionsSelector
  , tabStopTypeSelector
  , alignmentSelector

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

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ columnTerminatorsForLocale:@
columnTerminatorsForLocale :: IsNSLocale aLocale => aLocale -> IO (Id NSCharacterSet)
columnTerminatorsForLocale aLocale =
  do
    cls' <- getRequiredClass "NSTextTab"
    withObjCPtr aLocale $ \raw_aLocale ->
      sendClassMsg cls' (mkSelector "columnTerminatorsForLocale:") (retPtr retVoid) [argPtr (castPtr raw_aLocale :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithType:location:@
initWithType_location :: IsNSTextTab nsTextTab => nsTextTab -> NSTextTabType -> CDouble -> IO (Id NSTextTab)
initWithType_location nsTextTab  type_ loc =
  sendMsg nsTextTab (mkSelector "initWithType:location:") (retPtr retVoid) [argCULong (coerce type_), argCDouble (fromIntegral loc)] >>= ownedObject . castPtr

-- | @- initWithTextAlignment:location:options:@
initWithTextAlignment_location_options :: (IsNSTextTab nsTextTab, IsNSDictionary options) => nsTextTab -> NSTextAlignment -> CDouble -> options -> IO (Id NSTextTab)
initWithTextAlignment_location_options nsTextTab  alignment loc options =
withObjCPtr options $ \raw_options ->
    sendMsg nsTextTab (mkSelector "initWithTextAlignment:location:options:") (retPtr retVoid) [argCLong (coerce alignment), argCDouble (fromIntegral loc), argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

-- | @- location@
location :: IsNSTextTab nsTextTab => nsTextTab -> IO CDouble
location nsTextTab  =
  sendMsg nsTextTab (mkSelector "location") retCDouble []

-- | @- options@
options :: IsNSTextTab nsTextTab => nsTextTab -> IO (Id NSDictionary)
options nsTextTab  =
  sendMsg nsTextTab (mkSelector "options") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- tabStopType@
tabStopType :: IsNSTextTab nsTextTab => nsTextTab -> IO NSTextTabType
tabStopType nsTextTab  =
  fmap (coerce :: CULong -> NSTextTabType) $ sendMsg nsTextTab (mkSelector "tabStopType") retCULong []

-- | @- alignment@
alignment :: IsNSTextTab nsTextTab => nsTextTab -> IO NSTextAlignment
alignment nsTextTab  =
  fmap (coerce :: CLong -> NSTextAlignment) $ sendMsg nsTextTab (mkSelector "alignment") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @columnTerminatorsForLocale:@
columnTerminatorsForLocaleSelector :: Selector
columnTerminatorsForLocaleSelector = mkSelector "columnTerminatorsForLocale:"

-- | @Selector@ for @initWithType:location:@
initWithType_locationSelector :: Selector
initWithType_locationSelector = mkSelector "initWithType:location:"

-- | @Selector@ for @initWithTextAlignment:location:options:@
initWithTextAlignment_location_optionsSelector :: Selector
initWithTextAlignment_location_optionsSelector = mkSelector "initWithTextAlignment:location:options:"

-- | @Selector@ for @location@
locationSelector :: Selector
locationSelector = mkSelector "location"

-- | @Selector@ for @options@
optionsSelector :: Selector
optionsSelector = mkSelector "options"

-- | @Selector@ for @tabStopType@
tabStopTypeSelector :: Selector
tabStopTypeSelector = mkSelector "tabStopType"

-- | @Selector@ for @alignment@
alignmentSelector :: Selector
alignmentSelector = mkSelector "alignment"

