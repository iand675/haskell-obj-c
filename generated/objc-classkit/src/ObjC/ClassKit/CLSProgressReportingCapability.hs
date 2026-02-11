{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This class specifies progress reporting capability of a ClassKit client app
--
-- Generated bindings for @CLSProgressReportingCapability@.
module ObjC.ClassKit.CLSProgressReportingCapability
  ( CLSProgressReportingCapability
  , IsCLSProgressReportingCapability(..)
  , initWithKind_details
  , kind
  , details
  , initWithKind_detailsSelector
  , kindSelector
  , detailsSelector

  -- * Enum types
  , CLSProgressReportingCapabilityKind(CLSProgressReportingCapabilityKind)
  , pattern CLSProgressReportingCapabilityKindDuration
  , pattern CLSProgressReportingCapabilityKindPercent
  , pattern CLSProgressReportingCapabilityKindBinary
  , pattern CLSProgressReportingCapabilityKindQuantity
  , pattern CLSProgressReportingCapabilityKindScore

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

import ObjC.ClassKit.Internal.Classes
import ObjC.ClassKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Initialize and configure the type of progress reporting capability
--
-- @kind@ — The kind of progress reporting capability
--
-- @details@ — An optional localized string describing the capability. For example: "Reports percentage of progress", "Reports overall score". Schoolwork will use an appropriate default string if one is not provided.
--
-- ObjC selector: @- initWithKind:details:@
initWithKind_details :: (IsCLSProgressReportingCapability clsProgressReportingCapability, IsNSString details) => clsProgressReportingCapability -> CLSProgressReportingCapabilityKind -> details -> IO (Id CLSProgressReportingCapability)
initWithKind_details clsProgressReportingCapability  kind details =
withObjCPtr details $ \raw_details ->
    sendMsg clsProgressReportingCapability (mkSelector "initWithKind:details:") (retPtr retVoid) [argCLong (coerce kind), argPtr (castPtr raw_details :: Ptr ())] >>= ownedObject . castPtr

-- | Returns the kind of progress reporting capability
--
-- ObjC selector: @- kind@
kind :: IsCLSProgressReportingCapability clsProgressReportingCapability => clsProgressReportingCapability -> IO CLSProgressReportingCapabilityKind
kind clsProgressReportingCapability  =
  fmap (coerce :: CLong -> CLSProgressReportingCapabilityKind) $ sendMsg clsProgressReportingCapability (mkSelector "kind") retCLong []

-- | Returns progress reporting details
--
-- ObjC selector: @- details@
details :: IsCLSProgressReportingCapability clsProgressReportingCapability => clsProgressReportingCapability -> IO (Id NSString)
details clsProgressReportingCapability  =
  sendMsg clsProgressReportingCapability (mkSelector "details") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithKind:details:@
initWithKind_detailsSelector :: Selector
initWithKind_detailsSelector = mkSelector "initWithKind:details:"

-- | @Selector@ for @kind@
kindSelector :: Selector
kindSelector = mkSelector "kind"

-- | @Selector@ for @details@
detailsSelector :: Selector
detailsSelector = mkSelector "details"

